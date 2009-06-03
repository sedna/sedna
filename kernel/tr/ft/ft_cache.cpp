/*
 * File:  ft_cache.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/ft_cache.h"
#include "tr/ft/string_map.h"
#include "tr/ft/ft_index_data.h"
#include "tr/idx/btree/btree.h"
#include <inttypes.h>

//XXX: assumes that deref-ed FTC_PTR's are valid until cache is flushed.
//FIXME: all structs here will have aliasing problems.
#ifdef FTC_ALLOCATOR_IS_MALLOC_ALLOCATOR
MallocAllocator m_alloc;
#else
#error bad FTC_ALLOCATOR
#endif

FTC_PTR ftc_indexes = FTC_NULL;
struct ftc_doc_data
{
	xptr acc;
	char str_id[24];
};
enum wo_type {
	wo_add, wo_del
};
struct ftc_word_occur
{
	FTC_PTR pred;
	FTC_PTR next;
	int ind;
	wo_type type;
};
struct ftc_word_data
{
	FTC_PTR occur_map;
};
struct ftc_index_data;
void index_oom_flush(ftc_index_data *id);
struct ftc_index_data
{
	FTC_PTR docs;
	FTC_PTR words;
	xptr btree_root; //FIXME: this will need to change
	sorted_sequence *ss;
	FTC_ALLOCATOR ind_alloc;
	char name[];

	//need ft_index_sem
	static FTC_PTR create(const char *name, xptr btree_root)
	{
		FTC_PTR ptr = m_alloc.alloc(sizeof(ftc_index_data) + strlen(name) + 1); //TODO: check null
		ftc_index_data *id = ((ftc_index_data*)m_alloc.deref(ptr));
		new (&id->ind_alloc) FTC_ALLOCATOR(); //FIXME
		id->reset();
		strcpy(id->name, name);
		id->btree_root = btree_root;
		id->ss = NULL;
		return ptr;
	}
	void reset()
	{
		docs = FTC_NULL;
		words = FTC_WORDMAP::init(&ind_alloc); //TODO: check null!
	}
	static inline ftc_index_data *get(ftc_index_t idx)
	{
		return (ftc_index_data *)m_alloc.deref(idx);
	}
	ftc_doc_t create_doc(const xptr &acc)
	{
		FTC_PTR ptr = ind_alloc.alloc(sizeof(ftc_doc_data));
		if (ptr == ind_alloc.null_ptr())
			return ptr;
		ftc_doc_data * doc = (ftc_doc_data*)ind_alloc.deref(ptr);
		doc->acc = acc;
		sprintf(doc->str_id, "%020" PRIx64, *(uint64_t*)&acc);
		return ptr;
	}
	ftc_doc_data * get_doc(ftc_doc_t ptr)
	{
		return (ftc_doc_data*)ind_alloc.deref(ptr);
	}
	bool add_occur(ftc_word_data *wd, ftc_doc_t doc, int word_ind, wo_type wot)
	{
		ftc_doc_data *doc_data = get_doc(doc);
		if (wd->occur_map == FTC_ALLOCATOR::null_ptr())
		{
			wd->occur_map = FTC_OCCURMAP::init(&ind_alloc);
			if (wd->occur_map == ind_alloc.null_ptr())
				return false;
		}
		FTC_OCCURMAP *om = FTC_OCCURMAP::get_map(wd->occur_map, ind_alloc);
		ftc_occur_data *od= om->find(doc_data->str_id);

		FTC_PTR new_occur_ptr = ind_alloc.alloc(sizeof(ftc_word_occur));
		if (new_occur_ptr == ind_alloc.null_ptr())
			return false;
		ftc_word_occur *new_occur = (ftc_word_occur *)ind_alloc.deref(new_occur_ptr);

		new_occur->ind = word_ind;
		new_occur->next = ind_alloc.null_ptr();

		if (od == NULL)
		{
			FTC_OCCURMAP::pers_sset_entry *e = om->put(doc_data->str_id, ftc_occur_data());
			if (e == NULL)
				return false;
			od = &e->obj;
			od->first = od->last = new_occur_ptr;
			new_occur->pred = ind_alloc.null_ptr();
			od->doc = doc;
		}
		else
		{
			FTC_PTR pred_ptr = od->last;
			ftc_word_occur *pred_occur = (ftc_word_occur *)ind_alloc.deref(pred_ptr);

			od->last = new_occur_ptr;
			new_occur->pred = pred_ptr;
			pred_occur->next = new_occur_ptr;
		}
		return true;
	}
};


//pre: ft_index_sem must be accuired
ftc_index_t ftc_get_index(const char *name, xptr btree_root)
{
	if (ftc_indexes == FTC_NULL)
		ftc_indexes = FTC_MAP::init(&m_alloc); //TODO: check null!

	FTC_MAP *m = FTC_MAP::get_map(ftc_indexes, m_alloc);
	FTC_PTR *e = m->find(name);
	if (e == NULL)
	{
		FTC_PTR data = ftc_index_data::create(name, btree_root);
		m->put(name, data);
		return data;
	}
	return *e;
}


ftc_doc_t ftc_add_new_doc(ftc_index_t idx, xptr acc)
{
	//FIXME: this may (unlikely) cause double flush (if called after another flush)
	//FIXME: check if it exists
	ftc_index_data *id = ftc_index_data::get(idx);
	ftc_doc_t res = id->create_doc(acc);
	if (res == id->ind_alloc.null_ptr())
	{
		index_oom_flush(id);
		res = id->create_doc(acc);
		if (res == id->ind_alloc.null_ptr())
			throw SYSTEM_EXCEPTION("ftc_add_new_doc: failed to create doc");
	}
	return res;
}

#include "common/errdbg/d_printf.h"
//needs ft_index_sem unless id->ss is set
//FIXME: does it really need ft_index_sem??
void flush_index(ftc_index_data *id)
{
	FTC_WORDMAP *wm = FTC_WORDMAP::get_map(id->words, id->ind_alloc);
	FTC_WORDMAP::pers_sset_entry *wme = wm->rb_minimum(FTC_WORDMAP::get_entry(wm->root, id->ind_alloc));
	bt_key bkey;
	d_printf1("ftc_flush start\n");
	while (wme != NULL)
	{
		ftc_word_data *wd = &wme->obj;
		if (wd->occur_map != id->ind_alloc.null_ptr()) //may be null iff occur_map alloc failed; FIXME: check in debug mode?
		{
			FTC_OCCURMAP *om = FTC_OCCURMAP::get_map(wd->occur_map, id->ind_alloc);
			FTC_OCCURMAP::pers_sset_entry *ome = om->rb_minimum(FTC_OCCURMAP::get_entry(om->root, id->ind_alloc));
			//FTC_OCCURMAP::pers_sset_entry *ome = om->rb_maximum(FTC_OCCURMAP::get_entry(om->root, id->ind_alloc));
			while (ome != NULL)
			{
				ftc_doc_data *doc_data = id->get_doc(ome->obj.doc);
				ftc_word_occur *wo = (ftc_word_occur *)id->ind_alloc.deref(ome->obj.first);
				while (wo != NULL)
				{
					bkey.setnew(wme->str);
					if (!id->ss)
						bt_insert_tmpl<ft_idx_btree_element>(id->btree_root, bkey, ft_idx_btree_element(doc_data->acc, wo->ind));
					else
					{
						throw SYSTEM_EXCEPTION("ftc: non empty cahce and ss!=NULL");
					}

					wo = (ftc_word_occur *)id->ind_alloc.deref(wo->next);
				}
				ome = om->rb_successor(ome);
				//ome = om->rb_predecessor(ome);
			}
		}

		wme = wm->rb_successor(wme);
	}
	d_printf1("ftc_flush end\n");
	if (!id->ss)
	{
		ft_index_cell_cptr idc = find_ft_index(id->name, NULL);
		idc->ft_data.btree_root = id->btree_root;
	}

	id->ind_alloc.release();
}

void index_oom_flush(ftc_index_data *id)
{
	flush_index(id);
	id->reset();
}

void ftc_flush()
{
	if (ftc_indexes == FTC_NULL)
		return;
	FTC_MAP *m = FTC_MAP::get_map(ftc_indexes, m_alloc);
	FTC_MAP::pers_sset_entry *e = m->rb_minimum(FTC_MAP::get_entry(m->root, m_alloc));
	while (e != NULL)
	{
		flush_index(ftc_index_data::get(e->obj));
		e = m->rb_successor(e);
	}
}

void ftc_add_word(ftc_index_t index, ftc_doc_t &ft_doc, const char *word, int word_ind, bool no_flush)
{
	ftc_index_data *id = ftc_index_data::get(index);

	if (id->ss != NULL)
	{
		tuple t(3);
		ftc_doc_data *dd = id->get_doc(ft_doc);

		t.eos = false;
		t.cells[0] = tuple_cell::atomic_deep(xs_string, word);
		t.cells[1] = tuple_cell::node(dd->acc);
		t.cells[2] = tuple_cell::atomic((int64_t)word_ind);

		id->ss->add(t);
		return;
	}

	FTC_WORDMAP *wm = FTC_WORDMAP::get_map(id->words, id->ind_alloc);
	ftc_word_data *wd = wm->find(word);
	if (wd == NULL)
		wd = &wm->put(word, ftc_word_data())->obj; //TODO: check null!

	if (!id->add_occur(wd, ft_doc, word_ind, wo_add))
	{
		if (no_flush)
			throw SYSTEM_EXCEPTION("ftc: add_occur failed");
		ftc_doc_data *dd = id->get_doc(ft_doc);
		xptr acc = dd->acc;
		index_oom_flush(id);
		ft_doc = ftc_add_new_doc(index, acc);
		ftc_add_word(index, ft_doc, word, word_ind, true);
	}
}


void ftc_scan_result::scan_word(const char *word)
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);
	U_ASSERT(!id->ss);
	FTC_WORDMAP *wm = FTC_WORDMAP::get_map(id->words, id->ind_alloc);
	ftc_word_data *wd = wm->find(word);
	
	if (wd != NULL)
	{
		om = FTC_OCCURMAP::get_map(wd->occur_map, id->ind_alloc);
		ome = om->rb_minimum(FTC_OCCURMAP::get_entry(om->root, id->ind_alloc));
	}
	else
	{
		om = NULL;
		ome = NULL;
	}


	bt_key bkey;
	bkey.setnew(word);
	bcur = bt_find_tmpl<ft_idx_btree_element>(id->btree_root, bkey);
	ce = bcur.bt_next_obj();
}

void ftc_scan_result::get_next_result(tuple &t)
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);
	if (ome == NULL)
	{
		if (ce.node == NULL)
		{
			t.set_eos();
			return;
		}
		xptr p = ce.node;
		t.copy(tuple_cell::node(removeIndirection(p)));
		while (ce.node == p)
			ce = bcur.bt_next_obj();
	}
	else
	{
		ftc_doc_data *doc_data = id->get_doc(ome->obj.doc);

		if (ce.node == NULL)
		{
			t.copy(tuple_cell::node(removeIndirection(doc_data->acc)));
			ome = om->rb_successor(ome);
			return;
		}

		if (ce.node == doc_data->acc)
		{
			//TODO, impossible now
		}
		else if (ce.node < doc_data->acc)
		{
			xptr p = ce.node;
			t.copy(tuple_cell::node(removeIndirection(p)));
			while (ce.node == p)
				ce = bcur.bt_next_obj();
		}
		else //(ce.node > doc_data->acc)
		{
			t.copy(tuple_cell::node(removeIndirection(doc_data->acc)));
			ome = om->rb_successor(ome);
		}
	}
}

void ftc_set_ss(ftc_index_t idx, sorted_sequence *ss)
{
	ftc_index_data *id = ftc_index_data::get(idx);
	U_ASSERT(ss == NULL || id->ss == NULL);

	if (id->ss)
	{
		flush_index(id);
		id->reset();
	}

	id->ss = ss;
}
