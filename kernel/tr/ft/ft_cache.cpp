/*
 * File:  ft_cache.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/ft_cache.h"
#include "tr/ft/string_map.h"
#include "tr/ft/ft_index_data.h"
#include <inttypes.h>
#include "tr/mo/indirection.h"

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
	wo_add, wo_del, wo_null
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
	struct FtsData ft_data;
	FTC_ALLOCATOR ind_alloc;
#ifdef _MSC_VER
#pragma warning( disable : 4200 )
#endif
    char name[];
#ifdef _MSC_VER
#pragma warning( default : 4200 )
#endif

    //need ft_index_sem
	static FTC_PTR create(const char *name, const struct FtsData *ft_data)
	{
		FTC_PTR ptr = m_alloc.alloc(sizeof(ftc_index_data) + strlen(name) + 1); //TODO: check null
		ftc_index_data *id = ((ftc_index_data*)m_alloc.deref(ptr));
		new (&id->ind_alloc) FTC_ALLOCATOR(); //FIXME
		id->reset();
		strcpy(id->name, name);
		id->ft_data = *ft_data; //FIXME: add some explicit copy function
		return ptr;
	}
	void reset()
	{
		docs = FTC_DOCMAP::init(&ind_alloc); //TODO: check null!
		words = FTC_WORDMAP::init(&ind_alloc); //TODO: check null!
	}
	static inline ftc_index_data *get(ftc_index_t idx)
	{
		return (ftc_index_data *)m_alloc.deref(idx);
	}
	//add document to the cache, returns null if fails to allocate memory
	ftc_doc_t create_doc(const xptr &acc)
	{
		FTC_PTR ptr = ind_alloc.alloc(sizeof(ftc_doc_data));
		if (ptr == ind_alloc.null_ptr())
			return ptr;
		ftc_doc_data * doc = (ftc_doc_data*)ind_alloc.deref(ptr);
		doc->acc = acc;
		sprintf(doc->str_id, "%020" PRIx64, *(uint64_t*)&acc);

		FTC_DOCMAP *dm = FTC_DOCMAP::get_map(docs, ind_alloc);
		FTC_DOCMAP::pers_sset_entry *e = dm->put(doc->str_id, ptr);
		if (e == NULL)
			return ind_alloc.null_ptr();

		return ptr;
	}
	//returns null if document is not in the cache
	ftc_doc_t find_doc(const xptr &acc)
	{
		//FIXME!
		char str_id[25];
		sprintf(str_id, "%020" PRIx64, *(uint64_t*)&acc);
		FTC_DOCMAP *dm = FTC_DOCMAP::get_map(docs, ind_alloc);
		FTC_PTR *pptr = dm->find(str_id);
		if (pptr != NULL)
			return *pptr;
		else
			return ind_alloc.null_ptr();
	}
	inline bool doc_is_null(ftc_doc_t d)
	{
		return (d == ind_alloc.null_ptr());
	}
	ftc_doc_data * get_doc_data(ftc_doc_t ptr)
	{
		return (ftc_doc_data*)ind_alloc.deref(ptr);
	}
	bool upd_occur(ftc_word_data *wd, ftc_doc_t doc, int word_ind, wo_type wot)
	{
		ftc_doc_data *doc_data = get_doc_data(doc);
		if (wd->occur_map == FTC_ALLOCATOR::null_ptr())
		{
			wd->occur_map = FTC_OCCURMAP::init(&ind_alloc);
			if (wd->occur_map == ind_alloc.null_ptr())
				return false;
		}
		FTC_OCCURMAP *om = FTC_OCCURMAP::get_map(wd->occur_map, ind_alloc);
		ftc_occur_data *od= om->find(doc_data->str_id);

		//check if similar occur is already in the cache
		if (od != NULL)
		{
			FTC_PTR pred_ptr = od->cursor;
			ftc_word_occur *pred_occur = (ftc_word_occur *)ind_alloc.deref(pred_ptr);
			if (pred_occur->ind > word_ind)
			{
				//need to search from start;
				pred_ptr = od->first;
				pred_occur = (ftc_word_occur *)ind_alloc.deref(pred_ptr);

				if (pred_occur->ind > word_ind)
				{
					//new occur will become first
					FTC_PTR new_occur_ptr = ind_alloc.alloc(sizeof(ftc_word_occur));
					if (new_occur_ptr == ind_alloc.null_ptr())
						return false;
					ftc_word_occur *new_occur = (ftc_word_occur *)ind_alloc.deref(new_occur_ptr);
					new_occur->ind = word_ind;
					new_occur->type = wot;
					new_occur->next = pred_ptr;
					new_occur->pred = ind_alloc.null_ptr();
					pred_occur->pred = new_occur_ptr;

					od->first = new_occur_ptr;
					od->cursor = new_occur_ptr;
					return true;
				}
			}
			//search for place to insert/update occur
			while (true)
			{
				if (pred_occur->ind == word_ind)
				{
					//reuse deleted node
					od->cursor = pred_ptr;
					if (pred_occur->type == wo_null)
					{
						pred_occur->type = wot;
						return true;
					}
					if (pred_occur->type == wot)
						throw SYSTEM_EXCEPTION("ft_cache: double add/delete of word occur");
					//it's add+del or del+add, both are equiv. to doing nothing
					pred_occur->type = wo_null;
					return true;
				}
				U_ASSERT(pred_occur->ind < word_ind);
				if (pred_occur->next == ind_alloc.null_ptr())
				{
					//new occur will become the last one
					FTC_PTR new_occur_ptr = ind_alloc.alloc(sizeof(ftc_word_occur));
					if (new_occur_ptr == ind_alloc.null_ptr())
						return false;

					ftc_word_occur *new_occur = (ftc_word_occur *)ind_alloc.deref(new_occur_ptr);
					new_occur->ind = word_ind;
					new_occur->type = wot;
					new_occur->next = ind_alloc.null_ptr();
					new_occur->pred = pred_ptr;
					pred_occur->next = new_occur_ptr;
					od->cursor = new_occur_ptr;
					return true;
				}
				else
				{
					ftc_word_occur *next_occur = (ftc_word_occur *)ind_alloc.deref(pred_occur->next);
					if (next_occur->ind > word_ind)
					{
						//insert new occur between pred_occur and next_occur
						FTC_PTR new_occur_ptr = ind_alloc.alloc(sizeof(ftc_word_occur));
						if (new_occur_ptr == ind_alloc.null_ptr())
							return false;

						ftc_word_occur *new_occur = (ftc_word_occur *)ind_alloc.deref(new_occur_ptr);
						new_occur->ind = word_ind;
						new_occur->type = wot;
						new_occur->next = pred_occur->next;
						new_occur->pred = pred_ptr;
						pred_occur->next = new_occur_ptr;
						next_occur->pred = new_occur_ptr;
						od->cursor = new_occur_ptr;
						return true;
					}
					//next_occur->ind <= word_ind, iterate
					pred_ptr = pred_occur->next;
					pred_occur = next_occur;
				}
			}
		}
		else
		{
			FTC_PTR new_occur_ptr = ind_alloc.alloc(sizeof(ftc_word_occur));
			if (new_occur_ptr == ind_alloc.null_ptr())
				return false;
			ftc_word_occur *new_occur = (ftc_word_occur *)ind_alloc.deref(new_occur_ptr);

			new_occur->ind = word_ind;
			new_occur->type = wot;
			new_occur->next = ind_alloc.null_ptr();
			new_occur->pred = ind_alloc.null_ptr();

			FTC_OCCURMAP::pers_sset_entry *e = om->put(doc_data->str_id, ftc_occur_data());
			if (e == NULL)
				return false;
			od = &e->obj;
			od->first = od->cursor = new_occur_ptr;
			od->doc = doc;

			return true;
		}
		U_ASSERT(false);
		return false;
	}
};


//pre: ft_index_sem must be accuired
ftc_index_t ftc_get_index(const char *name, struct FtsData *fts_data)
{
	if (ftc_indexes == FTC_NULL)
		ftc_indexes = FTC_MAP::init(&m_alloc); //TODO: check null!

	FTC_MAP *m = FTC_MAP::get_map(ftc_indexes, m_alloc);
	FTC_PTR *e = m->find(name);
	if (e == NULL)
	{
		FTC_PTR data = ftc_index_data::create(name, fts_data);
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
	if (id->doc_is_null(res))
	{
		index_oom_flush(id);
		res = id->create_doc(acc);
		if (id->doc_is_null(res))
			throw SYSTEM_EXCEPTION("ftc_add_new_doc: failed to create doc");
	}
	return res;
}
ftc_doc_t ftc_get_doc(ftc_index_t idx, xptr acc)
{
	//FIXME: this may (unlikely) cause double flush (if called after another flush)
	ftc_index_data *id = ftc_index_data::get(idx);
	ftc_doc_t res = id->find_doc(acc);
	if (!id->doc_is_null(res))
		return res;
	res = id->create_doc(acc);
	if (id->doc_is_null(res))
	{
		index_oom_flush(id);
		res = id->create_doc(acc);
		if (id->doc_is_null(res))
			throw SYSTEM_EXCEPTION("ftc_get_doc: failed to create doc");
	}
	return res;
}

#include "common/errdbg/d_printf.h"
//needs ft_index_sem
//FIXME: does it really need ft_index_sem??
void flush_index(ftc_index_data *id)
{
	FTC_WORDMAP *wm = FTC_WORDMAP::get_map(id->words, id->ind_alloc);
	FTC_WORDMAP::pers_sset_entry *wme = wm->rb_minimum(FTC_WORDMAP::get_entry(wm->root, id->ind_alloc));
	d_printf1("ftc_flush start\n");
	id->ft_data.begin_update();
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
				ftc_doc_data *doc_data = id->get_doc_data(ome->obj.doc);
				ftc_word_occur *wo = (ftc_word_occur *)id->ind_alloc.deref(ome->obj.first);
				while (wo != NULL)
				{
					if (wo->type != wo_null)
					{
						switch (wo->type)
						{
						case wo_add:
							id->ft_data.add_word_occur(wme->str, doc_data->acc, wo->ind);
							break;
						case wo_del:
							id->ft_data.del_word_occur(wme->str, doc_data->acc, wo->ind);
							break;
						default:
							U_ASSERT(false);
						}
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

	ft_index_cell_cptr idc = find_ft_index(id->name, NULL);
	idc.modify();
	id->ft_data.end_update(&idc->fts_data);

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

//returns true on success, false if alloc failed
static bool ftc_upd_word_try(ftc_index_data *id, ftc_doc_t &ft_doc, const char *word, int word_ind, bool insert)
{
	FTC_WORDMAP *wm = FTC_WORDMAP::get_map(id->words, id->ind_alloc);
	ftc_word_data *wd = wm->find(word);
	if (wd == NULL)
	{
		FTC_WORDMAP::pers_sset_entry *e = wm->put(word, ftc_word_data());
		if (e == NULL)
			return false;
		wd = &e->obj;
	}

	if (!id->upd_occur(wd, ft_doc, word_ind, insert ? wo_add : wo_del))
	{
		return false;
	}
	return true;
}

void ftc_upd_word(ftc_index_t index, ftc_doc_t &ft_doc, const char *word, int word_ind, bool insert, bool no_flush)
{
	ftc_index_data *id = ftc_index_data::get(index);

	if (!ftc_upd_word_try(id, ft_doc, word, word_ind, insert))
	{
		ftc_doc_data *dd = id->get_doc_data(ft_doc);
		xptr acc = dd->acc;
		index_oom_flush(id);
		ft_doc = ftc_add_new_doc(index, acc);
		if (!ftc_upd_word_try(id, ft_doc, word, word_ind, insert))
			throw SYSTEM_EXCEPTION("ftc: add_occur failed");
	}
}


void ftc_scan_result::scan_word(const char *word)
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);

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

	fts_sd.init(&id->ft_data, word);
}

//scan occurs for wo_add entries, starting with cur_occur, returns true if some are found
static inline bool scan_occurs(ftc_index_data *id, ftc_word_occur *cur_occur)
{
	while (cur_occur != NULL)
	{
		if (cur_occur->type == wo_add)
			return true;
		cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(cur_occur->next);
	}
	return false;
}

//try to get next result, returns true if successful, false if this functions needs to be called again
bool ftc_scan_result::get_next_result_step(tuple &t)
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);
	if (ome == NULL)
	{
		if (fts_sd.at_end())
		{
			t.set_eos();
			return true;
		}
		xptr p = fts_sd.cur_node();
		t.copy(tuple_cell::node(indirectionDereferenceCP(p)));
		fts_sd.skip_node(p);
		return true;
	}
	else
	{
		ftc_doc_data *doc_data = id->get_doc_data(ome->obj.doc);

		if (fts_sd.at_end())
		{
			//check that there is at least one wo_add occur
			ftc_word_occur *cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(ome->obj.first);
			if (scan_occurs(id, cur_occur))
			{
				t.copy(tuple_cell::node(indirectionDereferenceCP(doc_data->acc)));
				ome = om->rb_successor(ome);
				return true;
			}
			else
			{
				//FIXME: assert that there are only wo_null occurs in ome
				ome = om->rb_successor(ome);
				return false;
			}
		}

		if (fts_sd.cur_node() == doc_data->acc)
		{
			ftc_word_occur *cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(ome->obj.first);

			while (true)
			{
				if (fts_sd.cur_node() == doc_data->acc)
				{
					if (cur_occur == NULL)
					{
						xptr p = fts_sd.cur_node();
						t.copy(tuple_cell::node(indirectionDereferenceCP(p)));
						fts_sd.skip_node(p);
						return true;
					}
					if (cur_occur->ind < fts_sd.cur_word_ind())
					{
						if (cur_occur->type == wo_add)
						{
							//first word ind is in the cache
							t.copy(tuple_cell::node(indirectionDereferenceCP(doc_data->acc)));
							fts_sd.skip_node(doc_data->acc);
							ome = om->rb_successor(ome);

							return true;
						}
						//skip this occur in chache and try again
						U_ASSERT(cur_occur->type == wo_null);
						cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(cur_occur->next);
						continue;
					}
					else if (cur_occur->ind > fts_sd.cur_word_ind())
					{
						//first word ind is in the b-tree
						t.copy(tuple_cell::node(indirectionDereferenceCP(doc_data->acc)));
						fts_sd.skip_node(doc_data->acc);
						ome = om->rb_successor(ome);

						return true;
					}
					else
					{
						//cur_occur->ind == ce.word_ind
						if (cur_occur->type == wo_del)
						{
							//skip deleted occur in b-tree and in cache and continue
							fts_sd.next_occur();
							cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(cur_occur->next);
							continue;
						}
						U_ASSERT(cur_occur->type == wo_null);

						t.copy(tuple_cell::node(indirectionDereferenceCP(doc_data->acc)));
						fts_sd.skip_node(doc_data->acc);
						ome = om->rb_successor(ome);

						return true;
					}
				}
				else
				{
					//only cache entries are left for this node
					//check that there is at least one wo_add occur
					if (scan_occurs(id, cur_occur))
					{
						t.copy(tuple_cell::node(indirectionDereferenceCP(doc_data->acc)));
						ome = om->rb_successor(ome);
						return true;
					}
					else
					{
						//FIXME: assert that there are only wo_null occurs in ome
						ome = om->rb_successor(ome);
						return false;
					}
				}
			}
		}
		else if (fts_sd.cur_node() < doc_data->acc)
		{
			xptr p = fts_sd.cur_node();
			t.copy(tuple_cell::node(indirectionDereferenceCP(p)));
			fts_sd.skip_node(p);
			return true;
		}
		else //(fts_sd.cur_node() > doc_data->acc)
		{
			//check that there is at least one wo_add occur
			ftc_word_occur *cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(ome->obj.first);
			if (scan_occurs(id, cur_occur))
			{
				t.copy(tuple_cell::node(indirectionDereferenceCP(doc_data->acc)));
				ome = om->rb_successor(ome);
				return true;
			}
			else
			{
				//FIXME: assert that there are only wo_null occurs in ome
				ome = om->rb_successor(ome);
				return false;
			}
		}
		U_ASSERT(false);
	}
}
void ftc_scan_result::get_next_result(tuple &t)
{
	while (!this->get_next_result_step(t)) ;
}