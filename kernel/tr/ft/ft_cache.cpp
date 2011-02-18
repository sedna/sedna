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
	int delete_op_ind;
	int add_done_op_ind;
	int doc_len;
};
struct ftc_word_occur
{
	FTC_PTR pred;
	FTC_PTR next;
	int ind;
	int insert_op_ind;
};
struct ftc_index_data;
void index_oom_flush(ftc_index_data *id);
struct ftc_index_data
{
	FTC_PTR docs;
	FTC_PTR words;
	struct FtsData ft_data;
	int naccs;
	FTC_ALLOCATOR ind_alloc;
	FtStemmer *stemmer; //TODO: add some destructor and delete it + consider sharing stemmers between indexes
	bool has_stemmer; //flag to avoid searching for stemmer more than once
	int next_op_ind;
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
		id->stemmer = NULL;
		id->has_stemmer = false;
		id->reset();
		strcpy(id->name, name);
		id->ft_data = *ft_data; //FIXME: add some explicit copy function
		return ptr;
	}
	void reset()
	{
		docs = FTC_DOCMAP::init(&ind_alloc); //TODO: check null!
		words = FTC_WORDMAP::init(&ind_alloc); //TODO: check null!
		next_op_ind = 1;
		naccs = 0;
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
		sprintf(doc->str_id, "%020" PRIx64, FT_XPTR_TO_UINT(acc));
		doc->delete_op_ind = 0;
		doc->add_done_op_ind = 0;
		doc->doc_len = 0;

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
		sprintf(str_id, "%020" PRIx64, FT_XPTR_TO_UINT(acc));
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
	bool upd_occur(ftc_word_data *wd, ftc_doc_t doc, int word_ind, int op_ind)
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
					new_occur->insert_op_ind = op_ind;
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
					//update node
					od->cursor = pred_ptr;
					pred_occur->insert_op_ind = op_ind;
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
					new_occur->insert_op_ind = op_ind;
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
						new_occur->insert_op_ind = op_ind;
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
		else //if (od != NULL)
		{
			FTC_PTR new_occur_ptr = ind_alloc.alloc(sizeof(ftc_word_occur));
			if (new_occur_ptr == ind_alloc.null_ptr())
				return false;
			ftc_word_occur *new_occur = (ftc_word_occur *)ind_alloc.deref(new_occur_ptr);

			new_occur->ind = word_ind;
			new_occur->insert_op_ind = op_ind;
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

FtStemmer *ftc_get_stemmer(ftc_index_t idx)
{
	ftc_index_data *id = ftc_index_data::get(idx);

	if (id->stemmer == NULL && !id->has_stemmer)
	{
		ft_index_cell_cptr idc = find_ft_index(id->name, NULL);

		if (idc->stemming != NULL && idc->stemming[0] != '\x0')
		{
			id->stemmer = FtStemmer::get(idc->stemming);
			if (id->stemmer == NULL)
				throw USER_EXCEPTION2(SE3022, "failed to create stemmer");
		}

		id->has_stemmer = true;
	}

	return id->stemmer;
}

#include "common/errdbg/d_printf.h"
//needs ft_index_sem
//FIXME: does it really need ft_index_sem??
void flush_index(ftc_index_data *id)
{
	FTC_WORDMAP *wm = FTC_WORDMAP::get_map(id->words, id->ind_alloc);
	FTC_WORDMAP::pers_sset_entry *wme = wm->rb_minimum(FTC_WORDMAP::get_entry(wm->root, id->ind_alloc));
	FtsUpdater updater;
	d_printf1("ftc_flush start\n");
	updater.begin_update(&id->ft_data);

	//supply deleted documents list to updater
	{
		FTC_DOCMAP *dm = FTC_DOCMAP::get_map(id->docs, id->ind_alloc);
		FTC_DOCMAP::pers_sset_entry *dme = dm->rb_minimum(FTC_DOCMAP::get_entry(dm->root, id->ind_alloc));
		while (dme != NULL)
		{
			ftc_doc_data *doc_data = id->get_doc_data(dme->obj);
			if (doc_data->delete_op_ind > 0)
				updater.del_document(doc_data->acc);
			if (doc_data->add_done_op_ind > doc_data->delete_op_ind && doc_data->doc_len > 0)
				updater.add_document(doc_data->acc, doc_data->doc_len);
			dme = dm->rb_successor(dme);
		}
	}

	//add new postings
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
					if (wo->insert_op_ind > doc_data->delete_op_ind)
					{
						updater.add_word_occur(wme->str, doc_data->acc, wo->ind);
					}
					wo = (ftc_word_occur *)id->ind_alloc.deref(wo->next);
				}
				ome = om->rb_successor(ome);
				//ome = om->rb_predecessor(ome);
			}
		}

		wme = wm->rb_successor(wme);
	}
	updater.end_update(&id->ft_data);
	d_printf1("ftc_flush end\n");

	ft_index_cell_cptr idc = find_ft_index(id->name, NULL);
	idc.modify();
	idc->fts_data = id->ft_data;


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
	ftc_indexes = FTC_NULL;
	m_alloc.release();
}

void ftc_del_doc(ftc_index_t index, const xptr acc)
{
	ftc_doc_t ft_doc = ftc_get_doc(index, acc);
	ftc_index_data *id = ftc_index_data::get(index);
	ftc_doc_data *dd = id->get_doc_data(ft_doc);
	dd->delete_op_ind = id->next_op_ind;
	id->naccs--;
	id->next_op_ind++;
}

//returns true on success, false if alloc failed
static bool ftc_upd_word_try(ftc_index_data *id, ftc_doc_t &ft_doc, const char *word, int word_ind)
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

	if (!id->upd_occur(wd, ft_doc, word_ind, id->next_op_ind))
	{
		return false;
	}
	id->next_op_ind++;
	return true;
}

void ftc_upd_word(ftc_index_t index, ftc_doc_t &ft_doc, const char *word, int word_ind, bool no_flush)
{
	ftc_index_data *id = ftc_index_data::get(index);

	if (!ftc_upd_word_try(id, ft_doc, word, word_ind))
	{
		ftc_doc_data *dd = id->get_doc_data(ft_doc);
		xptr acc = dd->acc;
		index_oom_flush(id);
		ft_doc = ftc_add_new_doc(index, acc);
		if (!ftc_upd_word_try(id, ft_doc, word, word_ind))
			throw SYSTEM_EXCEPTION("ftc: add_occur failed");
	}
}

void ftc_finalize_doc(ftc_index_t index, ftc_doc_t ft_doc, int doc_len)
{
	ftc_index_data *id = ftc_index_data::get(index);
	ftc_doc_data *dd = id->get_doc_data(ft_doc);
	dd->add_done_op_ind = id->next_op_ind;
	dd->doc_len = doc_len;
	id->naccs++;
	id->next_op_ind++;
}

bool FtCacheScanner::scan_occurs()
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);
	ftc_doc_data *doc_data = id->get_doc_data(ome->obj.doc);
	bool res = false;

	while (true)
	{
		while (cur_occur != NULL)
		{
			if (cur_occur->insert_op_ind > doc_data->delete_op_ind)
			{
				this->cur_acc_i = FT_XPTR_TO_UINT(doc_data->acc); //FIXME
				return res;
			}
			cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(cur_occur->next);
		}
		res = true;
		ome = om->rb_successor(ome);
		if (ome == NULL)
		{
			this->cur_acc_i = FT_ACC_UINT_NULL;
			return true;
		}
		doc_data = id->get_doc_data(ome->obj.doc);
	}
}

void FtCacheScanner::init_word(const char *word)
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);

	FTC_WORDMAP *wm = FTC_WORDMAP::get_map(id->words, id->ind_alloc);
	ftc_word_data *wd = wm->find(word);
	
	if (wd != NULL)
	{
		om = FTC_OCCURMAP::get_map(wd->occur_map, id->ind_alloc);
		ome = om->rb_minimum(FTC_OCCURMAP::get_entry(om->root, id->ind_alloc));
		cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(ome->obj.first);
		scan_occurs();
	}
	else
	{
		om = NULL;
		ome = NULL;
		cur_occur = NULL;
		this->cur_acc_i = FT_ACC_UINT_NULL;
	}
}
int FtCacheScanner::cur_word_ind()
{
	U_ASSERT(cur_occur != NULL);
	return cur_occur->ind;
}

void FtCacheScanner::next_occur()
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);
	cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(ome->obj.first);
	scan_occurs();
}

int FtCacheScanner::skip_acc()
{
	if (ome == NULL)
		return 0;
	ftc_index_data *id = ftc_index_data::get(ftc_idx);
	ftc_doc_data *doc_data = id->get_doc_data(ome->obj.doc);

	int noccurs = 0;
	while (cur_occur != NULL)
	{
		if (cur_occur->insert_op_ind > doc_data->delete_op_ind)
			noccurs++;
		cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(cur_occur->next);
	}

	ome = om->rb_successor(ome);
	if (ome == NULL)
	{
		this->cur_acc_i = FT_ACC_UINT_NULL;
		cur_occur = NULL;
		return noccurs;
	}
	cur_occur = (struct ftc_word_occur*)id->ind_alloc.deref(ome->obj.first);
	scan_occurs();
	return noccurs;
}

bool FtCacheScanner::acci_deleted(uint64_t acc_i)
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);
	ftc_doc_t doc = id->find_doc(FT_UINT_TO_XPTR(acc_i)); //FIXME
	if (!id->doc_is_null(doc))
	{
		ftc_doc_data *doc_data = id->get_doc_data(doc);
		if (doc_data->delete_op_ind > 0)
			return true;
	}
	return false;
}

uint64_t FtCacheScanner::get_doc_len(ft_acc_uint_t acc_i)
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);
	ftc_doc_t doc = id->find_doc(FT_UINT_TO_XPTR(acc_i)); //FIXME
	if (!id->doc_is_null(doc))
	{
		ftc_doc_data *doc_data = id->get_doc_data(doc);
		if (doc_data->add_done_op_ind > doc_data->delete_op_ind)
			return doc_data->doc_len;
		return 0;
	}
	return 0;
}

void ftc_scan_result::scan_word(const char *word)
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);

	ftc_s.init_word(word);
	fts_sd.init(&id->ft_data, word);
}

bool ftc_scan_result::get_next_occur_step(ft_acc_uint_t *acc_i, int *word_ind)
{
	if (ftc_s.at_end())
	{
		//no more entries in cache for this word
		while (true)
		{
			if (fts_sd.at_end())
			{
				*acc_i = FT_ACC_UINT_NULL;
				return true;
			}
			ft_acc_uint_t a_i = fts_sd.get_cur_acc_i();

			//check if document was deleted
			if (ftc_s.acci_deleted(a_i))
			{
				fts_sd.skip_node();
				continue;
			}

			*acc_i = a_i;
			*word_ind = fts_sd.cur_word_ind();
			fts_sd.next_occur();
			return true;
		}
	}
	else //if (ftc_s.at_end())
	{
		while (!fts_sd.at_end() && fts_sd.get_cur_acc_i() <= ftc_s.get_cur_acc_i())
		{
			if (ftc_s.acci_deleted(fts_sd.get_cur_acc_i()))
				fts_sd.skip_node();
			else
				break;
		}

		if (fts_sd.at_end())
		{
			*acc_i = ftc_s.get_cur_acc_i();
			*word_ind = ftc_s.cur_word_ind();
			ftc_s.next_occur();
			return true;
		}

		//now either fts_sd.get_cur_acc_i() > ftc_s.get_cur_acc_i() or !ftc_s.acci_deleted(fts_sd.get_cur_acc_i())

		//fts_sd has some nodes
		if (fts_sd.get_cur_acc_i() == ftc_s.get_cur_acc_i())
		{

			if (fts_sd.cur_word_ind() < ftc_s.cur_word_ind())
			{
				*acc_i = fts_sd.get_cur_acc_i();
				*word_ind = fts_sd.cur_word_ind();
				fts_sd.next_occur();
				return true;
			}
			else if (fts_sd.cur_word_ind() > ftc_s.cur_word_ind())
			{
				*acc_i = ftc_s.get_cur_acc_i();
				*word_ind = ftc_s.cur_word_ind();
				ftc_s.next_occur();
				return true;
			}
			else
			{
				U_ASSERT(fts_sd.cur_word_ind() == ftc_s.cur_word_ind());
				U_ASSERT(false); //same word can't be in different index partitions
				return false;
			}
		}
		else if (fts_sd.get_cur_acc_i() < ftc_s.get_cur_acc_i())
		{
			U_ASSERT(!ftc_s.acci_deleted(fts_sd.get_cur_acc_i()));

			*acc_i = fts_sd.get_cur_acc_i();
			*word_ind = fts_sd.cur_word_ind();
			fts_sd.next_occur();
			return true;
		}
		else //(fts_sd.get_cur_acc_i() > ftc_s.cur_acc_i())
		{
			*acc_i = ftc_s.get_cur_acc_i();
			*word_ind = ftc_s.cur_word_ind();
			ftc_s.next_occur();
			return true;
		}
	}
	U_ASSERT(false);
}
void ftc_scan_result::get_next_occur(ft_acc_uint_t *acc_i, ft_word_ind_t *word_ind)
{
	if (*acc_i != FT_ACC_UINT_NULL)
	{
		//skip acc-s so that cur_acc >= *acc
		while (!fts_sd.at_end() && fts_sd.get_cur_acc_i() < *acc_i)
			fts_sd.skip_node();
		while (!fts_sd.at_end() && fts_sd.get_cur_acc_i() == *acc_i && fts_sd.cur_word_ind() < *word_ind)
			fts_sd.next_occur();

		while (!ftc_s.at_end() && ftc_s.get_cur_acc_i() < *acc_i)
			ftc_s.skip_acc();
		while (!ftc_s.at_end() && ftc_s.get_cur_acc_i() == *acc_i && ftc_s.cur_word_ind() < *word_ind)
			ftc_s.next_occur();
	}
	while (!this->get_next_occur_step(acc_i, word_ind)) ;
}

uint64_t ftc_scan_result::get_doc_len(ft_acc_uint_t acc_i)
{
	uint64_t r = ftc_s.get_doc_len(acc_i);
	if (r > 0)
		return r;
	return fts_sd.get_doc_len(acc_i);
}

int ftc_get_doc_count(ftc_index_t idx)
{
	ftc_index_data *id = ftc_index_data::get(idx);
	return id->ft_data.naccs + id->naccs; //FIXME: may be wrong, but should be close enough
}

class FtcWordsScanner : public FtWordsScanner
{
private:
	FTC_WORDMAP *wm;
	FTC_WORDMAP::pers_sset_entry *wme;
public:
	FtcWordsScanner(ftc_index_data *id);
	virtual const char *cur_word();
	virtual void next_word();
	virtual ~FtcWordsScanner() {}
};
FtcWordsScanner::FtcWordsScanner(ftc_index_data *id)
{
	wm = FTC_WORDMAP::get_map(id->words, id->ind_alloc);
	wme = wm->rb_minimum(FTC_WORDMAP::get_entry(wm->root, id->ind_alloc));
}
const char *FtcWordsScanner::cur_word()
{
	if (wme == NULL)
		return NULL;
	return wme->str;
}
void FtcWordsScanner::next_word()
{
	wme = wm->rb_successor(wme);
}

ftc_scan_words_result::ftc_scan_words_result(ftc_index_t idx) : ftc_idx(idx)
{
	ftc_index_data *id = ftc_index_data::get(ftc_idx);

	nscanners = id->ft_data.npartitions + 1;
	scanners = new FtWordsScanner*[nscanners];

	for (int i = 0; i < id->ft_data.npartitions; i++)
		scanners[i] = ftp_init_words_scanner(&id->ft_data.partitions[i]);
	scanners[id->ft_data.npartitions] = new FtcWordsScanner(id);

}

void ftc_scan_words_result::get_next_result(tuple &t)
{
	const char *minw = NULL;
	for (int i = 0; i < nscanners; i++)
	{
		const char *w = scanners[i]->cur_word();
		if (w != NULL)
		{
			if (minw == NULL || strcmp(w, minw) < 0)
				minw = w;
		}
	}
	if (minw == NULL)
	{
		t.set_eos();
		return;
	}
	tuple_cell tc = tuple_cell::atomic_deep(xs_string, minw);
	minw = tc.get_str_mem();
	t.copy(tc);

	for (int i = 0; i < nscanners; i++)
		if (scanners[i]->cur_word() != NULL && !strcmp(scanners[i]->cur_word(), minw))
			scanners[i]->next_word();
}

ftc_scan_words_result::~ftc_scan_words_result()
{
	for (int i = 0; i < nscanners; i++)
		delete scanners[i];
	delete[] scanners;
}
