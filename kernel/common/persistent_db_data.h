/*
 * File:  persistent_db_data.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef PERSISTENT_DB_DATA_H
#define PERSISTENT_DB_DATA_H


template<class T, class X> struct pers_sset;

struct sn_metadata_cell;
struct xml_ns;
struct index_cell;
struct ft_index_cell;
struct persistent_db_data
{
	pers_sset<sn_metadata_cell, unsigned short> * metadata;
	pers_sset<index_cell, unsigned short> * index;
    	index_id		idx_counter;
	#ifdef SE_ENABLE_FTSEARCH
	pers_sset<ft_index_cell, unsigned short> * ft_index;
	index_id		ft_idx_counter;
	#endif
	unsigned char * last_nid;
	int	last_nid_size;

	pers_sset<xml_ns, unsigned short>* nslist;

    bool is_first_trn; // true if the current transaction is first for the current db

	void init() 
	{
		index = NULL;
        idx_counter = 1;
		last_nid = NULL;
		last_nid_size = 0;
		//support namespaces list
		nslist = NULL;
		metadata = NULL;
        is_first_trn = true;
         #ifdef SE_ENABLE_FTSEARCH
	 ft_index = NULL;
         ft_idx_counter = 1;
         #endif
	}
    bool is_first_transaction() const { return is_first_trn; }
    void clear_first_transaction_flag() { is_first_trn = false; }
};

extern persistent_db_data* entry_point;

#endif

