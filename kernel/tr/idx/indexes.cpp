/*
 * File:  indexes.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <vector>

#include "common/sedna.h"
#include "common/errdbg/d_printf.h"
#include "tr/idx/indexes.h"
#include "tr/structures/schema.h"
#include "tr/vmm/vmm.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/log/log.h"
#include "tr/executor/base/sorted_sequence.h"
#include "tr/executor/fo/op_map.h"
#include "tr/strings/utf8.h"


using namespace std;


void tuple_cell2bt_key(const tuple_cell& /*in*/ tc, bt_key& /*out*/ key)
{
    tuple_cell ltc = tuple_cell::make_sure_light_atomic(tc);
    // !!! ZAGLUSHKA
    // Note that the function call above should sometimes rase an exception
    // We must handle it and throw QEPTypeException that will be cought later
    // and the error counter will be incremented

    switch (ltc.get_atomic_type())
    {
        case xs_integer 		 : key.setnew((__int64)(ltc.get_xs_integer()));	break; //!!! FIX THIS
        case xs_float			 : key.setnew(ltc.get_xs_float());	break;
        case xs_double			 : key.setnew(ltc.get_xs_double());	break;
        case xs_string			 : key.setnew(ltc.get_str_mem());	break;
        case xs_date             :
        case xs_dateTime         :
        case xs_time             : key.setnew_dateTime(ltc.get_xs_dateTime(), ltc.get_atomic_type()); break;
        case xs_yearMonthDuration:
        case xs_dayTimeDuration  : key.setnew_duration(ltc.get_xs_duration(), ltc.get_atomic_type()); break;

        default					 : throw USER_EXCEPTION2(SE1003, "Unsupported type of index");
    }
}

//void bt_key2tuple_cell(const bt_key& /*in*/ key, tuple_cell& /*out*/ tc)
//{
//    switch (key.type)
//    {
//        case xs_float			: tc = tuple_cell::atomic(*(float*)(key.head));
//                                  break;
//        case xs_double			: tc = tuple_cell::atomic(*(double*)(key.head));
//                                  break;
//        case xs_string			: {
//                                      char *tmp = se_new char[key.size + 1];
//                                      memcpy(tmp, key.head, key.size);
//                                      tmp[key.size] = '\0';
//                                      tc = tuple_cell::atomic(xs_string, tmp);
//                                      break;
//                                  }
//        case xs_integer 		: tc = tuple_cell::atomic(*(int*)(key.head));
//                                  break;
//        default					: throw USER_EXCEPTION2(SE1003, "Unsupported type of index");
//    }
//}

pers_sset<index_cell,unsigned short>::pers_sset_entry* search_indexdata_cell(const char *index_title)
{
	return indexdata->get(index_title,NULL);	
}

void inline free_indexdata_cell(pers_sset<index_cell,unsigned short>::pers_sset_entry* entry)
{
	index_cell* idc=entry->obj;
	indexdata->rb_delete(entry);
	if (idc->index_title!=NULL)
		scm_free(idc->index_title,true);
	delete_PathExpr(idc->key);
	delete_PathExpr(idc->object);
	scm_free(idc->doc_name,true);
	scm_free(idc,true);
}

index_cell* create_inner_index (xmlscm_type key_type)
{
	index_cell* idc=(index_cell*)scm_malloc(sizeof(index_cell),true);
	idc->keytype = key_type;
	idc->err_cntr = 0;
	idc->btree_root = bt_create(key_type);
	return idc;
}

index_cell* create_index (PathExpr *object_path, 
                          PathExpr *key_path, 
                          xmlscm_type keytype, 
                          doc_schema_node* schemaroot,
                          const char * index_title, 
                          const char* doc_name,
                          bool is_doc)
{
	// 0. These counters are useful in debug 
	__int64 counter1 = 0;
	__int64 counter2 = 0;
	
	// I. Create and fill new index cell
	index_sem_down();
	if (search_indexdata_cell(index_title)!=NULL)
	{
		index_sem_up();	
		throw USER_EXCEPTION(SE2033);
	}
	down_concurrent_micro_ops_number();
	index_cell* idc=(index_cell*)scm_malloc(sizeof(index_cell),true);
    idc->id = *idx_counter;
    (*idx_counter)++;
	idc->keytype = keytype;
	idc->schemaroot = schemaroot;
	schemaroot->create_index(idc);
	idc->err_cntr = 0;
	idc->btree_root = bt_create(keytype);
	idc->object = object_path;
	idc->key = key_path;
	idc->index_title=(char*)scm_malloc(strlen(index_title)+1,true);
	strcpy(idc->index_title,index_title);
	idc->doc_name=(char*)scm_malloc(strlen(doc_name)+1,true);
	strcpy(idc->doc_name,doc_name);
	idc->is_doc=is_doc;
	/*idc->next = *indexdata;
	if ((*indexdata) != NULL) (*indexdata)->pred = idc;
	*indexdata = idc;*/
	indexdata->put(idc);
	index_sem_up();
	
	idx_user_data ud;
	ud.t=keytype;
	ud.buf=se_new idx_buffer();
	sorted_sequence * ss = se_new sorted_sequence(idx_compare_less,idx_get_size,idx_serialize,idx_serialize_2_blks,idx_deserialize,idx_deserialize_2_blks,&ud);
	tuple tup(2);
	
	// ALGORITHM: indexing data

	//II. Execute abs path (object_path) on the desriptive schema
    t_scmnodes sobj = execute_abs_path_expr(schemaroot, object_path);
	//III. For each schema node found (sn_obj)
	for (int i = 0; i < sobj.size(); i++)
	{
		//IV. Execute path expression (key_path) on the descriptive schema
		t_scmnodes skey = execute_abs_path_expr(sobj[i], key_path);
		//V. For each schema node found (sn_key)
        for (int j = 0; j < skey.size(); j++)
		{
			//VI. Add pair <&ind,&sn_obj> into schema node (the special list is used)
			skey[j]->add_index(idc,sobj[i]);

			if (skey[j]->bblk != XNULL)
			{
				xptr blk = skey[j]->bblk;
				CHECKP(blk);
				xptr node_key = GETBLOCKFIRSTDESCRIPTORABSOLUTE(((node_blk_hdr*)XADDR(blk)));
				//VII. For every descriptor node_key that corresponds to sn_key.
				while (node_key != XNULL)
				{
					// Андрей, тут мне кажется надо light-atomic делать. Иначе после Chekp значение съедет.
					// конструировать ключь надо в стеке (то есть без new), тогда деструктор 
					// будет вызываться автоматически

                    try {

						//VIII. Evaluate key: (cast typed-value(node_key) as key_type)->bt_key
                        tuple_cell tc = cast(dm_typed_value(node_key), keytype);

						//X. Find descriptor object that corresponds to sn_obj
                        //   schema node and is an ancestor to node_key
						xptr obj_indir = getAncestorIndirectionByScheme((n_dsc*)XADDR(node_key), skey[j], sobj[i]);

						
						//XI. Create tuple with key and xptr to value object
						tup.cells[0] = tc;
						tup.cells[1] = tuple_cell::node(obj_indir);
						
                        //XII. Insert created tuple into sorted sequence.
                        ss->add(tup);
                        counter1++;
						
                    } catch (SednaUserException &e) {
						//IX. Increment the counter, which shows the number of 
                        //    items that were not inserted because they have
                        //    the type other than the index has
						idc->err_cntr++;
                    }
					
					node_key = getNextDescriptorOfSameSortXptr(node_key);
				}
			}
		}
	}
	
	//XIII. Perfom sorting of the keys in ascending order.
	ss->lazy_sort();

	while (true)
	{
		ss->next(tup);
		if (tup.is_eos())
		{
			delete ss;
			ss = NULL;
			break;
		}
		else
		{
			try
			{
				//XV. Create and insert key/value pair into index.
				bt_key key;
				tuple_cell2bt_key(tup.cells[0], key);
				CHECKP(idc->btree_root);
				bt_insert(idc->btree_root, key, tup.cells[1].get_node(),false);
                counter2++;
			}
			catch (SednaUserException &e) {
                //XIV. Increment the counter, which shows the number of 
                //     items that were not inserted because they have
                //     the type other than the index has			    
			    idc->err_cntr++;
            }
		}
    }

	hl_logical_log_index(object_path, key_path, keytype,index_title, doc_name,is_doc,true);
	up_concurrent_micro_ops_number();
	return idc;
}

counted_ptr<db_entity> find_entity(const char* title)
{
	xptr res;
    index_sem_down();
	pers_sset<index_cell,unsigned short>::pers_sset_entry* idc=search_indexdata_cell(title);
    if (idc!=NULL)
	{
		db_entity *res = se_new db_entity;
        res->name = se_new char[strlen(idc->obj->doc_name) + 1];
        strcpy(res->name, idc->obj->doc_name);
		res->type = (idc->obj->is_doc) ? dbe_document : dbe_collection;
		index_sem_up();
		return counted_ptr<db_entity>(res);
	}
	else 
	{
		index_sem_up();	
        throw USER_EXCEPTION2(SE1061, (std::string("Index '") + title + "'").c_str());
	}

}

void delete_index (const char *index_title)
{
	index_sem_down();
	pers_sset<index_cell,unsigned short>::pers_sset_entry* idc=search_indexdata_cell(index_title);
	if (idc!=NULL)
	{
		down_concurrent_micro_ops_number();
		xptr b_root=(idc->obj)->btree_root;
		bt_drop(b_root);
		hl_logical_log_index((idc->obj)->object,(idc->obj)->key,(idc->obj)->keytype, (idc->obj)->index_title,(idc->obj)->doc_name,(idc->obj)->is_doc,false);
		index_cell* ic=idc->obj;
		doc_schema_node* sm=(idc->obj)->schemaroot;
		free_indexdata_cell(idc);
		(*idx_counter)--;
		sm->delete_index(ic);
		index_sem_up();
		up_concurrent_micro_ops_number();
	}
	else
		index_sem_up();
}

xptr find_btree(index_id id)
{
     return XNULL;
}

xptr find_btree(const char* title)
{
    xptr res;

    index_sem_down();
	pers_sset<index_cell,unsigned short>::pers_sset_entry* idc=search_indexdata_cell(title);
    if (idc!=NULL)
	{
		res = idc->obj->btree_root;
		index_sem_up();
		return res;		
	}
	else 
	{
		index_sem_up();	
		return XNULL;
	}
}

xmlscm_type get_index_xmlscm_type(const char* title)
{
    xmlscm_type res;

    index_sem_down();
	pers_sset<index_cell,unsigned short>::pers_sset_entry* idc=search_indexdata_cell(title);
    if (idc!=NULL)
	{
		res = idc->obj->keytype;
		index_sem_up();
		return res;		
	}
	else 
	{
		index_sem_up();	
		return -1;
	}
}


////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////
/// idx_buffer && idx_user_data implementations
////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

/*** idx_buffer ***/

void idx_buffer::copy_to_buffer(const void* addr, shft size)
{
	if (size > buffer_lgth)
	{
		if (buffer_lgth)
		{
			delete [] internal_buffer;
		}
		internal_buffer = se_new char[size];
		buffer_lgth = size;
	}	
	memcpy(internal_buffer, addr, size);
}

void idx_buffer::copy_from_buffer(xptr addr, shft shift, shft size)
{
    CHECKP(addr);
    VMM_SIGNAL_MODIFICATION(addr);
    memcpy(XADDR(addr), internal_buffer + shift, size);
}

void idx_buffer::expand_to_size(int size)
{
    if (size > buffer_lgth)
    {
        if (buffer_lgth)
        {
            char* buf = se_new char[size];
            memcpy(buf, internal_buffer, buffer_lgth);
            delete [] internal_buffer;
            internal_buffer = buf;
        }		
        else
           internal_buffer = se_new char[size];
        buffer_lgth = size;		
    }
}

void idx_buffer::copy_to_buffer(const void* addr, shft shift, shft size)
{
    if (size + shift > buffer_lgth)
    {
        if (buffer_lgth)
        {
            char* buf = se_new char[size+shift];
            memcpy(buf, internal_buffer, shift);
            delete [] internal_buffer;
            internal_buffer = buf;
        }		
        else
            internal_buffer = se_new char[size+shift];
        buffer_lgth = size + shift;
    }
    memcpy(internal_buffer + shift, addr, size);
}

void idx_buffer::copy_data_ser_to_buffer(xptr v1, int sz)
{
	if (sz > GET_FREE_SPACE(v1))
	{
		copy_to_buffer(v1, GET_FREE_SPACE(v1));
		xptr nblk = ((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
		CHECKP(nblk);
		copy_to_buffer(XADDR(nblk), GET_FREE_SPACE(v1), sz-GET_FREE_SPACE(v1));
	}
	else
	{
		copy_to_buffer(v1, sz);
	}	
}

/*** idx_user_data ***/

char* idx_user_data::make_sure_temp_size(int n, int size)
{
    int idx = n - 1;    

    if (size > sizes[idx])
    {
        if (sizes[idx])
        {
            char* buf = se_new char[size];
            delete [] temps[idx];
            temps[idx] = buf;
        }		
        else
           temps[idx] = se_new char[size];
        sizes[idx] = size;
    }    
    return temps[idx];
}

////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Function needed by sorted sequence implementation
////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

static tuple_cell get_tc(void* buf, xmlscm_type type, shft size)
{
	switch (type)
    {
		case xs_float                : {float value = *((float*)buf); return tuple_cell::tuple_cell(value); }
        case xs_double               : {double value = *((double*)buf); return tuple_cell::tuple_cell(value); }
        case xs_integer              : {__int64 value = *((__int64*)buf); return tuple_cell::tuple_cell(value); }
		case xs_string               : 
		{ 
			char* str = se_new char[size+1];
			memcpy(str, (char*)buf, size);
			str[size]='\0';
			return tuple_cell::tuple_cell(xs_string, str); 
		}
		case xs_time                 :
        case xs_date                 :
        case xs_dateTime             : {xs_packed_datetime value = *((xs_packed_datetime*)buf); return tuple_cell::tuple_cell(value, type);}
        case xs_yearMonthDuration    : 
        case xs_dayTimeDuration      : {xs_packed_duration value = *((xs_packed_duration*)buf); return tuple_cell::tuple_cell(value, type);}
        default                      : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type.");
    }
}

static void idx_get_size (xptr& v1, xptr& v2, int& s1, int&s2, const void * Udata)
{
	shft sz = 0;
	CHECKP(v1);
	
	if (GET_FREE_SPACE(v1)<sizeof(shft))
	{
		((idx_user_data*)Udata)->buf->copy_to_buffer(v1,GET_FREE_SPACE(v1));
		xptr v=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
		CHECKP(v);
		((idx_user_data*)Udata)->buf->copy_to_buffer(v+sizeof(seq_blk_hdr),GET_FREE_SPACE(v1),sizeof(shft)-GET_FREE_SPACE(v1));
		sz=*(shft*)(((idx_user_data*)Udata)->buf->get_buffer_pointer());
		CHECKP(v1);
	}
	else
	{
		sz=*((shft*)XADDR(v1));					
	}
	if (GET_FREE_SPACE(v1)<sizeof(shft)+sizeof(xptr)+sz)
	{
		if (GET_FREE_SPACE(v1)<=sizeof(shft)+sizeof(xptr))
		{
			xptr v=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
			v1=v+sizeof(seq_blk_hdr)+(sizeof(shft)+sizeof(xptr)-GET_FREE_SPACE(v1));
			v2=XNULL;
			s1=sz;
			s2=0;
		}
		else
		{			
			xptr v=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
			s1=GET_FREE_SPACE(v1)-(sizeof(shft)+sizeof(xptr));
			v1=v1+(sizeof(shft)+sizeof(xptr));
			v2=v+sizeof(seq_blk_hdr);			
			s2=sz-s1;
		}
	}
    else
    {
        v1=v1+(sizeof(shft)+sizeof(xptr));
        v2=XNULL;
        s1=sz;
        s2=0;
    }
}

static inline xptr idx_get_ptr_to_complete_serialized_data(xptr v, /* out */ char** temp, int n, idx_user_data* ud, /* out */ int& s)
{
    CHECKP(v);
    xptr v1 = v;
    xptr v2;
    int s1 = 0, s2 = 0;
    
    idx_get_size(v1, v2, s1, s2, ud);
    s = s1 + s2;
	
	if(v1 != XNULL && v2 != XNULL)
    {
        *temp = ud->make_sure_temp_size(n, s);
		CHECKP(v1);
        memcpy(*temp, XADDR(v1), s1);
        CHECKP(v2);
        memcpy(*temp + s1, XADDR(v2), s2);
        return XNULL;
    }
	else 
	{
		if(v1 == XNULL) return v2;
		else return v1;    
	}
}

int idx_compare_less(xptr v1, xptr v2, const void * Udata)
{
	idx_user_data* ud = (idx_user_data*)Udata;
    xmlscm_type type = ud->t;
	
    char* temp1 = NULL;
    char* temp2 = NULL; 
    xptr addr1, addr2;
	int s1, s2;
    
    addr1 = idx_get_ptr_to_complete_serialized_data(v1, &temp1, 1, ud, s1);
    addr2 = idx_get_ptr_to_complete_serialized_data(v2, &temp2, 2, ud, s2);
    
	if(addr1 != XNULL) CHECKP(addr1);
	tuple_cell tc1 = get_tc(addr1 != XNULL ? XADDR(addr1) : temp1, type, s1);
	if(addr2 != XNULL) CHECKP(addr2);
    tuple_cell tc2 = get_tc(addr2 != XNULL ? XADDR(addr2) : temp2, type, s2);

    get_binary_op_res r = get_binary_op(xqbop_lt, type, type);
    
    bool result;
    if (r.collation) 
        result = r.f.bf_c(tc1, tc2, charset_handler->get_unicode_codepoint_collation()).get_xs_boolean();
    else
        result = r.f.bf(tc1, tc2).get_xs_boolean();
	
    if(result) return -1;
    r = get_binary_op(xqbop_gt, type, type);
    
    if (r.collation) 
        result = r.f.bf_c(tc1, tc2, charset_handler->get_unicode_codepoint_collation()).get_xs_boolean();
    else
	    result = r.f.bf(tc1, tc2).get_xs_boolean();
    if(result) return 1;

    return 0;
}

int idx_get_size (tuple& t, const void * Udata)
{
	xmlscm_type tp=((idx_user_data*)Udata)->t;
	return sizeof(shft)+sizeof(xptr)+((xmlscm_type_size(tp) == 0) ? t.cells[0].get_strlen() : xmlscm_type_size(tp));
}

void idx_serialize (tuple& t,xptr v1, const void * Udata)
{
    xmlscm_type type=((idx_user_data*)Udata)->t;
    shft sz=(shft)xmlscm_type_size(type);
    if (!sz)
		sz=t.cells[0].get_strlen();
	CHECKP(v1);
    void * p=XADDR(v1);

#ifdef ALIGNMENT_REQUIRED
    memcpy(p, &sz, sizeof(shft));
    xptr temp_value = t.cells[1].get_node();
    memcpy((char*)p+sizeof(shft), &temp_value, sizeof(xptr));
#else
    *((shft*)p)=sz;
    *((xptr*)((char*)p+sizeof(shft)))=t.cells[1].get_node();
    shft offset=sizeof(shft)+sizeof(xptr);
#endif

    tuple_cell& tc=t.cells[0];

    VMM_SIGNAL_MODIFICATION(v1);
    switch (type)
    {
        case xs_float                : {float value = tc.get_xs_float();  memcpy((char*)p+offset, &value, sz); break;}
        case xs_double               : {double value = tc.get_xs_double(); memcpy((char*)p+offset, &value, sz); break;}
        case xs_integer              : {__int64 value = tc.get_xs_integer(); memcpy((char*)p+offset, &value, sz); break;}
		case xs_string               : 
		{
			tc = tuple_cell::make_sure_light_atomic(tc);
			CHECKP(v1);
			VMM_SIGNAL_MODIFICATION(v1);
			char* str = tc.get_str_mem();
			memcpy((char*)p+offset, str, sz); 
			break;
		}
        case xs_time                 :
        case xs_date                 :
        case xs_dateTime             : {xs_packed_datetime value = tc.get_xs_dateTime(); memcpy((char*)p+offset, &value, sz); break;}
        case xs_yearMonthDuration    : 
        case xs_dayTimeDuration      : {xs_packed_duration value = tc.get_xs_duration(); memcpy((char*)p+offset, &value, sz); break;}
        default                      : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type.");
    }
}

void idx_serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
    idx_user_data* ud = (idx_user_data*)Udata;   
    idx_buffer* buffer = ud -> buf;
    xmlscm_type type=ud->t;
    shft sz=(shft)xmlscm_type_size(type);
    if (!sz)
        sz=t.cells[0].get_strlen();
    buffer->copy_to_buffer(&sz,sizeof(shft));
    xptr tmp=t.cells[1].get_node();
    buffer->copy_to_buffer(&tmp, sizeof(shft),sizeof(xptr));    
    shft offset=sizeof(shft)+sizeof(xptr);
    tuple_cell& tc=t.cells[0];
	
    switch (type)
    {
        case xs_float                : {float value = tc.get_xs_float();  buffer->copy_to_buffer(&value, offset, sz); break;}
        case xs_double               : {double value = tc.get_xs_double(); buffer->copy_to_buffer(&value, offset, sz);  break;}
        case xs_integer              : {__int64 value = tc.get_xs_integer(); buffer->copy_to_buffer(&value, offset, sz); break;}
		case xs_string               : 
		{
			buffer->expand_to_size(offset+sz);
			tc = tuple_cell::make_sure_light_atomic(tc);
			char* str = tc.get_str_mem();
			memcpy(buffer->get_buffer_pointer()+offset, str, sz); 
			break;
		}
        case xs_time                 :
        case xs_date                 :
        case xs_dateTime             : {xs_packed_datetime value = tc.get_xs_dateTime(); buffer->copy_to_buffer(&value, offset, sz); break;}
        case xs_yearMonthDuration    : 
        case xs_dayTimeDuration      : {xs_packed_duration value = tc.get_xs_duration(); buffer->copy_to_buffer(&value, offset, sz); break;}
        default                      : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type.");
    }
	
    buffer->copy_from_buffer(v1, 0, size1);
    buffer->copy_from_buffer(v2, size1, sz+sizeof(shft)+sizeof(xptr)-size1);
}

void idx_deserialize (tuple &t, xptr& v1, const void * Udata)
{
    idx_user_data* ud = (idx_user_data*)Udata;   
    idx_buffer* buffer = ud -> buf;		
    CHECKP(v1);		
    shft sz=*((shft*)XADDR(v1));

#ifdef ALIGNMENT_REQUIRED
    xptr v=v1+sizeof(shft);
    buffer->copy_to_buffer(XADDR(v),sizeof(xptr)+sz);
    t.copy(
        get_tc( buffer->get_buffer_pointer()+sizeof(xptr),
                ((idx_user_data*)Udata)->t,
                sz
               ),
        tuple_cell::node(*((xptr*)buffer->get_buffer_pointer())));
#else
    xptr v2=v1+sizeof(shft);
    xptr v3=v2+sizeof(xptr);
	
	tuple_cell key = get_tc( XADDR(v3),((idx_user_data*)Udata)->t,sz);
	t.copy(key, tuple_cell::node(*((xptr*)XADDR(v2))));
#endif				
}

void idx_deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
	idx_user_data* ud = (idx_user_data*)Udata;   
	idx_buffer* buffer = ud -> buf;	
	xptr vf=v1;
	xptr vs=v2;
	int s1,s2;
	idx_get_size(vf,vs,s1,s2,Udata);
	buffer->copy_to_buffer(v1, size1);
	vs=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
	buffer->copy_to_buffer(vs, size1,s1+s2+sizeof(shft)+sizeof(xptr)-size1);
	t.copy(
			get_tc( buffer->get_buffer_pointer()+sizeof(shft)+sizeof(xptr),
			        ((idx_user_data*)Udata)->t,
					s1+s2
				   ),
			tuple_cell::node(*((xptr*)(buffer->get_buffer_pointer()+sizeof(shft)))));
}

////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////
