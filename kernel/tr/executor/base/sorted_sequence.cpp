/*
 * File:  sorted_sequence.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/base/sorted_sequence.h"

sorted_sequence::sorted_sequence(compare_fn _compareFN_, get_size_fn _getSizeFN_, serialize_fn _serializeFN_,
								 serialize_2_blks_fn _serialize2FN_,	deserialize_fn _deserializeFN_,
								 deserialize_2_blks_fn _deserialize2FN_,const void * _Udata_): compareFN(_compareFN_),getSizeFN(_getSizeFN_),serializeFN(_serializeFN_),serialize2FN(_serialize2FN_),deserializeFN(_deserializeFN_),deserialize2FN(_deserialize2FN_),Udata(_Udata_)
{
	finalized=false;
	ptr_place=XNULL;
	val_place=XNULL;
	bblk_in_chain=XNULL;
	blk_cnt=0;
	temp_buffer=se_new char[sizeof(xptr)];
	buf_length=sizeof(xptr);
	merge_tree=NULL;
	top=NULL;
}
sorted_sequence::~sorted_sequence()
{
	delete temp_buffer;
	clear();
	t_xptr_blk_arr::iterator it=empty_blk_arr.begin();
	while (it!=empty_blk_arr.end())
	{
		vmm_delete_block(*it);
		it++;
	}
    empty_blk_arr.clear();
	if (merge_tree!=NULL)
	{
		pers_sset<merge_cell,unsigned short>::pers_sset_entry* tmp=merge_tree->rb_minimum(merge_tree->root);
		while (tmp!=NULL)
		{
			scm_free(tmp->obj,false);
			tmp=merge_tree->rb_successor(tmp);
		}
		pers_sset<merge_cell,unsigned short>::sset_free(merge_tree);
		merge_tree=NULL;
		top=NULL;
	}
}
void sorted_sequence::sort()
{
	if (finalized)
		return;
	if (bblk_in_chain!=XNULL)
	{
		in_mem_sort();
		//5. order
		in_mem_order_data();
		//6. place to stack
	}
	merge_stack(true);
	//7. fix memory	
	unlock_memory();
	seq_size=get_size_in_mem();
	finalized=true;
}
void sorted_sequence::lazy_sort()
{
	if (finalized)
		return;
	if (bblk_in_chain!=XNULL)
	{
		in_mem_sort();
		//5. order
		in_mem_order_data();
		//6. place to stack
	}
	//merge_stack(true);
	//1.init merge_tree
	merge_tree=pers_sset<merge_cell,unsigned short>::init(false);
	//in_mem_block channel
	if (bblk_in_chain!=XNULL)			
	merge_tree->put(merge_cell::init(ptr_blk_arr[0]+sizeof(seq_blk_hdr),compareFN,Udata));
	//sorted sequences
	sorted_sequence::t_sorted_seqs_arr::iterator it= sorted_seqs_arr.begin();
	while (it!=sorted_seqs_arr.end())
	{
		merge_tree->put(merge_cell::init((*it).first+sizeof(seq_blk_hdr),compareFN,Udata));
		++it;
	}	
	
	//7. fix memory	
	unlock_memory();
	//seq_size=get_size_in_mem();
	finalized=true;
}
void sorted_sequence::next(tuple& t)
{
	//t.set_eos();
	if (!top)
	{
		top=merge_tree->rb_minimum(merge_tree->root);		
		if(!top) 
		{
			t.set_eos();
			return;
		}
	}
	xptr res=top->obj->in_node;
	int sz=get_size(top->obj->node);
	if (GET_FREE_SPACE(res)<sz)
	 {
		 CHECKP(res);
		 xptr v2=((seq_blk_hdr*)XADDR(BLOCKXPTR(res)))->nblk;
		 deserialize2FN(t,res,GET_FREE_SPACE(res),v2,Udata);
	 }
	 else
		 deserializeFN(t,res,Udata);

	xptr tmp=top->obj->node;
	set_next_ptr_with_free(tmp,false);
	
	if (tmp!=XNULL)
	{
		xptr tmp_in;
		get_val(tmp,tmp_in);
		pers_sset<merge_cell,unsigned short>::pers_sset_entry* nxt=merge_tree->rb_successor(top);
		if (nxt)
		{
			
			if (compareFN(tmp_in,nxt->obj->in_node,Udata)<=0)
			{
				top->obj->node=tmp;	
				top->obj->in_node=tmp_in;	
			}
			else
			{
				merge_cell* nc=top->obj;
				nc->node=tmp;			
				nc->in_node=tmp_in;	
				merge_tree->rb_delete(top);
				merge_tree->put(nc);
				top=nxt;//(following)?merge_tree->rb_minimum(merge_tree->root):merge_tree->rb_maximum(merge_tree->root);				
			}
		}
		else
		{			
			top->obj->node=tmp;							
			top->obj->in_node=tmp_in;		
		}			
	}
	else
	{			
		pers_sset<merge_cell,unsigned short>::pers_sset_entry* nxt=merge_tree->rb_successor(top);		
		merge_tree->rb_delete(top);
		scm_free(top->obj,false);
		top=nxt;		
	}	
}
void sorted_sequence::add(tuple& p)
{
	if (finalized)
		throw USER_EXCEPTION2(SE1003, "Failed to add item to already sorted sequence");
	//1.setting initial place
	if (bblk_in_chain==XNULL)
	{
		//1.a initial case
		ptr_blk_arr.clear();
		bblk_in_chain=get_free_block();			
		val_place=bblk_in_chain+sizeof(seq_blk_hdr);
		ptr_place=get_free_block();	
		ptr_blk_arr.push_back(ptr_place);
		ptr_place+=sizeof(seq_blk_hdr);		
	}
	else
	{
		//1.b case of "not first time"
		if (GET_FREE_SPACE(val_place)==0)
		{
			//1.b.a data block is full
			val_place-=PAGE_SIZE;
			set_next_block_in_chain(val_place);			
		}
		CHECKP(ptr_place);
		if( ((seq_blk_hdr*)XADDR(BLOCKXPTR(ptr_place)))->cursor<PTR_BLK_SIZE)
		{
			//1.b.b ptr fits into existing block
			ptr_place+=sizeof(data_ptr);
		}
		else
		{
			//1.b.c ptr fits into existing block
			set_next_block_in_chain(ptr_place);
			ptr_blk_arr.push_back(BLOCKXPTR(ptr_place));
		}
	}
	
	//2.serializing data
	//2.1 calc size of data
	int size=getSizeFN(p,Udata);
	//2.2 bind pointers section to data section
	CHECKP(ptr_place);
	data_ptr* ptr=(data_ptr*)XADDR(ptr_place);
	ptr->size=size;
	ptr->value=val_place;
	((seq_blk_hdr*)XADDR(BLOCKXPTR(ptr_place)))->cursor++;
	VMM_SIGNAL_MODIFICATION(ptr_place);
	int fp=GET_FREE_SPACE(val_place);
	if (size>DATA_BLK_SIZE)
		throw USER_EXCEPTION2(SE1003, "Failed to add big item to sequence");
	if (fp<size)
	{
		//2.3.a case of data that doesn't fits to block
		xptr tmp=get_free_block();	
		xptr param=tmp+sizeof(seq_blk_hdr);
		serialize2FN(p,val_place,fp,param,Udata);
		CHECKP(val_place);
		((seq_blk_hdr*)XADDR(BLOCKXPTR(val_place)))->nblk=tmp;
		VMM_SIGNAL_MODIFICATION(val_place);
		val_place=tmp+(sizeof(seq_blk_hdr)+size-fp);
	}
	else
	{
		//2.3.a case of data that fits to block
		serializeFN(p,val_place,Udata);
		val_place+=size;
	}

	//3. checking if the memory is full
	if (blk_cnt<MAX_BLOCKS_IN_CHAIN)
		return;
	//4. sort the sequence
	in_mem_sort();
	//5. order
	in_mem_order_data();
	//6. place to stack
	merge_stack(false);
	//7. fix memory	
	unlock_memory();
}
xptr sorted_sequence::get_free_block()
{
	xptr blk;
	if (empty_blk_arr.size()>0)
	{
		blk= empty_blk_arr.back();
		CHECKP(blk);
		empty_blk_arr.pop_back();		
	}
	else
	{
		vmm_alloc_tmp_block(&blk);
	}
	blk_cnt++;
	seq_blk_hdr::init(XADDR(blk));
	((seq_blk_hdr*)XADDR(blk))->cursor=0;
	//VMM_SIGNAL_MODIFICATION(blk);
	//lock
	return blk;

}
int sorted_sequence::get_size_in_mem()
{
	int sz=(ptr_blk_arr.size()-1)*PTR_BLK_SIZE;
	if (sz<0)
		return 0;
	xptr lblk=ptr_blk_arr.back();
	CHECKP(lblk);
	return sz+((seq_blk_hdr*)XADDR(lblk))->cursor;
}
void sorted_sequence::in_mem_sort()
{
	int sz=get_size_in_mem();
	if (sz==0)
		return;
	sort1(0,sz);
}
void sorted_sequence::sort1(int off, int len)
{
	// Insertion sort on smallest arrays
	if (len < 7) {
	    for (int i=off; i<len+off; i++)
		for (int j=i; j>off && compareFN(get_ptr(j),get_ptr(j-1),Udata)<0; j--)
		    swap( j, j-1);
	    return;
	}

	// Choose a partition element, v
	int m = off + (len >> 1);       // Small arrays, middle element
	if (len > 7) {
	    int l = off;
	    int n = off + len - 1;
	    if (len > 40) {        // Big arrays, pseudomedian of 9
		int s = len/8;
		l = med3(l,     l+s, l+2*s);
		m = med3( m-s,   m,   m+s);
		n = med3( n-2*s, n-s, n);
	    }
	    m = med3( l, m, n); // Mid-size, med of 3
	}
	xptr v = get_ptr(m);

	// Establish Invariant: v* (<v)* (>v)* v*
	int a = off, b = a, c = off + len - 1, d = c;
	while(true) {
	    while (b <= c && compareFN( v,get_ptr(b),Udata)>=0) {
		if (!compareFN(get_ptr(b), v,Udata))
		    swap(a++, b);
		b++;
	    }
	    while (c >= b && compareFN(v,get_ptr(c),Udata)<=0) {
		if (!compareFN(get_ptr(c),v,Udata))
		    swap(c, d--);
		c--;
	    }
	    if (b > c)
		break;
	    swap(b++, c--);
	}

	// Swap partition elements back to middle
	int s, n = off + len;
	s = min(a-off, b-a  );  vecswap( off, b-s, s);
	s = min(d-c,   n-d-1);  vecswap( b,   n-s, s);

	// Recursively sort non-partition-elements
	if ((s = b-a) > 1)
	    sort1( off, s);
	if ((s = d-c) > 1)
	    sort1(n-s, s);
}

void sorted_sequence::swap(int a, int b)
{
	xptr t = get_data(a);
	data_ptr* ct=(data_ptr*)XADDR(t);
	data_ptr dp,sp;
	CHECKP(t);
	dp.size=ct->size;
	dp.value=ct->value;
	xptr p = get_data(b);
    data_ptr* dt=(data_ptr*)XADDR(p);
	CHECKP(p);
	sp.size=dt->size;
	sp.value=dt->value;
	dt->size=dp.size;
	dt->value=dp.value;
	VMM_SIGNAL_MODIFICATION(p);
	CHECKP(t);
	ct->size=sp.size;
	ct->value=sp.value;
	VMM_SIGNAL_MODIFICATION(t);
	
}
 
/**
 * Returns the index of the median of the three indexed longs.
 */
int sorted_sequence::med3( int a, int b, int c) 
{
	return (compareFN(get_ptr(a),get_ptr(b),Udata)<0 ?
		(compareFN(get_ptr(b),get_ptr(c),Udata)<0 ? b : compareFN(get_ptr(a),get_ptr(c),Udata)<0 ? c : a) :
		(compareFN(get_ptr(c),get_ptr(b),Udata)<0 ? b : compareFN(get_ptr(c),get_ptr(a),Udata)<0 ? c : a));
}

 /**
  * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
 */
void sorted_sequence::vecswap(int a, int b, int n) 
{
	for (int i=0; i<n; i++, a++, b++) swap(a, b);
}
void sorted_sequence::in_mem_order_data()
{
	
	int sz=get_size_in_mem();
	if (!sz) return;
	//xptr cur=get_data(0);
	xptr new_chain=get_free_block()+sizeof(seq_blk_hdr);	
	//1.copying to new place
	for (int i=0;i<sz;i++)	
		copy_data_to_new_place(get_data(i),new_chain);
	//2. free old data blocks
	xptr blk=bblk_in_chain;
	while (blk!=XNULL)
	{
		//unlock
		empty_blk_arr.push_back(blk);
		CHECKP(blk);
		blk=((seq_blk_hdr*)XADDR(blk))->nblk;
	}
}
void sorted_sequence::set_next_block_in_chain(xptr& place, bool marking)
{
	xptr tmp=get_free_block();	
	if (marking) ptr_blk_arr.push_back(tmp);
	CHECKP(place);
	((seq_blk_hdr*)XADDR(BLOCKXPTR(place)))->nblk=tmp;
	VMM_SIGNAL_MODIFICATION(place);
	place=tmp+sizeof(seq_blk_hdr);
}
void sorted_sequence::copy_data_to_new_place(xptr ptr,xptr& place)
{
	if (!GET_FREE_SPACE(place))
	{
		place-=PAGE_SIZE;
		set_next_block_in_chain(place);	
	}
	CHECKP(ptr);
	data_ptr* dpr=(data_ptr*)XADDR(ptr);	
	xptr val_ptr=dpr->value;
	dpr->value=place;	
	VMM_SIGNAL_MODIFICATION(ptr);
	int sz=dpr->size;
	int fpart=(GET_FREE_SPACE(val_ptr)>=sz)?sz:GET_FREE_SPACE(val_ptr);
	int spart=sz-fpart;
	int space=GET_FREE_SPACE(place);	
	if (buf_length<sz)
	{
		delete[] temp_buffer;
		temp_buffer=se_new char[sz];
		buf_length=sz;
	}	
	CHECKP(val_ptr);
	memcpy(temp_buffer,XADDR(val_ptr),fpart);
	if (spart>0)
	{
		xptr tmp=((seq_blk_hdr*)XADDR((BLOCKXPTR(val_ptr))))->nblk;
		CHECKP(tmp);
		memcpy(temp_buffer+fpart,XADDR((tmp+sizeof(seq_blk_hdr))),spart);	
	}
	if (space<sz)
	{
		CHECKP(place);
		memcpy(XADDR(place),temp_buffer,space);		
		set_next_block_in_chain(place);
		CHECKP(place);
		memcpy(XADDR(place),temp_buffer+space,sz-space);
		VMM_SIGNAL_MODIFICATION(place);
		place+=(sz-space);	
		
	}
	else
	{
		CHECKP(place);
		memcpy(XADDR(place),temp_buffer,sz);
		VMM_SIGNAL_MODIFICATION(place);
		place+=sz;		
	}	
}
void sorted_sequence::copy_ptr_to_new_place(xptr ptr,xptr& place, bool marking)
{
	data_ptr pt;
	CHECKP(ptr);
	pt.size=((data_ptr*)XADDR(ptr))->size;
	pt.value=((data_ptr*)XADDR(ptr))->value;
	CHECKP(place);
	((seq_blk_hdr*)XADDR(BLOCKXPTR(place)))->cursor++;
	((data_ptr*)XADDR(place))->value=pt.value;
	((data_ptr*)XADDR(place))->size=pt.size;
	if( ((seq_blk_hdr*)XADDR(BLOCKXPTR(place)))->cursor<PTR_BLK_SIZE)
		{
			//1.b.b ptr fits into existing block
			place+=sizeof(data_ptr);
		}
		else
		{
			//1.b.c ptr fits into existing block
			set_next_block_in_chain(place,marking);			
		}

}
xptr sorted_sequence::merge_sequences(xptr s1,xptr s2, bool final)
{
	if (final)
	{
		ptr_blk_arr.clear();
	}
	xptr first_seq=s1+sizeof(seq_blk_hdr);
	xptr second_seq=s2+sizeof(seq_blk_hdr);
	xptr v1,v2;
	get_val(first_seq,v1);
	get_val(second_seq,v2);
	xptr last_val_1=BLOCKXPTR(v1);
	xptr last_val_2=BLOCKXPTR(v2);
	//2. create new sequence
	xptr new_seq=get_free_block();
	ptr_blk_arr.push_back(new_seq);
	xptr ptr=new_seq+sizeof(seq_blk_hdr);
	xptr val=get_free_block()+sizeof(seq_blk_hdr);
	while (true)
	{
		//3.comparing values
		if ((first_seq!=XNULL) && (second_seq==XNULL ||compareFN(v1,v2,Udata)<0))
		{
			//1. copy value
			copy_data_to_new_place(first_seq,val);
			//2.copy ptr
			copy_ptr_to_new_place(first_seq,ptr,final);
			//3.shift ptr
			set_next_ptr_with_free(first_seq);
			if (first_seq!=XNULL)
			{
				get_val(first_seq,v1);
				//4. empty data block
				if (BLOCKXPTR(v1)!=last_val_1)
				{
					empty_blk_arr.push_back(last_val_1);
					last_val_1=BLOCKXPTR(v1);
				}
			}
			else
			{
				while (last_val_1!=XNULL)
				{
					empty_blk_arr.push_back(last_val_1);
					CHECKP(last_val_1);
					last_val_1=((seq_blk_hdr*)XADDR(last_val_1))->nblk;
				}
				if (second_seq==XNULL) break;
			}
		}
		else
		{
			//1. copy value
			copy_data_to_new_place(second_seq,val);
			//2.copy ptr
			copy_ptr_to_new_place(second_seq,ptr,final);
			//3.shift ptr
			set_next_ptr_with_free(second_seq);
			if (second_seq!=XNULL)
			{
				get_val(second_seq,v2);
				//4. empty data block
				if (BLOCKXPTR(v2)!=last_val_2)
				{
					empty_blk_arr.push_back(last_val_2);
					last_val_2=BLOCKXPTR(v2);
				}
			}
			else
			{
				while (last_val_2!=XNULL)
				{
					empty_blk_arr.push_back(last_val_2);
					CHECKP(last_val_2);
					last_val_2=((seq_blk_hdr*)XADDR(last_val_2))->nblk;
				}
				if (first_seq==XNULL) break;
			}			
		}
		
	}
	return new_seq;
}


void sorted_sequence::merge_stack(bool final)
{
	if (sorted_seqs_arr.size())
	{
		t_seq_pair sp;
		//1. merging in_mem and last in stack
		xptr res_seq;
		if (bblk_in_chain!=XNULL)
			res_seq=ptr_blk_arr[0];
		else
		{
			res_seq=sorted_seqs_arr.back().first;
			sorted_seqs_arr.pop_back();	
			if (!sorted_seqs_arr.size())			
				return;			
		}
		shft cnt=0;
		while (sorted_seqs_arr.size())
		{			
			sp=sorted_seqs_arr.back();
			if (final || sp.second==cnt)
			{
				res_seq=merge_sequences(res_seq,sp.first,sorted_seqs_arr.size()==1);
				cnt++;
				sorted_seqs_arr.pop_back();				
			}
			else
				break;
		}
		sorted_seqs_arr.push_back(t_seq_pair(res_seq,cnt));	
	}
	else
	{
		if (bblk_in_chain!=XNULL)
			sorted_seqs_arr.push_back(t_seq_pair(ptr_blk_arr[0],0));
	}
}
void sorted_sequence::set_next_ptr_with_free(xptr& ptr, bool free)
{
	int pos=((__uint32)ptr.addr-((__uint32)ptr.addr & PAGE_BIT_MASK)-sizeof(seq_blk_hdr))/sizeof(data_ptr);
	if (pos+1<PTR_BLK_SIZE)
	{
		CHECKP(ptr);
		if (pos+1<((seq_blk_hdr*)XADDR(BLOCKXPTR(ptr)))->cursor)
			ptr+=sizeof(data_ptr);
		else
		{
			if (free) empty_blk_arr.push_back(BLOCKXPTR(ptr));
			ptr=XNULL;
		}
	}
	else
	{
		CHECKP(ptr);
		if (free) empty_blk_arr.push_back(BLOCKXPTR(ptr));
		ptr=((seq_blk_hdr*)XADDR(BLOCKXPTR(ptr)))->nblk;
		if (ptr!=XNULL)ptr+=sizeof(seq_blk_hdr);		
	}
}
void sorted_sequence::unlock_memory()
{
	ptr_place=XNULL;
	val_place=XNULL;
	bblk_in_chain=XNULL;
	blk_cnt=0;
	//unlock all
}
xptr sorted_sequence::get_ptr(int pos)
{
	int ps=pos/PTR_BLK_SIZE;
	int md=pos % PTR_BLK_SIZE;
	CHECKP(ptr_blk_arr[ps]);
	return ((data_ptr*)XADDR((ptr_blk_arr[ps]+(md*sizeof(data_ptr)+sizeof(seq_blk_hdr)))))->value;
}
xptr sorted_sequence::get_data(int pos)
{
	int ps=pos/PTR_BLK_SIZE;
	int md=pos % PTR_BLK_SIZE;	
	return ptr_blk_arr[ps]+(md*sizeof(data_ptr)+sizeof(seq_blk_hdr));
}
 tuple sorted_sequence::get(const iterator& it)
 {
	 return get(it.pos);
 }
 tuple sorted_sequence::get(int pos)
 {
	 tuple t(1);
	 int ps=pos/PTR_BLK_SIZE;
	 int md=pos % PTR_BLK_SIZE;
	 CHECKP(ptr_blk_arr[ps]);
	 xptr val=((data_ptr*)XADDR((ptr_blk_arr[ps]+(md*sizeof(data_ptr)+sizeof(seq_blk_hdr)))))->value;
	 shft sz=((data_ptr*)XADDR((ptr_blk_arr[ps]+(md*sizeof(data_ptr)+sizeof(seq_blk_hdr)))))->size;
	 if (GET_FREE_SPACE(val)<sz)
	 {
		 CHECKP(val);
		 xptr v2=((seq_blk_hdr*)XADDR(BLOCKXPTR(val)))->nblk;
		 deserialize2FN(t,val,GET_FREE_SPACE(val),v2,Udata);
	 }
	 else
		 deserializeFN(t,val,Udata);
	 return t;
 }
 void sorted_sequence::clear()
 {
	 //1. empty all blocks
	 xptr res_seq;
	 if (bblk_in_chain!=XNULL)
			res_seq=ptr_blk_arr[0]+sizeof(seq_blk_hdr);
	 else
	 {
		 if (!sorted_seqs_arr.size())
		 {
			  bblk_in_chain=XNULL;
			  blk_cnt=0;	
			  sorted_seqs_arr.clear();
			  finalized=false;
			  return;
		 }
		 res_seq=sorted_seqs_arr.back().first+sizeof(seq_blk_hdr);
		 sorted_seqs_arr.pop_back();	
	 }
	 while (true)
	 {
		 
		 while (res_seq!=XNULL)		
			 set_next_ptr_with_free(res_seq);
		 if (sorted_seqs_arr.size())
		 {
			 res_seq=sorted_seqs_arr.back().first+sizeof(seq_blk_hdr);
			 sorted_seqs_arr.pop_back();
		 }
		 else
			 break;
	 }
	 //2. init vars
	 bblk_in_chain=XNULL;
	 blk_cnt=0;	
	 sorted_seqs_arr.clear();
	 ptr_blk_arr.clear();
	 finalized=false;
	 if (merge_tree!=NULL)
	 {
		 pers_sset<merge_cell,unsigned short>::pers_sset_entry* tmp=merge_tree->rb_minimum(merge_tree->root);
		 while (tmp!=NULL)
		 {
			 scm_free(tmp->obj,false);
			 tmp=merge_tree->rb_successor(tmp);
		 }
		 pers_sset<merge_cell,unsigned short>::sset_free(merge_tree);
		 merge_tree=NULL;
		 top=NULL;
	 }

 }

 void sorted_sequence::get(tuple& t, int pos)
 {
	 int ps=pos/PTR_BLK_SIZE;
	 int md=pos % PTR_BLK_SIZE;
	 CHECKP(ptr_blk_arr[ps]);
	 xptr val=((data_ptr*)XADDR((ptr_blk_arr[ps]+(md*sizeof(data_ptr)+sizeof(seq_blk_hdr)))))->value;
	 shft sz=((data_ptr*)XADDR((ptr_blk_arr[ps]+(md*sizeof(data_ptr)+sizeof(seq_blk_hdr)))))->size;
	 if (GET_FREE_SPACE(val)<sz)
	 {
		 CHECKP(val);
		 xptr v2=((seq_blk_hdr*)XADDR(BLOCKXPTR(val)))->nblk;
		deserialize2FN(t,val,GET_FREE_SPACE(val),v2,Udata);
	 }
	 else
		 deserializeFN(t,val,Udata);
 }