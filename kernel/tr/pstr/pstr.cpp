/*
 * File:  pstr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>
#include "sm_vmm_data.h"
#include "pstrblk.h"
#include "pstr.h"
#include "utl.h"
#include "hh.h"
#include "vmm.h"
#include "node_utils.h"
#include "locks.h"
#include "log.h"
using namespace std;

const shft	PSTRBLK_HEADER_SIZE = sizeof(struct vmm_sm_blk_hdr) + 5*sizeof(shft) + HHMAXSIZE*sizeof(hh_slot);
/* the maximum string storable in our block - the case of single sting occupying whole block */
const shft	PSTRMAXSIZE = PAGE_SIZE - PSTRBLK_HEADER_SIZE - sizeof(shft);

//#define PSTR_NO_CHECKP
void checkPointer(xptr blk, char* ptr, int size)
{
	CHECKP(blk);
	char * blk_ptr= (char*)XADDR(blk);
	if (ptr<blk_ptr || (ptr+size)>(blk_ptr+PAGE_SIZE))
	{
	 throw  SYSTEM_EXCEPTION("fff");
	}

}
/* create new pstr block */
xptr pstr_create_blk(bool persistent) {
	xptr result;
	if (persistent) 
	{
		vmm_alloc_data_block(&result);
		//PHYS LOG
		hl_phys_log_create_node_blk(XADDR(result));
	}
	else
		vmm_alloc_tmp_block(&result);
	VMM_SIGNAL_MODIFICATION(result);
    //printf("bm\n");fflush(stdout);
	pstr_blk_markup(result);
    //printf("am\n");fflush(stdout);
	return result;
}



/*	Make initial markup of pstr block.
	Assume block is in-memory 
 */
void pstr_blk_markup(xptr blk) {
	/* set static fields */
	*(shft*)PSTRNUM_ADDR(blk) = 0;
	*(shft*)SITB_ADDR(blk) = PAGE_SIZE-sizeof(shft);
	*(shft*)SITH_ADDR(blk) = PSTR_EMPTY_SLOT;				/* no free slots - the SIT is empty */
	*(shft*)SSB_ADDR(blk) = SS_ADDR(blk)-(char*)XADDR(blk);
	*(shft*)BFS_ADDR(blk) = PSTRMAXSIZE;					/* one slot is registered in SIT */
	*(shft*)HHSIZE_ADDR(blk) = 0;
}

/* checks if the string of given size will fit into block */
bool	pstr_fit_into_blk(xptr blk, shft s_size) {
#ifndef PSTR_NO_CHECKP
	CHECKP(blk);
#endif
	if(BFS(blk) >= s_size + sizeof(shft))
		return true;
	else
		return false;
}

void pstr_print_blk(xptr blk) {
#ifndef PSTR_NO_CHECKP
CHECKP(blk);
#endif
	if (blk == XNULL) return;
	cout << " Block #"<< XADDR(blk) << endl;
	cout << "============================================" << endl;
	cout << "PSTRNUM[" << (int)PSTRNUM_ADDR(blk) <<"]=" << PSTRNUM(blk) << endl;
	cout << "SITB[" << (int)SITB_ADDR(blk) <<"]=" << SITB(blk) << endl;
	cout << "SITH[" << (int)SITH_ADDR(blk) <<"]=" << SITH(blk) << endl;
	cout << "SSB[" << (int)SSB_ADDR(blk) <<"]=" << SSB(blk) << endl;
	cout << "BFS[" << (int)BFS_ADDR(blk) <<"]=" << BFS(blk) << endl;
	cout << "HHSIZE[" << (int)HHSIZE_ADDR(blk) <<"]=" << HHSIZE(blk) << endl;
	cout << "----------------------"<< endl;
	int i;
	/*for(i=0; i<HHSIZE(blk); i++) {
		hh_slot* s = (hh_slot*)HH_ADDR(blk, i);
		cout <<"HOLE[" << i << "]=(" <<s->hole_shft <<", " <<s->hole_size <<")" << endl;
	} 

	i=0;
	shft sit = SITB(blk);
	while (sit != PAGE_SIZE-sizeof(shft)) {
		// the highest slot is not-used, reserved
		sit+=sizeof(shft);
		char* tmp = (char*)XADDR(blk)+sit;
		cout <<"SIT[" << i <<"]="<< *(shft*)((char*)XADDR(blk)+sit) << endl;
		i++;
	}
	cout << "============================================" << endl;
	*/

}

xptr pstr_allocate(xptr blk, xptr node, const char* s, int s_size) {
	
	xptr result = pstr_do_allocate(blk, s, s_size);
    //printf("da");fflush(stdout);
	if (result == XNULL)
    {
        //printf("bmigr");fflush(stdout);
		result = pstr_migrate(blk, node, s, s_size);
        //printf("amigr");fflush(stdout);
    }
#ifndef PSTR_NO_CHECKP
	CHECKP(node);
#endif
	VMM_SIGNAL_MODIFICATION(node);
	//PHYS LOG
	if (IS_DATA_BLOCK(node)) 
	{
		hl_phys_log_change(&((t_dsc*)XADDR(node))->data,sizeof(xptr));
		hl_phys_log_change(&((t_dsc*)XADDR(node))->size,sizeof(xptr));
	}
	((t_dsc*)XADDR(node))->data=result;
	((t_dsc*)XADDR(node))->size=s_size;
	
	
	///TEMP!!!!
	/*	shft hh_size=HHSIZE(blk);
		for (int i=0; i<hh_size; i++) 
		{
			hh_slot* tmp = (hh_slot*)HH_ADDR(blk, i);
			if (tmp->hole_shft+ tmp->hole_size==SSB(blk) ) 
			{
				throw SYSTEM_EXCEPTION("[pstr_deallocate()] string can not be adjacent with with SS tail and with some hole on the right simultaneously");
			}
		}
	*/
		
	return result;
}

xptr pstr_do_allocate(xptr blk, const char* s, int s_size) {
	int		hole_size;
	xptr	result=XNULL;

#ifndef PSTR_NO_CHECKP
CHECKP(blk);
#endif
	//check_blk_consistency(blk);
	/* */
	if (s_size <= 0)
		throw SYSTEM_EXCEPTION("trying to allocate string with size <= 0");
	/* Check if the string exceeds PSTRMAXSIZE */
	if (s_size > PSTRMAXSIZE)
		throw USER_EXCEPTION(2024);
	/* Check if the string fits into block at all */
	if (!pstr_fit_into_blk(blk, s_size)) {
			return result;
	}
	shft s1 = BFS(blk);
	/* Check if we have enough slots in HH */
	/* not needed */
	/*if (HHSIZE(blk) >= HHMAXSIZE) {	
		pstr_defragment(blk);
	}*/
	hole_size = hh_maxhole_size(blk);
	VMM_SIGNAL_MODIFICATION(blk);
	/* Check if to insert into maxhole or into SS tail */
	if (hole_size < s_size) {
		/* Check if its possible to insert into SS tail without defragmentation */
		if (  SITB(blk) - SSB(blk) < (s_size+ (SITH(blk)?0:sizeof(shft)) ) ) {
			pstr_defragment(blk);
		}
		result = pstr_insert_into_tail(blk,s,s_size);
	} else {
		if ( (!SITH(blk))&&(SITB(blk) - SSB(blk) < sizeof(shft))) 
		{
			pstr_defragment(blk);
			result = pstr_insert_into_tail(blk,s,s_size);
		}
		else
			result = pstr_insert_into_maxhole(blk,s,s_size);
	}
	//check_blk_consistency(result);
	return result;
}

/* inserts string into SS tail */
xptr pstr_insert_into_tail(xptr blk, const char* s, int s_size) {
	xptr	result;
	shft*	sh;
	int		occupied_space=s_size;

#ifndef PSTR_NO_CHECKP
CHECKP(blk);
#endif
	/* this must hold if internal layout of block is correct */
	shft s1 = SITB(blk);
	shft s2 = SSB(blk);
	shft s3 = BFS(blk);
	if (SITB(blk) - SSB(blk) < s_size) {
		//pstr_print_blk(blk);
		throw SYSTEM_EXCEPTION("[pstr_insert_into_tail()] free space in SS tail is smaller then the size of string to insert");
	}
	char* debug1 = (char*)XADDR(blk) + SSB(blk);
    char *dest = (char*)XADDR(blk) + SSB(blk);
	//PHYS LOG 
	if (IS_DATA_BLOCK(blk))
			hl_phys_log_change(dest,s_size);
	memcpy(dest, s, s_size); /* copy string */
	//shft s2 = SITH(blk);
	if (SITH(blk) != PSTR_EMPTY_SLOT) {
		/* there are free slots in SIT */
		sh = (shft*)((char*)XADDR(blk) + SITH(blk));
		//PHYS LOG 
		if (IS_DATA_BLOCK(blk))
			hl_phys_log_change(SITH_ADDR(blk),sizeof(shft));
		*(shft*)SITH_ADDR(blk) = *sh;					/* set new head of free SIT slots list */
	//	shft s1 = SITH(blk);
	} else {
		/* no free slots in SIT - allocate new one */
		sh = (shft*)((char*)XADDR(blk) + SITB(blk));
		//PHYS LOG 
		if (IS_DATA_BLOCK(blk))
			hl_phys_log_change(SITB_ADDR(blk),sizeof(shft));
		(*(shft*)SITB_ADDR(blk))-=sizeof(shft);			/* move SIT bound */
		occupied_space+=sizeof(shft);
	}
	/* now 'sh' points to SIT slot where to register new string */
	//PHYS LOG 
	if (IS_DATA_BLOCK(blk))
		hl_phys_log_change(sh,sizeof(shft));
	*sh = *(shft*)SSB_ADDR(blk);
	result = ADDR2XPTR(sh);
	/* update block metastructures */
	if (IS_DATA_BLOCK(blk))
	{
		hl_phys_log_change(SSB_ADDR(blk),sizeof(shft));
		hl_phys_log_change(BFS_ADDR(blk),sizeof(shft));
		hl_phys_log_change(PSTRNUM_ADDR(blk),sizeof(shft));
	}
	*(shft*)SSB_ADDR(blk)+=s_size;				/* move SSB bound */
	*(shft*)BFS_ADDR(blk)-=occupied_space;		/* decrement amount of block free space in BFS */
	*(shft*)PSTRNUM_ADDR(blk)+=1;				/* count number of strings in block */
	//pstr_print_blk(blk);
	/*if (debug3 != *((char*)blk.addr-1))
		throw PSTRException("debug");*/
	/*if (debug4 != *((char*)blk.addr+PAGE_SIZE))
		throw PSTRException("debug");*/
	return result;
}

/* insert string into maxhole */
xptr pstr_insert_into_maxhole(xptr blk, const char* s, int s_size) {
	/* maxhole is the first hole */
	hh_slot h = *(hh_slot*)HH_ADDR(blk, 0);
	xptr	result;
	shft*	sitb;
	shft*	sh;
	int		occupied_space=s_size;
#ifndef PSTR_NO_CHECKP
CHECKP(blk);
#endif
	/* this must hold if internal layout of block is correct */
	if (h.hole_size < s_size) 
		throw SYSTEM_EXCEPTION("[pstr_insert_into_maxhole()] maxhole is smaller than the size of string to insert");
	//PHYS LOG 
	if (IS_DATA_BLOCK(blk))
			hl_phys_log_change((char*)XADDR(blk)+h.hole_shft,s_size);
	memcpy((char*)XADDR(blk)+h.hole_shft, s, s_size); /* copy string into hole */
	if (SITH(blk)) {
		/* there are free slots in SIT */
		sh = (shft*)((char*)XADDR(blk) + SITH(blk));
		//PHYS LOG 
		if (IS_DATA_BLOCK(blk))
			hl_phys_log_change(SITH_ADDR(blk),sizeof(shft));
		*(shft*)SITH_ADDR(blk) = *sh;					/* set new head of free SIT slots list */
	} else {
		/* no free slots in SIT - allocate new one */
		sh = (shft*)((char*)XADDR(blk) + SITB(blk));
		//PHYS LOG 
		if (IS_DATA_BLOCK(blk))
			hl_phys_log_change(SITB_ADDR(blk),sizeof(shft));
		(*(shft*)SITB_ADDR(blk))-=sizeof(shft);			/* move SIT bound */
		occupied_space+=sizeof(shft);
	}
	/* now 'sh' points to SIT slot where to register new string */
	//PHYS LOG 
	if (IS_DATA_BLOCK(blk))
		hl_phys_log_change(sh,sizeof(shft));
	*sh = h.hole_shft;
	result = ADDR2XPTR(sh);
	/* update block metastructures */
	//PHYS LOG 
	if (IS_DATA_BLOCK(blk))
	{
		hl_phys_log_change(BFS_ADDR(blk),sizeof(shft));
		hl_phys_log_change(PSTRNUM_ADDR(blk),sizeof(shft));
	}
	*(shft*)BFS_ADDR(blk)-=occupied_space;
	*(shft*)PSTRNUM_ADDR(blk)+=1;				/* count number of strings in block */
	/* replace old hole with new */
	hh_remove_max(blk);
	if (h.hole_size != s_size) {
		/* prepare new hole and insert it into HH */
		h.hole_shft +=s_size;
		h.hole_size -=s_size;
		hh_insert(blk, h);
	}
	return result;
}

/* Replace text content of text descriptor with new string */
xptr	pstr_modify(xptr node, char* s, int s_size) {
#ifndef PSTR_NO_CHECKP
CHECKP(node);
#endif
	xptr result;
	xptr data=((t_dsc*)XADDR(node))->data;
	int	size = ((t_dsc*)XADDR(node))->size;
	pstr_do_deallocate(BLOCKXPTR(data), data, size, false);
    //printf("dd");fflush(stdout);
	result = pstr_allocate(BLOCKXPTR(data), node, s, s_size);
    //printf("al");fflush(stdout);
	return result;
}

void pstr_deallocate(xptr node) {
#ifndef PSTR_NO_CHECKP
//CHECKP(node);
#endif
	xptr	ps = ((t_dsc*)XADDR(node))->data;
	int		s_size = ((t_dsc*)XADDR(node))->size;
	xptr	blk = BLOCKXPTR(ps);
	pstr_do_deallocate(blk, ps, s_size, true);

	/* Update descriptor */
#ifndef PSTR_NO_CHECKP
	CHECKP(node);
#endif
	((t_dsc*)XADDR(node))->data = XNULL;
	((t_dsc*)XADDR(node))->size = 0;
}

bool pstr_do_deallocate(xptr blk, xptr ps, int s_size, bool drop_empty_block) 
{
	bool	adjacent_with_ss_tail = false;
#ifndef PSTR_NO_CHECKP
	CHECKP(blk);
#endif
//	check_blk_consistency(blk);
	VMM_SIGNAL_MODIFICATION(blk);
	//pstr_print_blk(blk);
	/* Check if we have enough slots in HH */
	shft tmp = HHSIZE(blk);
	if ((HHSIZE(blk) >= HHMAXSIZE)) {	
		pstr_defragment(blk);
		//return false;
	}

	shft	ps_shft = *(shft*)XADDR(ps);

	/* Check if ps_shft slot contains non-empty shft */
	if (ps_shft == PSTR_EMPTY_SLOT) 
		throw SYSTEM_EXCEPTION("[pstr_deallocate()] string to be deallocated occupies PSTR_EMPTY_SLOT");
	
	/* Check if the string is in SS tail (last) */
	if (ps_shft+s_size == SSB(blk))
		adjacent_with_ss_tail = true;
	/* Check if there is adjacend hole for ps */
	shft hh_size=HHSIZE(blk);
	shft mark_coalescence=0;
	for (int i=0; i<hh_size; i++) {
		hh_slot* tmp = (hh_slot*)HH_ADDR(blk, i);
		if (tmp->hole_shft == ps_shft+s_size) {
			/* ps border hole on the right */
			/* this must hold if internal layout of block is correct */
			if (adjacent_with_ss_tail)
				throw SYSTEM_EXCEPTION("[pstr_deallocate()] string can not be adjacent with with SS tail and with some hole on the right simultaneously");
			tmp->hole_size += s_size;
			tmp->hole_shft = ps_shft;
			hh_heapify(blk, 1);
			mark_coalescence=ps_shft;
			goto post_operations;
		} else if (tmp->hole_shft+tmp->hole_size == ps_shft) {
			/* ps border hole on the left */
			if (adjacent_with_ss_tail) {
				/* remove this hole and move the SS boundary */
				/* note: numeration of holes in "hh_" functions starts from index 1 */
				*(shft*)SSB_ADDR(blk) = tmp->hole_shft;
				hh_remove(blk, i+1);
			} else {

				tmp->hole_size += s_size;
				hh_heapify(blk, 1);
				mark_coalescence=ps_shft+s_size;//tmp->hole_shft+tmp->hole_size;
			}
			goto post_operations;
		}
	}
	/* Come here when the string is not adjacent with any hole */
	if (adjacent_with_ss_tail) {
		//PHYS LOG 
		if (IS_DATA_BLOCK(blk))
			hl_phys_log_change(SSB_ADDR(blk),sizeof(shft));
		*(shft*)SSB_ADDR(blk) = ps_shft;
		goto post_operations;
	}
	/* Come here when the string is not adjacent with any hole or SS tail - create new hole */
	hh_slot h;
	h.hole_shft=ps_shft;
	h.hole_size=s_size;
	hh_insert(blk, h);

post_operations:
	/* update block metastructures */
	/* put the SIT slot that was occupied by deleted string into list of free slots */
	//PHYS LOG 
	if (mark_coalescence)
	{
		int left_pos=-1;
		int right_pos=-1;
		hh_size=HHSIZE(blk);
		for (int i=0; i<hh_size; i++) 
		{
			hh_slot* tmp = (hh_slot*)HH_ADDR(blk, i);
			if (tmp->hole_shft == mark_coalescence)
				right_pos=i;
			if (tmp->hole_shft+tmp->hole_size == mark_coalescence)
				left_pos=i;
			if (left_pos>-1 && right_pos>-1) break;
		}
		if (left_pos>-1 && right_pos>-1)
		{
			hh_slot* tmp = (hh_slot*)HH_ADDR(blk, left_pos);
			hh_slot* tmp2 = (hh_slot*)HH_ADDR(blk, right_pos);
			tmp->hole_size+=tmp2->hole_size;
			hh_remove(blk, right_pos+1);
			hh_heapify(blk,1);
		}
	}
	if (IS_DATA_BLOCK(blk))
	{
		hl_phys_log_change(XADDR(ps),sizeof(shft));
		hl_phys_log_change(SITH_ADDR(blk),sizeof(shft));
		hl_phys_log_change(BFS_ADDR(blk),sizeof(shft));
		hl_phys_log_change(PSTRNUM_ADDR(blk),sizeof(shft));
	}
	*(shft*)XADDR(ps) = SITH(blk);
	*(shft*)SITH_ADDR(blk) = (char*)XADDR(ps) - (char*)XADDR(blk);
	/* update counter of free space in block */
	*(shft*)BFS_ADDR(blk) += s_size;
	*(shft*)PSTRNUM_ADDR(blk)-=1;				/* count number of strings in block */
	/*if (HHSIZE(blk) != 0)
		pstr_print_blk(blk);*/
	if (PSTRNUM(blk) <= 0 && drop_empty_block ) {
		/* drop pstr block once it becomes empty */
		//PHYS LOG REC
		if (IS_DATA_BLOCK(blk))
			hl_phys_log_change_blk(XADDR(blk));
		vmm_delete_block(blk);
		return true;
	} else {
		/* mark the block as modified */
		//lockWriteXptr(blk);
		///TEMP!!!!
		/*shft hh_size=HHSIZE(blk);
		for (int i=0; i<hh_size; i++) 
		{
			hh_slot* tmp = (hh_slot*)HH_ADDR(blk, i);
			if (tmp->hole_shft+ tmp->hole_size==SSB(blk) ) 
			{
				throw SYSTEM_EXCEPTION("[pstr_deallocate()] string can not be adjacent with with SS tail and with some hole on the right simultaneously");
			}
		}*/
//		check_blk_consistency(blk);
	}
	return false;
}


void pstr_defragment(xptr blk) {
	char		buf[PAGE_SIZE];
	int			hhsize = HHSIZE(blk);
	sort_item*	last=NULL;

	shft		frag_shft=0;	/* how far to move to the left next fragment of data */
	shft		frag_src=NULL;	/* fragment begining */
	shft		frag_size=0;	/* fragment size */

#ifndef PSTR_NO_CHECKP
CHECKP(blk);
#endif
/* mark the block as modified */
	//PHYS LOG
	if (IS_DATA_BLOCK(blk)) 
		hl_phys_log_change_blk(XADDR(blk));
//	pstr_print_blk(blk);
	/* prepare joint list of SIT and HH items sorted by shfts of string/hole beginings */
	sort_item* sorted_sit = utl_sort_sit(blk);
//	utl_print(sorted_sit);
	sort_item* sorted_hh	= utl_sort_hh(blk);
//	utl_print(sorted_hh);
	sort_item* sorted_hh_sit = utl_merge(sorted_sit, sorted_hh);
//	utl_print(sorted_hh_sit);
	if (!sorted_hh_sit) return;

	/* copy blk to tmp block */
	memcpy(buf, XADDR(blk), PAGE_SIZE);
	//memset(buf, 0x00, PAGE_SIZE);

	/* defragment blk */
	/*for (sort_item* it=sorted_hh_sit; it->next_item; it=it->next_item) {
		cout << "type=" << it->item_type << " value=" << it->sort_value<< endl;
	}*/
	/*sort_item*	tmp3=sorted_hh_sit;
    int k=0;
	while(tmp3) {
		cout << "merged:"<<tmp3->sort_value<<endl;
		cout << "actual:"<< *((shft*)(buf+tmp3->item_shft))<<endl;
		cout << "in xptr:"<< *((shft*)((char*)XADDR(blk)+tmp3->item_shft))<<endl;
		tmp3=tmp3->next_item;
		k++;
	}
	cout << "merged " << k << " elements" << endl;*/
	
	for (sort_item* it=sorted_hh_sit; it; it=it->next_item) {
		if (it->item_type == ITEM_HOLE) {
//cout << "this is a hole:" <<it->sort_value<< endl;
			if (frag_src) {
				/* calc size of fragment as diff between begining of the hole and frag_src */
				frag_size = it->sort_value - frag_src;
				/* this must hold if internal layout of block is correct and sorted_hh_sit is correct */
				if (frag_size <= 0)
					throw SYSTEM_EXCEPTION("[pstr_defragment()] fragment size is <= 0");
				/* this is the end of fragment - copy it to tmp block */
				memcpy(buf + frag_src - frag_shft, (char*)XADDR(blk) + frag_src, frag_size);
//cout << "moving from " << frag_src << " to " << (frag_src - frag_shft) << endl;
				/* actualize SIT slots of shifted strings in tmp block */
				sort_item* goback_it = it;
				while (goback_it->prev_item && (goback_it=goback_it->prev_item)->item_type == ITEM_PSTR) {
					/* item_shft field for PSTR item is the shift of PSTR's slot in the SIT */
					*(shft*)(buf + goback_it->item_shft)-=frag_shft;
				}
			}
			frag_src=NULL;
			frag_shft+=((hh_slot*)((char*)XADDR(blk) + it->item_shft))->hole_size;
		} else if (it->item_type == ITEM_PSTR) {
//cout << "this is a fragment:" <<it->sort_value<< endl;
			if (!frag_src)
				/* this is the begining of new fragment */
				frag_src = it->sort_value;
		}
		last=it;
	}
    
   	/* if the last item was HOLE the internal layour of block is incorrect because
	   the hole can not be adjacent with SSB */
	if (!frag_src)
		throw SYSTEM_EXCEPTION("[pstr_deallocate()] the hole can not be adjacent with SSB");
    /* we must copy the last fragment that continues up to SSB */
	frag_size = SSB(blk) - frag_src;
	/* this must hold if internal layout of block is correct and sorted_hh_sit is correct */
	if (frag_size <= 0)
		throw SYSTEM_EXCEPTION("[pstr_defragment()] last fragment size is <= 0");
	memcpy(buf + frag_src - frag_shft, (char*)XADDR(blk) + frag_src, frag_size);
//cout << "moving from" << frag_src << "to " << (frag_src - frag_shft) << endl;
	/* actualize SIT slots of shifted strings in tmp block */
	sort_item* goback_it = last;
	while (goback_it->prev_item && goback_it->item_type == ITEM_PSTR) {
		/* item_shft field for PSTR item is the shift of PSTR's slot in the SIT */
		*(shft*)(buf + goback_it->item_shft)-=frag_shft;
		goback_it=goback_it->prev_item;
	}

	/* copy back tmp block to blk */
	memcpy((char*)XADDR(blk) + sizeof(struct vmm_sm_blk_hdr), buf + sizeof(struct vmm_sm_blk_hdr), PAGE_SIZE-sizeof(struct vmm_sm_blk_hdr));

	/* update block metastructures */
	*(shft*)HHSIZE_ADDR(blk)=0;
	*(shft*)SSB_ADDR(blk)-=frag_shft;
	delete [] sorted_sit;
	delete [] sorted_hh;
	//pstr_print_blk(blk);

}

/*	!!!!! pstr_migrate() IS USED FOR TEXT NODE CONTENTS STORAGE, NOT FOR PREFIXES !!!!! */
/*
	processing of situation when the string can not fit into block. New block(s) is(are) to be 
	allocated, then contents of old block are distributed between 2(or maximum 3) blocks to keep
	the property of string inter-block order in accordance with their descriptor inter-block order.
	The algorithm is as follows:
	1) find the left_bound descriptor (lbd) via micro interface function 
	   getLeftmostDescriptorWithPstrInThisBlock(xptr blk, xptr node) where blk is current
	   pstr block and node is current descriptor to start looking from ("node" parameter of the function)
	2) find the right_bound descriptor (rbd) via micro interface function 
	   getRightmostDescriptorWithPstrInThisBlock(xptr blk, xptr node) where blk is current
	   pstr block and node is current descriptor to start looking from ("node" parameter of the function)
	3) allocate new pstr migrate block where to migrate contents of old block and the string to be
	   newly allocated ("s" parameter of the function)
	4) run through descriptor sequence from lbd to rbd via micro interface function 
	   getNextDescriptorOfSameSort() migrating corresponding string contents from old pstr block into 
	   new pstr block and updating descriptors correspondingly
	5) in case there is no enough space in new pstr block, allocate another new pstr migrate block and 
	   continue the process with this one as a new migrate block (no more than 3 new blocks must be 
	   enough in any case)
	6) drop the old pstr block 
 */

xptr debug_migrate_blk=XNULL;

xptr pstr_migrate(xptr blk, xptr node, const char* s, int s_size) {
/* debug */
/*xptr debug_result;
if (debug_migrate_blk == XNULL)
	debug_migrate_blk = pstr_create_blk(true);
if (!pstr_fit_into_blk(debug_migrate_blk, s_size))
	debug_migrate_blk = pstr_create_blk(true);
debug_result = pstr_do_allocate(debug_migrate_blk, s, s_size);
return debug_result; */
	/* debug */
	xptr	debug=XNULL;
	xptr	debug1=XNULL;
	int		debug_cnt=0;
	char	debug_buf[PAGE_SIZE];

	xptr	rbd = getRightmostDescriptorWithPstrInThisBlock(blk, node);
	xptr	next_node = getLeftmostDescriptorWithPstrInThisBlock(blk, node);
	xptr	result=XNULL;
	char	next_s[PSTRMAXSIZE];
	int		next_s_size=0;
	xptr	next_s_xptr=XNULL;
	xptr	new_blk=XNULL;
	xptr	tmp;
	bool	first_iteration=true;
    //printf("\npstr_migrate\n");fflush(stdout);
#ifndef PSTR_NO_CHECKP
	CHECKP(blk);
#endif	
	bool	is_data_block=IS_DATA_BLOCK(blk);
	/* if new node is the last one in the sequence of descriptors, just allocate new string in new block */
	if (rbd == node) {
        //printf("before pstr_create_blk\n");fflush(stdout);
		new_blk = pstr_create_blk(is_data_block);
        //printf("after pstr_create_blk\n");fflush(stdout);
		return pstr_do_allocate(new_blk, s, s_size);
	}

	/* run through descriptors from lbd to rbd, distributing their string contents into
	   new pstr blocks */
    //printf("cycle\n");
   // node.print();
   // rbd.print();
   // next_node.print();
    fflush(stdout);
	do {
		/* omit getNextDescriptorOfSameSortXptr() call for the lbd node */
		if (!first_iteration) 
			next_node = getNextDescriptorOfSameSortXptr(next_node);
		else
			first_iteration=false;
		/* debug */
		/* if (((t_dsc*)XADDR(next_node))->data!=XNULL)
		{
		 if (debug==XNULL)
			debug = ((t_dsc*)XADDR(next_node))->data;
		 else if (BLOCKXPTR(debug)!=BLOCKXPTR(((t_dsc*)XADDR(next_node))->data))
			throw PSTRException("[pstr_migrate()] descriptors out of the current pstr block");
		}*/
#ifndef PSTR_NO_CHECKP
		CHECKP(next_node);
#endif	
		VMM_SIGNAL_MODIFICATION(next_node);
		/* skip descriptors without pstr data except "node" descriptor */
		if ((((t_dsc*)XADDR(next_node))->data == XNULL) && (next_node != node))
			continue;
		
		/* if this is new descriptor */
		if (next_node == node) {
			/* if the string fits into initial block, it means that some strings were transfered
			   from it. In this case allocate new string in initial block and mark block modified.
			   Otherwise allocate new string in new block */
            //printf("next_node == node ");fflush(stdout);
			if (pstr_fit_into_blk(blk, s_size)) {
				return pstr_do_allocate(blk, s, s_size);
			} else {
				new_blk = pstr_create_blk(is_data_block);
				return pstr_do_allocate(new_blk, s, s_size);
			}
		}

		/* this is old descriptor */
		
		/* create new block if not created yet */
		if (new_blk == XNULL)
			new_blk = pstr_create_blk(is_data_block);
		CHECKP(next_node);
		/* read next_node string contents and deallocate them from old block */
		next_s_size = ((t_dsc*)XADDR(next_node))->size;
		next_s_xptr =  ((t_dsc*)XADDR(next_node))->data;
		pstr_read_from_node(next_node,  next_s);
		pstr_do_deallocate(blk, next_s_xptr, next_s_size, false);
		/* debug */
		/*
		memcpy(debug_buf, next_s, next_s_size);
		debug_buf[next_s_size]='\0';
		*/
		//cout << "[pstr_read_from_node]:"<< debug_buf << endl;
		tmp = pstr_do_allocate(new_blk, next_s, next_s_size);
		if (tmp == XNULL)
			throw SYSTEM_EXCEPTION("[pstr_migrate()] existing pstr from old block didn't fit into new block");
#ifndef PSTR_NO_CHECKP
		CHECKP(next_node);
#endif
		/* update old descriptor */
		VMM_SIGNAL_MODIFICATION(next_node);
		//PHYS LOG
		if (IS_DATA_BLOCK(next_node)) 
			hl_phys_log_change(&((t_dsc*)XADDR(next_node))->data,sizeof(xptr));
		((t_dsc*)XADDR(next_node))->data=tmp;
	} while (next_node != rbd);

	/* never should come here */
	throw SYSTEM_EXCEPTION("[pstr_migrate()] didn't meet new descriptor node");
}

/* read contents (pstr) of the text node */
void pstr_read_from_node(xptr node, char* the_s) {
#ifndef PSTR_NO_CHECKP
//CHECKP(node);
#endif
	xptr ps = ((t_dsc*)XADDR(node))->data;
	if (ps == XNULL)
		throw SYSTEM_EXCEPTION("[pstr_read_from_node()] the target node has XNULL pointer to it's pstr content");
	int ps_size = ((t_dsc*)XADDR(node))->size;
	pstr_read(ps, ps_size, the_s);
}

/* read persistent string returning pointer to buffer, which must be freed */
void	pstr_read(xptr ps, int ps_size, char* the_s) {
	if (ps == XNULL) {
		return;
	}
	xptr blk = BLOCKXPTR(ps);
#ifndef PSTR_NO_CHECKP
CHECKP(blk);
#endif
	if (*(shft*)XADDR(ps)) 
		memcpy(the_s, (char*)XADDR(blk) + *(shft*)XADDR(ps), ps_size);
}
bool is_last_shft_in_blk(xptr addr)
{
 xptr sh=addr;
 CHECKP(sh);
 sh=sh+sizeof(shft);
 xptr blk=BLOCKXPTR(addr);
 CHECKP(blk);
 while (((int)XADDR(sh)-(int)XADDR(blk))<=PAGE_SIZE-sizeof(shft))
 {
	 if ((*(shft*)XADDR(sh))<SSB(blk) &&(*(shft*)XADDR(sh))>0 ) return false;
	 sh=sh+sizeof(shft);
 }
 return true;
}
void check_blk_consistency(xptr addr)
{
	xptr blk=BLOCKXPTR(addr);
	CHECKP(blk);
	shft hh_size=HHSIZE(blk);
	for (int i=0; i<hh_size; i++) 
	{
		hh_slot* tmp = (hh_slot*)HH_ADDR(blk, i);
		if (tmp->hole_shft+ tmp->hole_size==SSB(blk) ) 
		{
			throw SYSTEM_EXCEPTION("[pstr_deallocate()] string can not be adjacent with with SS tail and with some hole on the right simultaneously");
		}
	}

}
