/*
* File:  pstr.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <iostream>

#include "common/sedna.h"
#include "common/xptr/sm_vmm_data.h"
#include "common/errdbg/d_printf.h"

#include "tr/pstr/pstrblk.h"
#include "tr/pstr/pstr.h"
#include "tr/pstr/utl.h"
#include "tr/pstr/hh.h"
#include "tr/vmm/vmm.h"

#include "tr/structures/descriptor.h"
#include "tr/structures/nodeutils.h"

using namespace std;

const shft	PSTRBLK_HEADER_SIZE = sizeof(struct vmm_sm_blk_hdr) + 6*sizeof(shft) + HHMAXSIZE*sizeof(hh_slot);
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
    }
    else
        vmm_alloc_tmp_block(&result);
    VMM_SIGNAL_MODIFICATION(result);
    //d_printf1("bm\n");fflush(stdout);
    pstr_blk_markup(result);
    //d_printf1("am\n");fflush(stdout);
    return result;
}


/*	Make initial markup of pstr block.
Assume block is in-memory
*/
void pstr_blk_markup(xptr blk) {
    /* set static fields */
    *(shft*)PSTRNUM_ADDR(blk) = 0;
    *(shft*)SITB_ADDR(blk)    = PAGE_SIZE-sizeof(shft);
    *(shft*)SITH_ADDR(blk)    = PSTR_EMPTY_SLOT;                  /* no free slots - the SIT is empty */
    *(shft*)SSB_ADDR(blk)     = SS_ADDR(blk)-(char*)XADDR(blk);
    *(shft*)BFS_ADDR(blk)     = PSTRMAXSIZE+sizeof(shft);         /* one slot is registered in SIT */
    *(shft*)HHSIZE_ADDR(blk)  = 0;
}

/* checks if the string of given size will fit into block */
bool	pstr_fit_into_blk(xptr blk, shft s_size) {
    CHECKP(blk);

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
    d_printf2(" Block #0x%x\n", XADDR(blk));
    d_printf1("============================================\n");
    d_printf3("PSTRNUM[0x%x]=%d\n", PSTRNUM_ADDR(blk), (int)PSTRNUM(blk));
    d_printf3("SITB[0x%x]=%d\n", SITB_ADDR(blk), (int)SITB(blk));
    d_printf3("SITH[0x%x]=%d\n", SITH_ADDR(blk), (int)SITH(blk));
    d_printf3("SSB[0x%x]=%d\n", SSB_ADDR(blk), (int)SSB(blk));
    d_printf3("BFS[0x%x]=%d\n", BFS_ADDR(blk), (int)BFS(blk));
    d_printf3("HHSIZE[0x%x]=%d\n", HHSIZE_ADDR(blk), (int)HHSIZE(blk));
    d_printf1("----------------------\n");
    /*int i;
    for(i=0; i<HHSIZE(blk); i++) {
    hh_slot* s = (hh_slot*)HH_ADDR(blk, i);
    d_printf4("HOLE[%d]=(%d, %d)\n", i, (int)(s->hole_shft), (int)(s->hole_size));
    }

    i=0;
    shft sit = SITB(blk);
    while (sit != PAGE_SIZE-sizeof(shft)) {
    // the highest slot is not-used, reserved
    sit+=sizeof(shft);
    char* tmp = (char*)XADDR(blk)+sit;
    d_printf3("SIT[%d]=%d\n", i, (int)*(shft*)((char*)XADDR(blk)+sit));
    i++;
    }
    d_printf1("============================================\n");
    */
}

inline static
void setNodePstrData(xptr node, xptr textptr, strsize_t textsize)
{
    WRITEP(node);
    internal::node_text_t * nodetext = getTextFromAnyNode(node);
    U_ASSERT(textsize <= PSTRMAXSIZE);
    U_ASSERT(textsize > internal::maxDescriptorTextSize);
    memcpy(nodetext->data, &textptr, sizeof(xptr));
    nodetext->size = (uint16_t) textsize;
}

inline static
void getNodePstrData(xptr node, xptr &textptr, size_t &textsize)
{
    CHECKP(node);
    internal::node_text_t * nodetext = getTextFromAnyNode(node);
    textsize = nodetext->size;
    U_ASSERT(textsize <= PSTRMAXSIZE);
    U_ASSERT(textsize > internal::maxDescriptorTextSize);
    memcpy(&textptr, nodetext->data, sizeof(xptr));
}

inline static
void clearPstr(xptr node)
{
    WRITEP(node);
    getTextFromAnyNode(node)->size = 0;
}

inline static
bool isPstr(xptr node)
{
    uint16_t size = getTextFromAnyNode(node)->size;
    return (size <= PSTRMAXSIZE) && (size > internal::maxDescriptorTextSize);
}

xptr pstr_allocate(xptr blk, xptr node, const char* s, int s_size) {

    xptr result = pstr_do_allocate(blk, s, s_size);

    if (result == XNULL) {
        result = pstr_migrate(blk, node, s, s_size);
    }

    setNodePstrData(node, result, s_size);

    return result;
}

xptr pstr_do_allocate(xptr blk, const char* s, int s_size) {
    int		hole_size;
    xptr	result=XNULL;

#ifndef PSTR_NO_CHECKP
    CHECKP(blk);
#endif

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

    hole_size = hh_maxhole_size(blk);
    VMM_SIGNAL_MODIFICATION(blk);
    /* Check if to insert into maxhole or into SS tail */
    if (hole_size < s_size) {
        /* Check if its possible to insert into SS tail without defragmentation */
        int lp=(int)SITB(blk) - (int)SSB(blk);
        if (  lp < (int)(s_size) ) {
            pstr_defragment(blk);
        }
        result = pstr_insert_into_tail(blk,s,s_size);
    } else {
        if ( (!SITH(blk))&&((int)SITB(blk) - (int)SSB(blk) < (int)sizeof(shft)))
        {
            pstr_defragment(blk);
            result = pstr_insert_into_tail(blk,s,s_size);
        }
        else
            result = pstr_insert_into_maxhole(blk,s,s_size);
    }

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
    if ((int)SITB(blk) - (int)SSB(blk) < s_size) {
        //pstr_print_blk(blk);
        throw SYSTEM_EXCEPTION("[pstr_insert_into_tail()] free space in SS tail is smaller then the size of string to insert");
    }
    char *dest = (char*)XADDR(blk) + SSB(blk);
    memcpy(dest, s, s_size); /* copy string */
    //shft s2 = SITH(blk);
    if (SITH(blk) != PSTR_EMPTY_SLOT) {
        /* there are free slots in SIT */
        sh = (shft*)((char*)XADDR(blk) + SITH(blk));
        *(shft*)SITH_ADDR(blk) = *sh;					/* set new head of free SIT slots list */
    } else {
        /* no free slots in SIT - allocate new one */
        sh = (shft*)((char*)XADDR(blk) + SITB(blk));
        (*(shft*)SITB_ADDR(blk))-=sizeof(shft);			/* move SIT bound */
        occupied_space+=sizeof(shft);
    }
    /* now 'sh' points to SIT slot where to register new string */
    *sh = *(shft*)SSB_ADDR(blk);
    result = ADDR2XPTR(sh);
    /* update block metastructures */
    *(shft*)SSB_ADDR(blk)+=s_size;				/* move SSB bound */
    *(shft*)BFS_ADDR(blk)-=occupied_space;		/* decrement amount of block free space in BFS */
    *(shft*)PSTRNUM_ADDR(blk)+=1;				/* count number of strings in block */

    RECOVERY_CRASH;
    return result;
}

/* insert string into maxhole */
xptr pstr_insert_into_maxhole(xptr blk, const char* s, int s_size) {
    /* maxhole is the first hole */
    hh_slot h = *(hh_slot*)HH_ADDR(blk, 0);
    xptr	result;
    shft*	sh;
    int		occupied_space=s_size;
#ifndef PSTR_NO_CHECKP
    CHECKP(blk);
#endif
    /* this must hold if internal layout of block is correct */
    if (h.hole_size < s_size)
        throw SYSTEM_EXCEPTION("[pstr_insert_into_maxhole()] maxhole is smaller than the size of string to insert");
    memcpy((char*)XADDR(blk)+h.hole_shft, s, s_size); /* copy string into hole */
    if (SITH(blk)) {
        /* there are free slots in SIT */
        sh = (shft*)((char*)XADDR(blk) + SITH(blk));
        *(shft*)SITH_ADDR(blk) = *sh;					/* set new head of free SIT slots list */
    } else {
        /* no free slots in SIT - allocate new one */
        sh = (shft*)((char*)XADDR(blk) + SITB(blk));
        (*(shft*)SITB_ADDR(blk))-=sizeof(shft);			/* move SIT bound */
        occupied_space+=sizeof(shft);
    }
    /* now 'sh' points to SIT slot where to register new string */
    *sh = h.hole_shft;
    result = ADDR2XPTR(sh);
    /* update block metastructures */
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
    RECOVERY_CRASH;
    return result;
}

/* Replace text content of text descriptor with new string */
xptr	pstr_modify(xptr node, char* s, int s_size) {
#ifndef PSTR_NO_CHECKP
    CHECKP(node);
#endif
    xptr result;

    xptr data;
    size_t size;

    getNodePstrData(node, data, size);

    pstr_do_deallocate(BLOCKXPTR(data), data, size, false);
    result = pstr_allocate(BLOCKXPTR(data), node, s, s_size);
    return result;
}

void pstr_deallocate(xptr node) {
#ifndef PSTR_NO_CHECKP
    CHECKP(node);
#endif

    xptr data;
    size_t size;

    getNodePstrData(node, data, size);

    pstr_do_deallocate(BLOCKXPTR(data), data, size, true);

    /* Update descriptor */
    clearPstr(node);
}

bool pstr_do_deallocate(xptr blk, xptr ps, int s_size, bool drop_empty_block)
{
    bool	adjacent_with_ss_tail = false;
#ifndef PSTR_NO_CHECKP
    CHECKP(blk);
#endif
    VMM_SIGNAL_MODIFICATION(blk);

    /* Check if we have enough slots in HH */

    if ((HHSIZE(blk) >= HHMAXSIZE)) {
        pstr_defragment(blk);
    }

    shft ps_shft = *(shft*)XADDR(ps);

    /* Check if ps_shft slot contains non-empty shft */
    if (ps_shft == PSTR_EMPTY_SLOT||(ps_shft+s_size > SSB(blk)))
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
    *(shft*)XADDR(ps) = SITH(blk);
    *(shft*)SITH_ADDR(blk) = (char*)XADDR(ps) - (char*)XADDR(blk);
    /* update counter of free space in block */
    *(shft*)BFS_ADDR(blk) += s_size;
    *(shft*)PSTRNUM_ADDR(blk)-=1;				/* count number of strings in block */
    /*if (HHSIZE(blk) != 0)
    pstr_print_blk(blk);*/
    if (PSTRNUM(blk) <= 0 && drop_empty_block ) {
        /* drop pstr block once it becomes empty */
        RECOVERY_CRASH;
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
        //	if ((int)blk.addr==0x4acc0000)
        //	check_blk_consistency(blk);
    }
    RECOVERY_CRASH;
    return false;
}


void pstr_defragment(xptr blk) {
    char    buf[PAGE_SIZE];
    sort_item * last=NULL;

    shft frag_shft=0;	/* how far to move to the left next fragment of data */
    shft frag_src=0;	/* fragment begining */
    shft frag_size=0;	/* fragment size */

#ifndef PSTR_NO_CHECKP
    CHECKP(blk);
#endif
    /* mark the block as modified */
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
            frag_src = 0;
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

    RECOVERY_CRASH;

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

xptr debug_migrate_blk = XNULL;

xptr pstr_migrate(xptr blk, xptr node, const char* s, int s_size) {

    xptr	rbd = getRightmostDescriptorWithPstrInThisBlock(blk, node);
    xptr	next_node = getLeftmostDescriptorWithPstrInThisBlock(blk, node);
    xptr	result=XNULL;
    char	next_s[PSTRMAXSIZE];
    size_t	next_s_size=0;
    xptr	next_s_xptr=XNULL;
    xptr	new_blk=XNULL;
    xptr	tmp;
    bool	first_iteration=true;
#ifndef PSTR_NO_CHECKP
    CHECKP(blk);
#endif
    bool is_data_block=IS_DATA_BLOCK(blk);
    /* if new node is the last one in the sequence of descriptors, just allocate new string in new block */
    if (rbd == node) {
        new_blk = pstr_create_blk(is_data_block);
        return pstr_do_allocate(new_blk, s, s_size);
    }

    /* run through descriptors from lbd to rbd, distributing their string contents into
    new pstr blocks */
    do {
        /* omit getNextDescriptorOfSameSort() call for the lbd node */
        if (!first_iteration)
            next_node = getNextDescriptorOfSameSort(next_node);
        else
            first_iteration=false;

        U_ASSERT(next_node != XNULL);

        CHECKP(next_node);
        /* skip descriptors without pstr data except "node" descriptor */
        if ((next_node != node) && !isPstr(next_node)) { continue; }

        VMM_SIGNAL_MODIFICATION(next_node);

        /* if this is new descriptor */
        if (next_node == node) {
            /* if the string fits into initial block, it means that some strings were transfered
            from it. In this case allocate new string in initial block and mark block modified.
            Otherwise allocate new string in new block */
            //d_printf1("next_node == node ");fflush(stdout);
            if (pstr_fit_into_blk(blk, s_size)) {
                return pstr_do_allocate(blk, s, s_size);
            } else {
                new_blk = pstr_create_blk(is_data_block);
                return pstr_do_allocate(new_blk, s, s_size);
            }
        }
        /* create new block if not created yet */
        if (new_blk == XNULL)
            new_blk = pstr_create_blk(is_data_block);

        /* read next_node string contents and deallocate them from old block */

        getNodePstrData(next_node, next_s_xptr, next_s_size);
        pstr_read_from_node(next_node,  next_s);
        pstr_do_deallocate(blk, next_s_xptr, next_s_size, false);

        tmp = pstr_do_allocate(new_blk, next_s, next_s_size);
        if (tmp == XNULL)
            throw SYSTEM_EXCEPTION("[pstr_migrate()] existing pstr from old block didn't fit into new block");

        setNodePstrData(next_node, tmp, next_s_size);
    } while (next_node != rbd);

    /* never should come here */
    throw SYSTEM_EXCEPTION("[pstr_migrate()] didn't meet new descriptor node");
}

/* read contents (pstr) of the text node */
void pstr_read_from_node(xptr node, char* the_s) {
    U_ASSERT(isPstr(node));
    size_t ps_size = (size_t) nodeGetTextSize(getTextFromAnyNode(node));
    xptr ps = nodeGetTextPointer(getTextFromAnyNode(node));
    if (ps == XNULL)
        throw SYSTEM_EXCEPTION("[pstr_read_from_node()] the target node has XNULL pointer to it's pstr content");
    CHECKP(ps);
    memcpy(the_s, (char*) XADDR(ps), ps_size);
}

void pstr_read(xptr ps, int ps_size, char* the_s) {
    if (ps == XNULL) { return; }
    CHECKP(ps);
    memcpy(the_s, (char*) XADDR(PSTRDEREF(ps)), ps_size);
}

bool is_last_shft_in_blk(xptr addr)
{
    xptr sh=addr;
    CHECKP(sh);
    sh=sh+sizeof(shft);
    xptr blk=BLOCKXPTR(addr);
    CHECKP(blk);
    while (((uintptr_t)XADDR(sh)-(uintptr_t)XADDR(blk))<=PAGE_SIZE-sizeof(shft))
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

    sort_item* sorted_sit    = utl_sort_sit(blk);
    sort_item* sorted_hh     = utl_sort_hh(blk);
    sort_item* sorted_hh_sit = utl_merge(sorted_sit, sorted_hh);

    if (sorted_hh_sit==NULL) return;
    while (((char*)sorted_hh_sit->next_item)!=NULL)
    {
        sorted_hh_sit=sorted_hh_sit->next_item;
    }
    if (sorted_hh_sit->item_type == ITEM_HOLE)
        throw SYSTEM_EXCEPTION("wrong place for hole in block");
    if (sorted_hh_sit->sort_value>=SSB(blk))
        throw SYSTEM_EXCEPTION("wrong position of data in block");
}
