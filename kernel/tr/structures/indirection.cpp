/*
 * File:  indirection.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <string>
#include <set>

#include "common/sedna.h"

#include "tr/structures/indirection.h"
#include "common/u/usem.h"
#include "tr/vmm/vmm.h"
#include "tr/log/log.h"
#include "common/sm_vmm_data.h"
#include "tr/executor/base/xptr_sequence.h"

/*new static variables*/

static std::set<xptr>* blocks_to_delete;
static int rollback_mode = MODE_NORMAL;
static xptr rollback_record;

static bool indirection_session_initialized = false;
static bool indirection_transaction_initialized = false;
static bool indirection_statement_initialized = false;

static xptr last_indir = XNULL;

bool is_rolled_back()
{
    return rollback_mode != MODE_NORMAL;
}

xptr add_record_to_indirection_table(xptr p)
{
    xptr rba;
    CHECKP(p);
    node_blk_hdr * rbh;
    node_blk_hdr * nbh=(GETBLOCKBYNODE(p));

    if (rollback_mode==MODE_UNDO)
    {
        rba=rollback_record;
        rbh=(GETBLOCKBYNODE(rba));

        if (blocks_to_delete->find(BLOCKXPTR(rba)) != blocks_to_delete->end()) {
            blocks_to_delete->erase(BLOCKXPTR(rba));
            createBlockNextToTheCurrentBlock(nbh, BLOCKXPTR(rba));
            CHECKP(p);
        }

        U_ASSERT(blocks_to_delete->find(BLOCKXPTR(rba)) == blocks_to_delete->end());
    } else {
        if (nbh->free_first_indir!=0)
            rba=ADDR2XPTR(GETPOINTERTODESC(nbh,nbh->free_first_indir));
        else
        {
            xptr q_bl=nbh->snode->bblk_indir;
            CHECKP(q_bl);
            node_blk_hdr * qbh=(GETBLOCKBYNODE(q_bl));
            rba=ADDR2XPTR(GETPOINTERTODESC(qbh,qbh->free_first_indir));
        }
    }

    CHECKP(rba);        
    VMM_SIGNAL_MODIFICATION(rba);

    node_blk_hdr * nbi=(GETBLOCKBYNODE(rba));
    nbi->indir_count++;
    nbi->free_first_indir=*((shft*)((char*)nbi+nbi->free_first_indir));
    *(xptr*)(XADDR(rba)) = p;
    if ((BLOCKXPTR(rba))!=(BLOCKXPTR(p)))
    {
        if (nbi->indir_count>=nbi->count&& 
            (nbi->pblk_indir!=XNULL ||
            nbi->nblk_indir!=XNULL ||
            nbi->snode->bblk_indir==nbi->sm_vmm.p))
        {
            xptr l_bl=nbi->pblk_indir;
            xptr r_bl=nbi->nblk_indir;
            if (nbi->snode->bblk_indir==nbi->sm_vmm.p)
                nbi->snode.modify()->bblk_indir=r_bl;
            else
            if (l_bl!=XNULL)
            {
                CHECKP(l_bl);
                VMM_SIGNAL_MODIFICATION(l_bl);
                node_blk_hdr * lbi=(GETBLOCKBYNODE(l_bl));
                lbi->nblk_indir=r_bl;
            }
            if (r_bl!=XNULL)
            {
                CHECKP(r_bl);
                VMM_SIGNAL_MODIFICATION(r_bl);
                node_blk_hdr * rbi=(GETBLOCKBYNODE(r_bl));
                rbi->pblk_indir=l_bl;
            }

            CHECKP(rba);
            VMM_SIGNAL_MODIFICATION(rba);
            nbi->pblk_indir = XNULL;
            nbi->nblk_indir = XNULL;
        }
        CHECKP(p);
        if (nbh->indir_count<nbh->count&& 
            (nbh->pblk_indir==XNULL &&
            nbh->nblk_indir==XNULL &&
            nbh->snode->bblk_indir!=nbh->sm_vmm.p))
        {
            VMM_SIGNAL_MODIFICATION(p);
            nbh->nblk_indir=nbh->snode->bblk_indir;
            nbh->snode.modify()->bblk_indir=nbh->sm_vmm.p;
            xptr r_bl=nbh->nblk_indir;
            if (r_bl!=XNULL)
            {
                CHECKP(r_bl);
                VMM_SIGNAL_MODIFICATION(r_bl);
                node_blk_hdr * rbi=(GETBLOCKBYNODE(r_bl));
                rbi->pblk_indir=rbi->snode->bblk_indir;
            }
        }

    }
    CHECKP(rba);

    last_indir = rba; // we need this hint to apply dynamic xptr remapping durind redo

    return rba;
}

void del_record_from_indirection_table(xptr p)
{
    CHECKP(p);      
    VMM_SIGNAL_MODIFICATION(p);

    xptr node=*((xptr*)XADDR(p));
    node_blk_hdr* nbi=GETBLOCKBYNODE(p);
    node_blk_hdr* nbh=GETBLOCKBYNODE(node);
    nbi->indir_count--;

    if (nbi->count+nbi->indir_count==0) {
        add_predeleted_block(ADDR2XPTR(nbi));
        CHECKP(p);
    }

    *(shft*)(XADDR(p)) = nbi->free_first_indir;
    nbi->free_first_indir=CALCSHIFT(XADDR(p),nbi);

    if ((BLOCKXPTR(node))!=(BLOCKXPTR(p)))
    {
        if (nbi->indir_count<nbi->count&& 
            (nbi->pblk_indir==XNULL &&
            nbi->nblk_indir==XNULL &&
            nbi->snode.modify()->bblk_indir!=nbi->sm_vmm.p))
        {
            VMM_SIGNAL_MODIFICATION(p);
            nbi->nblk_indir=nbi->snode->bblk_indir;
            nbi->snode.modify()->bblk_indir=nbi->sm_vmm.p;
            xptr r_bl=nbi->nblk_indir;
            if (r_bl!=XNULL)
            {
                CHECKP(r_bl);
                VMM_SIGNAL_MODIFICATION(r_bl);
                node_blk_hdr * rbi=(GETBLOCKBYNODE(r_bl));
                rbi->pblk_indir=rbi->snode->bblk_indir;
            }
        }

        CHECKP(node);
        if (nbh->indir_count>=nbh->count&& 
            (nbh->pblk_indir!=XNULL ||
            nbh->nblk_indir!=XNULL ||
            nbh->snode->bblk_indir==nbh->sm_vmm.p))
        {
            xptr l_bl=nbh->pblk_indir;
            xptr r_bl=nbh->nblk_indir;
            if (nbh->snode->bblk_indir==nbh->sm_vmm.p)
                nbh->snode.modify()->bblk_indir=r_bl;
            else
            if (l_bl!=XNULL)
            {
                CHECKP(l_bl);
                VMM_SIGNAL_MODIFICATION(l_bl);
                node_blk_hdr * lbi=(GETBLOCKBYNODE(l_bl));
                lbi->nblk_indir=r_bl;
            }
            if (r_bl!=XNULL)
            {
                CHECKP(r_bl);
                VMM_SIGNAL_MODIFICATION(r_bl);
                node_blk_hdr * rbi=(GETBLOCKBYNODE(r_bl));
                rbi->pblk_indir=l_bl;
            }
    
            CHECKP(node);
            VMM_SIGNAL_MODIFICATION(node);
            nbh->pblk_indir = XNULL;
            nbh->nblk_indir = XNULL;
        }
    }   

    CHECKP(p);
}

void indirection_table_on_session_begin()
{
    indirection_session_initialized = true;
}

void indirection_table_on_transaction_begin()
{
    if (blocks_to_delete!=NULL) delete blocks_to_delete;    
    blocks_to_delete = se_new std::set<xptr>;

    indirection_transaction_initialized = true;
}

void indirection_table_on_statement_begin()
{
    indirection_statement_initialized = true;
}

void indirection_table_on_session_end()
{
    if (indirection_session_initialized) {
        indirection_session_initialized = false;
    }
}

void sync_indirection_table()
{
    if (blocks_to_delete == NULL) return;
    //2. scan the deleted blocks and delete some of them
    std::set<xptr>::iterator it2= blocks_to_delete->begin();
    xptr block;

    while (it2!=blocks_to_delete->end())
    {
        block = *it2;
        CHECKP(block);
        node_blk_hdr* blk=GETBLOCKBYNODE(block);
        U_ASSERT(blk->count+blk->indir_count==0);
        vmm_delete_block(blk->sm_vmm.p);
        it2++;
    }

    delete blocks_to_delete;
    blocks_to_delete=NULL;
}

void indirection_table_on_transaction_end()
{
    if (indirection_transaction_initialized)
    {
        sync_indirection_table();
        rollback_mode = MODE_NORMAL;
        indirection_transaction_initialized = false;
    }
}

void indirection_table_on_statement_end()
{
    if (indirection_statement_initialized) {
        indirection_statement_initialized = false;
    }
}

// functions for rollback

void switch_to_rollback_mode(int type)
{
    if (blocks_to_delete==NULL)     
    blocks_to_delete = se_new std::set<xptr>;

    rollback_mode = type;
}

void set_rollback_record(xptr p)
{
    rollback_record = p;
}

void add_predeleted_block(xptr block)
{
    if (IS_DATA_BLOCK(block)) {
        CHECKP(block);
        node_blk_hdr * nbh = (node_blk_hdr *) XADDR(block);
        
        U_ASSERT(blocks_to_delete->find(BLOCKXPTR(block)) == blocks_to_delete->end());
        U_ASSERT((nbh->count + nbh->indir_count) == 0);

        xptr li = nbh->pblk_indir;
        xptr ri = nbh->nblk_indir;
        xptr lb = nbh->pblk;
        xptr rb = nbh->nblk;

        if (nbh->snode->bblk_indir == block)
            nbh->snode.modify()->bblk_indir = ri;

        if (nbh->snode->bblk == block)
            nbh->snode.modify()->bblk = rb;

        col_schema_node_object* snode_as_collection = dynamic_cast<col_schema_node_object*>(&*schema_node_cptr(nbh->snode));

        if ((snode_as_collection != NULL) && (snode_as_collection->eblk == block)) {
            snode_as_collection->eblk = rb;
        }

        VMM_SIGNAL_MODIFICATION(block);
        nbh->pblk_indir = XNULL;
        nbh->nblk_indir = XNULL;
        nbh->pblk = XNULL;
        nbh->nblk = XNULL;
        
        if (li!=XNULL)
        {
            CHECKP(li);
            VMM_SIGNAL_MODIFICATION(li);
            ((node_blk_hdr *) (GETBLOCKBYNODE(li)))->nblk_indir = ri;
        }

        if (lb!=XNULL)
        {
            CHECKP(lb);
            VMM_SIGNAL_MODIFICATION(lb);
            ((node_blk_hdr *) (GETBLOCKBYNODE(lb)))->nblk = rb;
        }

        if (ri!=XNULL)
        {
            CHECKP(ri);
            VMM_SIGNAL_MODIFICATION(ri);
            ((node_blk_hdr *) (GETBLOCKBYNODE(ri)))->pblk_indir = li;
        }

        if (rb!=XNULL)
        {
            CHECKP(rb);
            VMM_SIGNAL_MODIFICATION(rb);
            ((node_blk_hdr *) (GETBLOCKBYNODE(rb)))->pblk = lb;
        }

        CHECKP(block); // callers usually expect block to stay the same

        blocks_to_delete->insert(block);
    }
}




/*
 *  Check indirection infrastructure to be consistent.
 * 
 *  For given schema node the function checks following invariants:
 *
 *   - occupied indirection record count equals to the number of 
 *     occupied node descriptors
 *
 *   - count of occupied indirection records, stored in indirection quote blocks
 *     is less than node descriptor count for that blocks
 *
 *   - count of occupied indirection records, stored in blocks, that are not 
 *     indirection quote blocks, is greater than or equal to node descriptor 
 *     count for that blocks
 *
 *   - check block list pointers and indirection quota block list pointers
 *
 *  If recourse parameter is set to true, recoursively repeat function for all
 *  schema node children
 *
 */

bool check_indirection_consistency_schema(schema_node_cptr sn, bool recourse = false) 
{
    xptr b, left_nb;
    node_blk_hdr * nbh;
    int node_desc_count, ind_rec_count;
    sc_ref_item* sn_i;
    
    b = sn->bblk;
    node_desc_count = 0;
    ind_rec_count = 0;
    while (b != XNULL) {
        CHECKP(b);
        nbh = ((node_blk_hdr *) XADDR(b));
        
        /* Count the total number of node descriptors and indirection records */
    
        node_desc_count += nbh->count;
        ind_rec_count += nbh->indir_count;

        /* If the block is in inirection quota list, check if it should really be
         * there. And if it's not there, check whether it really should not. */
        
        if (nbh->nblk_indir != XNULL || nbh->pblk_indir != XNULL) {
            if (nbh->count <= nbh->indir_count) 
                throw SYSTEM_EXCEPTION("Indirection quota block has overfull indirection table");
//              throw USER_EXCEPTION2(SE2030, "Indirection quota block has overfull indirection table");
        } else {
            if ((nbh->snode->bblk_indir != nbh->sm_vmm.p) && (nbh->count > nbh->indir_count)) 
                throw SYSTEM_EXCEPTION("Non-indirection quota block has underfull indirection table");
//              throw USER_EXCEPTION2(SE2030, "Non-indirection quota block has underfull indirection table");
        }
        
        b = nbh->nblk;
    }

  /* Check if the total number of node descriptors equals to the 
     * one of indirection records  */
    
    if (node_desc_count != ind_rec_count)
        throw SYSTEM_EXCEPTION("Total number of node descriptors and total number of indirection table records differ");
//      throw USER_EXCEPTION2(SE2030, "Total number of node descriptors and total number of indirection table records differ");
    
    b = sn->bblk_indir;
    left_nb = XNULL;
    while (b != XNULL) {
        CHECKP(b);
        nbh = ((node_blk_hdr *) XADDR(b));
        
    /* Check if quota block list pointer is ok and 
         * belongs to the right schema node */      

        if (nbh->snode != sn.ptr())
            throw SYSTEM_EXCEPTION("Unexpected block in indirection quota chain");
//          throw USER_EXCEPTION2(SE2030, "Unexpected block in indirection quota chain");
        
        if (nbh->pblk_indir != left_nb)
            throw SYSTEM_EXCEPTION("Broken indirection quota chain");
//          throw USER_EXCEPTION2(SE2030, "Broken indirection quota chain");
        
        left_nb = b;

        b = nbh->nblk_indir;
    }
    
    if (recourse) {
        sn_i = sn->children.first;
        while (sn_i != NULL) {
            if (!check_indirection_consistency_schema(sn_i->object.snode, true)) {
                return false;
            }
            sn_i = sn_i->next;
        }
    }
    
    return true;
}

/*
 *  Check indirection infrastructure to be consistent wrapper.
 * 
 *  Function takes node pointer as input and computes it's schema node.
 *  Then, checks indirection consistency for that schema node.
 *
 */

bool check_indirection_consistency(xptr p, bool recourse) 
{
    CHECKP(p);
    schema_node_cptr sn = (GETBLOCKBYNODE(p))->snode;
    
    return check_indirection_consistency_schema(sn, recourse);
}

// we use this function to fetch a hint about last indirection record (for redo purposes)
xptr get_last_indir()
{
    return last_indir;
}
