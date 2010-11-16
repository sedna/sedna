/*
 * File:  numb_scheme.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/structures/schema.h"
#include "tr/vmm/vmm.h"
#include "tr/pstr/pstr.h"
#include "tr/cat/catvars.h"
#include "tr/mo/microoperations.h"

#include "tr/nid/lex.h"
#include "tr/nid/numb_scheme.h"
#include "tr/nid/nid.h"
#include "tr/nid/nidalloc.h"

#ifndef min
#define min(x,y) ((x) < (y) ? (x) : (y))
#endif

static fnumber PROPORTION(0,1);  /* proportion used to divide alphabet sequence */
static xptr	TMPNIDBLK;           /* current temporary block for nid prefixes */
static doc_schema_node_xptr nid_holder; /*current persistent block holder*/

int nid_block_count = 0;
t_nid NIDNULL;
std::pair<int /*size*/,int /*increment*/>* sizehnt=NULL;

void
nid_set_proportion(fnumber p) {
	PROPORTION = p;
}

bool
NID_CONSISTENT(t_prefix &a) {
	return
		(a.prefix[a.size-1] != ALPHABET_SIZE);
}

/*
 * Get nid of node descriptor "dsc" uploading native block of this descriptor
 */
t_nid
nid_get_nid(xptr node) {
	t_nid	result;
	internal::node_base_t*	dsc;

    CHECKP(node);
	/* dsc now points to descriptor inside in-memory block */
	dsc = (internal::node_base_t*)XADDR(node);
	result = dsc->nid;
	return result;
}

bool nid_parse(const xptr nid, xptr * prefix, shft * size) {
    CHECKP(nid);

    const t_nid * tnid = (t_nid *) XADDR(nid);
    const unsigned char sz = tnid->size;

    if (sz == 0) {
        *size = *(shft*) ((tnid->prefix) + sizeof(xptr));
        *prefix = pstrderef(checkp(*(xptr*) (tnid->prefix)));
        return true;
    } else {
        *prefix = nid;
        *size = sz;
        return false;
    }
}


/*
 * Locate the block where to create prefix of size p_size
 */
static xptr
nid_get_blk(shft p_size, bool persistent) {
	/*--------------------------------------------------------
	>>> this is the case when block is determined by neighbouring node
	xptr	result=XNULL;
	t_nid	existing_nid = nid_get_nid(existing_node);
	if (existing_nid.external) {
		result = BLOCKXPTR((*(xptr*)existing_nid.prefix));
		if (!pstr_fit_into_blk(result, p_size+1))
			result = pstr_create_blk();
	} else
		result = pstr_create_blk();

	--------------------------------------------------------*/

	if (persistent) {
		if (nid_holder->ext_nids_block == XNULL) {
			nid_holder.modify()->ext_nids_block = pstr_create_blk(true);
			nid_block_count++;
		}
		if (!pstr_fit_into_blk(nid_holder->ext_nids_block, p_size)) {
				nid_holder.modify()->ext_nids_block = pstr_create_blk(true);
				nid_block_count++;
		}
		return nid_holder->ext_nids_block;
	} else {
		if (TMPNIDBLK == XNULL) {
			TMPNIDBLK = pstr_create_blk(false);
		}
		if (!pstr_fit_into_blk(TMPNIDBLK, p_size)) {
				TMPNIDBLK = pstr_create_blk(false);
		}
		return TMPNIDBLK;
	}
}

/*
	initialize the node descriptor with given nid (prefix, dc)
	blk is the block where to store prefix string
 */
void nid_assign_pers_data(xptr node,char* data,int size)
{
		xptr blk = nid_get_blk(size, true);
		xptr tmp = pstr_do_allocate(blk, data, size);
		CHECKP(node);
		internal::node_base_t* dsc=(internal::node_base_t*)XADDR(node);
		VMM_SIGNAL_MODIFICATION(node);
		memcpy(dsc->nid.prefix, (char*)&tmp, sizeof(xptr));
		*(shft*)(dsc->nid.prefix + sizeof(xptr)) = size;
		dsc->nid.size = 0;
}
void	nid_assign(xptr node, t_prefix p) {
	internal::node_base_t*	dsc;
	xptr	tmp;
	xptr	blk;
	dsc = (internal::node_base_t*)XADDR(node);
	CHECKP(node);
	if (p.size <= MAXINTERNALPREFIX)
	{

		VMM_SIGNAL_MODIFICATION(node);
		memcpy(dsc->nid.prefix, p.prefix, p.size);
		dsc->nid.size=(unsigned char)p.size;
	}
	else
	{
		if (p.size > PSTRMAXSIZE)
		{
			dsc->nid.prefix[0]=1;
			dsc->nid.size=1;
            moSetUserException(SE2023);
		}
		if (IS_DATA_BLOCK(node)) {
			nid_holder=getSchemaNode(node)->root;
            U_ASSERT(nid_holder != XNULL);
        }
		blk = nid_get_blk(p.size, IS_DATA_BLOCK(node));
		tmp = pstr_do_allocate(blk, (char*) p.prefix, p.size);
/*--------------------------------------------------------
>>> this is the debug case of keeping prefix just in memory
		tmp.addr = se_new char[prefix_size];
		strcpy((char*)tmp.addr, prefix);
--------------------------------------------------------*/
		CHECKP(node);
		VMM_SIGNAL_MODIFICATION(node);
		memcpy(dsc->nid.prefix, (char*)&tmp, sizeof(xptr));
		*(shft*)(dsc->nid.prefix + sizeof(xptr)) = p.size;
		dsc->nid.size = 0;
		//statistics
		getSchemaNode(node).modify()->extnids+=p.size;
		if (IS_DATA_BLOCK(node))
			nid_holder.modify()->total_ext_nids+=p.size;

	}
}

/*
 * Read the prefix of a given nid from inside the nid or from external block
 */
t_prefix
nid_get_prefix(xptr node) {
	CHECKP(node);
	/* dsc now points to descriptor inside in-memory block */
	t_nid* the_nid = &((internal::node_base_t*)XADDR(node))->nid;

	t_prefix	result;
	result.prefix =(unsigned char*)nid_alloc();
	result.size = (the_nid->size==0)?*(shft*)(the_nid->prefix+sizeof(xptr)):the_nid->size;
	if (the_nid->size==0) {
		xptr	prefixp	= *(xptr*)the_nid->prefix;
		/*--------------------------------------------------------
		>>> this is the debug case when prefix is allocated just in memory
		memcpy (result, (char*)XADDR(prefixp), prefix_size);
		delete[] (char*)XADDR(prefixp);
		return result;
		--------------------------------------------------------*/
		pstr_read(prefixp, result.size, (char*)result.prefix);
	} else
		memcpy(result.prefix, the_nid->prefix, result.size);
	return result;
}

t_prefix
nid_get_prefix(t_nid the_nid) {
	t_prefix	result;
	result.prefix =(unsigned char*)nid_alloc();
	result.size = (the_nid.size==0)?*(shft*)(the_nid.prefix+sizeof(xptr)):the_nid.size;
	if (the_nid.size==0) {
		xptr	prefixp	= *(xptr*)the_nid.prefix;
		/*--------------------------------------------------------
		>>> this is the debug case when prefix is allocated just in memory
		memcpy (result, (char*)XADDR(prefixp), prefix_size);
		delete[] (char*)XADDR(prefixp);
		return result;
		--------------------------------------------------------*/
		pstr_read(prefixp, result.size, (char*)result.prefix);
	} else
		memcpy(result.prefix, the_nid.prefix, result.size);
	return result;
}


/* effective comparison */
int	nid_cmp_effective(xptr node1, xptr node2) {

    if (node1 == node2) return 0;

    CHECKP(node1);
	t_prefix p1 = nid_get_prefix(node1);
	unsigned char* ptr1 = p1.prefix;
	shft size1 = p1.size;
	CHECKP(node2);
	t_nid* nd = &((internal::node_base_t*)XADDR(node2))->nid;
	shft size2 = nd->size;
	unsigned char* ptr2=nd->prefix;
	if (size2 == 0)
	{
		xptr ps	= *(xptr*)ptr2;
		size2=*(shft*)(nd->prefix+sizeof(xptr));
		CHECKP(ps);
		ptr2=(unsigned char*)XADDR(BLOCKXPTR(ps)) + *(shft*)XADDR(ps);
	}

	int result = sign(memcmp(ptr1,ptr2,min(size1,size2))); /// There is no guarantee that memcmp returns 1, -1, 0!
	if (!result)
	{
        if (size1 > size2) {
            result = (ptr1[size2] == ALPHABET_SIZE) ? 1 : 2;
        } else {
            result = (ptr2[size1] == ALPHABET_SIZE) ? -1 : -2;
        }
	}
	nid_free(p1.prefix);
	return result;
}

/* doc-order comparison */
int nid_cmp(xptr node1, xptr node2) {
	t_nid		nid1;
	t_nid		nid2;
	t_prefix	p1;
	t_prefix	p2;
	int		result=0;

    if (node1 == node2) return result;

	/* load sibling prefixes */
    // !!! Andrey: it seems there is an error. nid_get_nid calls CHECKP but nid_get_prefix doesn't
	nid1 = nid_get_nid(node1);
	nid2 = nid_get_nid(node2);
	p1 = nid_get_prefix(nid1);
	p2 = nid_get_prefix(nid2);

    result = lex_cmp(p1, p2);

	/* nid memory free */
	nid_free(p1.prefix);
	nid_free(p2.prefix);

	return result;
}

/* ancestor comparison (true if node1 is ancestor of node2) */
bool nid_ancestor(xptr node1, xptr node2) {
	/* COMPLETELY CHANGED */
	t_nid		nid1;
	t_nid		nid2;
	t_prefix	p1;
	t_prefix	p2;
	t_prefix	limp;
	bool		result=false;

	/* load sibling prefixes */
    nid1 = nid_get_nid(node1);
	nid2 = nid_get_nid(node2);
	int size1=(nid1.size==0)?*((shft*)(nid1.prefix+sizeof(xptr))) :nid1.size;
	int size2=(nid2.size==0)?*((shft*)(nid2.prefix+sizeof(xptr))) :nid2.size;
	if (size1>=size2) return false;
	p1 = nid_get_prefix(nid1);
	p2 = nid_get_prefix(nid2);


	if ( p2.prefix[size1]!=ALPHABET_SIZE &&lex_ispref(p1, p2))
			result = true;

	/* nid memory free */
	nid_free(p1.prefix);
	nid_free(p2.prefix);

	return result;
}


/* descendant comparison (true is node1 is descendant of node2) */
bool	nid_descendant(xptr node1, xptr node2) {
	return nid_ancestor(node2, node1);
}


/*  generate nid for new outmost left sibling
	use PROPORTION var to break alphabet
 */

void decrementNID(t_prefix &lp,int size)
{
	//1. Decrementation
	int pos=lp.size-1;
	if (lp.prefix[pos]!=1)
	{
		lp.prefix[pos]--;
		return;
	}
	else
	{
		if (lp.size-size>1)
		{
			while (true)
			{
				pos--;
				if (pos==size-1) break;
				if (lp.prefix[pos]>0)
				{
					if (lp.prefix[pos]>1)
					{
						lp.prefix[pos]--;
						lp.size=pos+1;
						return;
					}
					else
					{
						lp.prefix[pos]=0;
						lp.prefix[pos+1]=DEF_LETTER;
						lp.size=pos+2;
						return;
					}
				}
			}
			lp.prefix[lp.size-1]=0;
			lp.prefix[lp.size]=DEF_LETTER;
			lp.size++;
			return;
		}
		else
		{
			lp.prefix[pos]=0;
			lp.prefix[pos+1]=DEF_LETTER;
			lp.size++;
			return;
		}
	}
}

/*  generate nid for new outmost right sibling
	use PROPORTION var to break alphabet
 */

void incrementNID_between(t_prefix &lp, int size)
{
	//1. Incrementation
	lp.prefix[lp.size-1]++;
	//2. Checking
	if (lp.prefix[lp.size-1]==ALPHABET_SIZE)
	{
		int pos=lp.size-1;
		while (pos>size)
		{
			lp.prefix[pos]=1;
			pos--;
			if (lp.prefix[pos]!=ALPHABET_SIZE)
			{
				lp.prefix[pos]++;
				return;
			}
		}

		pos=size;
		while (pos<lp.size)
		{
			lp.prefix[pos]=ALPHABET_SIZE;
			pos++;
		}
		lp.prefix[lp.size]=DEF_LETTER;
		lp.prefix[lp.size+1]=1;
		lp.size+=2;
	}
}

void incrementNID(t_prefix &lp, int size)
{
	//1. Incrementation
	lp.prefix[lp.size-1]++;
	//2. Checking
	if (lp.prefix[size]==ALPHABET_SIZE)
	{
		if (lp.size!=size+1) throw SYSTEM_EXCEPTION("Algo error");
		lp.prefix[size]=MAX_LETTER;
		lp.prefix[size+1]=ALPHABET_SIZE;
		lp.prefix[size+2]=1;
		lp.size+=2;
		return;
	}
	if (lp.prefix[lp.size-1]==ALPHABET_SIZE)
	{
		int pos=lp.size-1;
		while (pos>size)
		{
			lp.prefix[pos]=1;
			if (lp.prefix[pos-1]!=ALPHABET_SIZE)
			{
				lp.prefix[pos-1]++;
				break;
			}
			pos--;
		}
		if (lp.prefix[size]==ALPHABET_SIZE)
		{
			lp.prefix[size]=MAX_LETTER;
			pos=size+1;
			while (pos<lp.size)
			{
				lp.prefix[pos]=ALPHABET_SIZE;
				pos++;
			}
			lp.prefix[lp.size]=DEF_LETTER;
			lp.prefix[lp.size+1]=1;
//			lp.size+=1;
			lp.size+=2;
		}
	}
}


t_prefix* betweenNID(t_prefix & lp,t_prefix & rp)
{
	int difsmall=0;
	int pos=0;
	//1.find diff
	while(pos<lp.size && pos<rp.size)
	{
		if (lp.prefix[pos]<rp.prefix[pos])
		{
			if (lp.prefix[pos]+1<rp.prefix[pos])
			{
				lp.prefix[pos]++;
				lp.size=pos+1;
				return &lp;
			}
			else
				if (!difsmall) difsmall=pos;
		}
		pos++;
	}
	//2,3. BAD CASE
	if (difsmall!=0)
	{
		if (difsmall<lp.size-1)
		{
			//3.2 0<difsmall<lp.size-1
			incrementNID_between(lp,difsmall+1);
		}
		else
		{
			//3.1 difsmall==lp.size-1
			lp.prefix[difsmall+1]=ALPHABET_SIZE;
			lp.prefix[difsmall+2]=DEF_LETTER;
			lp.size+=2;
		}
		return &lp;
	}
	else
	{
		// 2 right=left|255|xxxx
		decrementNID(rp,lp.size+1);
		return &rp;
	}
}
/*  generate nid between given left and rigth prefixes
	use PROPORTION var to break alphabet
 */
void	nid_create_between(xptr left, xptr right, xptr result) {
	t_nid rnid = nid_get_nid(right);
	t_prefix rp = nid_get_prefix(rnid);
	t_nid lnid = nid_get_nid(left);
	t_prefix lp = nid_get_prefix(lnid);
	t_prefix* resp=betweenNID(lp,rp);

	U_ASSERT(NID_CONSISTENT(*resp));

	nid_assign(result,*resp);
	/* memory free */
	nid_free(lp.prefix);
	nid_free(rp.prefix);
}

void	nid_create_right(xptr left, xptr parent, xptr result) {
	t_nid lnid = nid_get_nid(left);
	t_prefix lp = nid_get_prefix(lnid);
	//generation
	if (sizehnt)
	{
		int pos=lp.size-1;
		if (((int)lp.prefix[pos])+sizehnt->second<ALPHABET_SIZE)
			lp.prefix[pos]+=sizehnt->second;
		else
		{
			lp.prefix[pos]=(unsigned char)((int)sizehnt->second+lp.prefix[pos]-MAX_LETTER);
			while (true)
			{
				if (pos==lp.size-sizehnt->first)
					throw USER_EXCEPTION(SE2023);
				pos--;
				lp.prefix[pos]++;
				if (lp.prefix[pos]!=ALPHABET_SIZE)
					break;
				else
					lp.prefix[pos]=1;

			}
		}
	}
	else
	{
		t_nid pnid = nid_get_nid(parent);
		int size=(pnid.size>0)?pnid.size:*((shft*)(pnid.prefix+sizeof(xptr)));
		incrementNID(lp, size);
	}

	U_ASSERT(NID_CONSISTENT(lp));

	nid_assign(result,lp);
	/* memory free */
	nid_free(lp.prefix);
};

/*  generate nid for new outmost left sibling
	use PROPORTION var to break alphabet
*/
void	nid_create_left(xptr right, xptr parent, xptr result) {
	t_nid rnid = nid_get_nid(right);
	t_prefix lp = nid_get_prefix(rnid);
	//generation
	t_nid pnid = nid_get_nid(parent);
	int size=(pnid.size>0)?pnid.size:*((shft*)(pnid.prefix+sizeof(xptr)));
	decrementNID(lp,size);

	U_ASSERT(NID_CONSISTENT(lp));
	nid_assign(result,lp);
	/* memory free */
	nid_free(lp.prefix);
}

/* generate nid for the first child of parent argument
   use PROPORTION var to break alphabet
 */
void	nid_create_child(xptr parent, xptr result)
{
	t_nid pnid = nid_get_nid(parent);
	t_prefix pp = nid_get_prefix(pnid);
	//generation
	if (sizehnt)
	{
		for (int i=0;i<sizehnt->first-1;i++)
			pp.prefix[pp.size+i]=1;
		pp.prefix[pp.size+sizehnt->first-1]=1+sizehnt->second;
		pp.size+=sizehnt->first;
	}
	else
	{
		pp.prefix[pp.size]=1;
		pp.size+=1;
	}

	U_ASSERT(NID_CONSISTENT(pp));
	nid_assign(result,pp);
	/* memory free */
	nid_free(pp.prefix);
}
/* We use bitmap matrix in the "persistent_db_data" region to account prefixes allocated
   for root document nodes as described below:
   For the alphabet of size ALPHABET_SIZE we have the following sequence of symbols in
   the alphabet {1,2, ..., ALPHABET_SIZE-1}. Initially we let the pool of free prefixes to
   be {1,2, ..., ALPHABET_SIZE-2}. Note that the ALPHABET_SIZE-1 prefix is reserved for
   future extension of the pool. Then initial pool of free prefixes includes one-symbol
   prefixes and occupies ALPHABET_SIZE-1/8(bits in byte) + 1 bytes, that is alligned to the
   byte boundary (i.e. the last byte can be partially filled). Each bit in the bitmap encodes
   allocation status of corresponding prefix of the sequence {1,2, ..., ALPHABET_SIZE-2}.
   The bit is "1" if the prefix is free and "0" otherwise. When no more prefixes remain in
   the initial pool, it is being extended to the double size. The following sequence
   of prefixes is appended to initial one:
   {1(ALPHABET_SIZE-1), 2(ALPHABET_SIZE-1), ..., ALPHABET_SIZE-1(ALPHABET_SIZE-1)} via double
   extension of bitmap. The subsequent extentions are carried out similarly.
*/

/* extends bitmap with new layer returning pointer to that layer begining */
//char* nid_extend_bitmap() {
//	int		bytes_in_layer = 0;
//	char*	ret;
//
//	bytes_in_layer = (ALPHABET_SIZE-2)/8;
//	if ((ALPHABET_SIZE-2)%8)
//			bytes_in_layer++;
//	entry_point->nbm = (char*)pers_realloc(entry_point->nbm, bytes_in_layer*(entry_point->nbm_size+1));
//	/* raise all bits of extention */
//	memset(entry_point->nbm + bytes_in_layer*(entry_point->nbm_size), 0xff, bytes_in_layer);
//	ret=entry_point->nbm + bytes_in_layer*(entry_point->nbm_size);
//	entry_point->nbm_size++;
//	return ret;
//}



void updateEP_nid(int * last_nid_size, uint8_t * last_nid)
{
    if (*last_nid_size==255) {
//        throw MAX_ROOT_NID
        return;
    }

    if (*last_nid_size==0)
    {
        *last_nid_size=1;
        last_nid[0]=1;
        return;
    } else if (last_nid[*last_nid_size-1]!=MAX_LETTER) {
        last_nid[*last_nid_size-1]++;
        return;
    } else {
        // 1. searching pos to increment
        int pos=*last_nid_size-3;
        while (pos>=0) {
            if (last_nid[pos] < MAX_LETTER) {
                //2. case w/o expansion
                last_nid[pos]++;
                pos+=2;
                while (pos < *last_nid_size) {
                    last_nid[pos] = 1;
                    pos+=2;
                }
                return;
            }
            pos-=2;
        }

        //3 expanding
        *last_nid_size+=2;
        int i = 0;
        for (i=0;i<*last_nid_size;i+=2) last_nid[i]=1;
        for (i=1;i<*last_nid_size;i+=2) last_nid[i]=ALPHABET_SIZE;
        return;
    }
}

/* generate nid for the root nodes */
void nid_create_root(xptr result, bool persistent)
{
    internal::node_base_t* dsc = (internal::node_base_t*)XADDR(result);
    VMM_SIGNAL_MODIFICATION(result);

    if (persistent) {
        updateEP_nid(&last_nid_size, last_nid);
        if (last_nid_size <= MAXINTERNALPREFIX) {
            dsc->nid.size = last_nid_size;
            memcpy(dsc->nid.prefix, last_nid, dsc->nid.size);
        } else {
            nid_assign_pers_data(result, (char*)last_nid, last_nid_size);
        }
    } else {
        dsc->nid.prefix[0]=0;
        dsc->nid.size=1;
    }
}

/*
	free the nid; if it is a doc root, account it in root bitmap
 */
/* debug */
/*t_prefix tp;*/
void	nid_delete(xptr node) {
    CHECKP(node);
	bool	vroot = getNodeType(node) == virtual_root;

	t_nid	the_nid = nid_get_nid(node);

	/* free the pstr contents */
	if (the_nid.size==0 /*CHANGED BY LEON*/)
	{
		//statistics
		shft nids=(*(shft*)(the_nid.prefix+sizeof(xptr)));
		schema_node_cptr scm=getSchemaNode(node);
        scm.modify();
		scm->extnids-=nids;
		if (IS_DATA_BLOCK(node))
			scm->root.modify()->total_ext_nids-=nids;
		xptr	blk = BLOCKXPTR((*(xptr*)the_nid.prefix));
		if (
			pstr_do_deallocate(blk, *(xptr*)(the_nid.prefix), *(shft*)(the_nid.prefix+sizeof(xptr)), true)
			&& scm->root->ext_nids_block==blk
			)
			scm->root.modify()->ext_nids_block=XNULL;
		/*--------------------------------------------------------
		>>> this is debug case when prefix is allocated just in memory
		delete[] (char*)XADDR(*(xptr*)the_nid.prefix);
		--------------------------------------------------------*/
	}
}

void    	nid_on_kernel_statement_end()
{
    TMPNIDBLK = XNULL;
}

/*
	wrapper upon nid generation functions depending on value of "p"
 */
t_prefix nid_generate(t_prefix p1, t_prefix p2, fnumber p) {
	if (!p.getx())
		return lex_next(p1, p2);
	else if (p.getx()==1 && p.gety()==2)
		return lex_middle(p1, p2);
	else
		return lex_between(p1, p2, p);
}

void	nid_print(xptr node)
{
	t_nid	the_nid = nid_get_nid(node);
	t_prefix p	= nid_get_prefix(the_nid);
	d_printf1("(");
	for (int i=0; i<p.size; i++)
		d_printf2("%d=",(int)(unsigned char)p.prefix[i]);
    d_printf1(")");
	nid_free(p.prefix);
}

/* */
void	nid_print(xptr node, se_ostream& c)
{
	t_nid	the_nid = nid_get_nid(node);
	t_prefix p	= nid_get_prefix(the_nid);
	c << "(";
	for (int i=0; i<p.size; i++)
		c << (int)(unsigned char)p.prefix[i]<<"=";
	c << ",";
// REMOVED	c << the_nid.dc;
	c << ")";
	nid_free(p.prefix);
}

void    nid_print(xptr node, std::ostream& c)
{
    t_nid   the_nid = nid_get_nid(node);
    t_prefix p  = nid_get_prefix(the_nid);
    c << "(";
    for (int i=0; i<p.size; i++)
        c << (int)(unsigned char)p.prefix[i]<<"=";
    c << ",";
// REMOVED  c << the_nid.dc;
    c << ")";
    nid_free(p.prefix);
}
