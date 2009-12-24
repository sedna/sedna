/*
 * File:  node_utils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/crmutils/node_utils.h"
#include "tr/locks/locks.h"
#include "tr/pstr/pstr.h"
#include "tr/structures/schema.h"
#include "tr/crmutils/crmutils.h"

using namespace std;

/*returns the next attribute sibling by document order*/
xptr getNextByOrderAttribute(xptr source)
{
    CHECKP(source);
    xptr right=GETRIGHTPOINTER(source);
    while (right!=XNULL)
    {
        CHECKP(right);
        switch (GETTYPE((GETBLOCKBYNODE(right))->snode))
        {
        case attribute: return right;
        case xml_namespace: right=GETRIGHTPOINTER(source);
        default: return XNULL;
        }
    }
    return XNULL;
}

/*returns the first attribute child by document order*/
xptr getFirstByOrderAttributeChild(xptr source)
{
    CHECKP(source);
    shft size=CHILDCOUNT(source);
    schema_node_cptr scn=GETSCHEMENODE(XADDR(source));
    xptr* childx=(xptr*)((char*)XADDR(source)+size_of_node(GETBLOCKBYNODE(source)));
    for (int i=0;i<size;i++)
    {
        if(*childx!=XNULL && scn->get_child_type(i)==attribute)
        {
            xptr node=*childx;
            CHECKP(node);
            xptr left=GETLEFTPOINTER(node);
            while (left!=XNULL)
            {
                CHECKP(left);
                if (GETSCHEMENODE(XADDR(left))->type!=attribute)
                    return node;
                node=left;
                left=GETLEFTPOINTER(node);
            }
            return node;
        }
        childx+=1;
    }
    return XNULL;
}
/*returns the next element sibling by document order*/
xptr getNextByOrderElement(xptr source)
{
    CHECKP(source);
    xptr right=GETRIGHTPOINTER(source);
    if (right==XNULL)return XNULL;
    CHECKP(right);
    while (GETTYPE((GETBLOCKBYNODE(right))->snode)!=element)
    {
        right=GETRIGHTPOINTER(right);
        if (right==XNULL)return XNULL;
        CHECKP(right);
    }
    return right;
}
/*returns the first element child by document order*/
xptr  getFirstByOrderElementChild(xptr source)
{
    xptr fnode=getFirstByOrderChildNode(source);
    if (fnode==XNULL) return XNULL;
    while(fnode!=XNULL)
    {
        CHECKP(fnode);
        if ((GETBLOCKBYNODE(fnode))->snode->type==element)
            return fnode;
        else
            fnode=((n_dsc*)XADDR(fnode))->rdsc;

    }
    return XNULL;
}
/*returns the first text child by document order*/
xptr  getFirstByOrderTextChild(xptr source)
{
    CHECKP(source);
    shft size=CHILDCOUNT(source);
    schema_node_cptr scn=GETSCHEMENODE(XADDR(source));
    xptr* childx=(xptr*)((char*)XADDR(source)+size_of_node(GETBLOCKBYNODE(source)));
    for (int i=0;i<size;i++)
    {
        if(*childx!=XNULL && scn->get_child_type(i)==text)
        {
            xptr node=*childx;
        //  CHECKP(node);
            return node;
        }
        childx+=1;
    }
    return XNULL;
}
/*returns the last  child node by document order*/
xptr  getLastByOrderChildNode(xptr source)
{
    //CHECKP(source);
    xptr par_i=((n_dsc*)XADDR(source))->indir;
    shft size=CHILDCOUNT(source);
    schema_node_cptr scn=GETSCHEMENODE(XADDR(source));
    xptr* childx=(xptr*)((char*)XADDR(source)+size_of_node(GETBLOCKBYNODE(source))+(size-1)*sizeof(xptr));

    for (int i=size-1;i>-1;i--)
    {
        if(*childx!=XNULL)
        {
            xptr nodex=*childx;
            CHECKP(nodex);
            n_dsc* node=(n_dsc*)XADDR(nodex);
            n_dsc* tmp;
            while ((tmp=getNextDescriptorOfSameSort(node))!=NULL)
            {
                if (par_i!=tmp->pdsc) break;
                node=tmp;
                nodex=ADDR2XPTR(node);
            }
            CHECKP(nodex);
            if (node->rdsc==XNULL)
                return nodex;
            CHECKP(source);
        }
        childx-=1;
    }
    return XNULL;
}
/*returns the first  child  in dm:children accessor*/
xptr  getFirstByOrderChildNode(xptr source)
{
    CHECKP(source);
    shft size=CHILDCOUNT(source);
    schema_node_cptr scn=GETSCHEMENODE(XADDR(source));
    xptr* childx=(xptr*)((char*)XADDR(source)+size_of_node(GETBLOCKBYNODE(source)));
    for (int i=0;i<size;i++)
    {
        if(*childx!=XNULL && scn->get_child_type(i)!=attribute && scn->get_child_type(i)!=xml_namespace )
        {
            xptr node=*childx;
            CHECKP(node);
            xptr left=GETLEFTPOINTER(node);
            if (left==XNULL) return node;
            CHECKP(left);
            t_item typ=GETTYPE((GETBLOCKBYNODE(left))->snode);
            if (typ==attribute ||typ==xml_namespace) { CHECKP(node); return node;}
            CHECKP(source);
        }
        childx+=1;
    }
    return XNULL;
}

/*returns the first  child  in dm:children accessor + attributes*/
xptr  getFirstByOrderNNSChildNode(xptr source)
{
    CHECKP(source);
    shft size=CHILDCOUNT(source);
    schema_node_cptr scn=GETSCHEMENODE(XADDR(source));
    xptr* childx=(xptr*)((char*)XADDR(source)+size_of_node(GETBLOCKBYNODE(source)));
    for (int i=0;i<size;i++)
    {
        if(*childx!=XNULL  && scn->get_child_type(i)!=xml_namespace )
        {
            xptr node=*childx;
            CHECKP(node);
            xptr left=GETLEFTPOINTER(node);
            if (left==XNULL) return node;
            CHECKP(left);
            t_item typ=GETTYPE((GETBLOCKBYNODE(left))->snode);
            if (typ==xml_namespace)return node;
            CHECKP(source);
        }
        childx+=1;
    }
    return XNULL;
}

/*returns the first child by document order of any type*/
xptr giveFirstByOrderChild(xptr source,shft size)
{
    xptr* childx=(xptr*)((char*)XADDR(source)+size_of_node(GETBLOCKBYNODE(source)));
    for (int i=0;i<size;i++)
    {
        if (*childx!=XNULL)
        {
            xptr node=*childx;
            CHECKP(node);
            xptr left=GETLEFTPOINTER(node);
            if (left==XNULL) return node;
            CHECKP(source);
        }
        childx+=1;
    }
    return XNULL;
}
xptr giveFirstByOrderChild_CP(xptr source,shft size)
{
    CHECKP(source);
    return giveFirstByOrderChild(source,size);
}

xptr giveAnyChild(n_dsc* source,shft size)
{

    xptr* childx=(xptr*)((char*)source+size_of_node(GETBLOCKBYNODE_ADDR(source)));
    for (int i=0;i<size;i++)
    {
        if(*childx!=XNULL) return *childx;
        childx+=1;
    }
    return XNULL;
}

/*returns any non-attribute child of the current node*/
xptr giveAnyNonAttributeChild(n_dsc* source,shft size)
{
    schema_node_cptr scn=GETSCHEMENODE(source);
    xptr* childx=(xptr*)((char*)source+size_of_node(GETBLOCKBYNODE_ADDR(source)));
    for (int i=0;i<size;i++)
    {
        if(*childx!=XNULL && scn->get_child_type(i)!=attribute) return *childx;
        childx+=1;
    }
    return XNULL;
}
/*returns any  child of the current node from the dm:children() sequence*/
xptr giveAnyDmChildrenChild(n_dsc* source,shft size)
{
    schema_node_cptr scn=GETSCHEMENODE(source);
    xptr* childx=(xptr*)((char*)source+size_of_node(GETBLOCKBYNODE_ADDR(source)));
    for (int i=0;i<size;i++)
    {
        if(*childx!=XNULL && scn->get_child_type(i)!=attribute && scn->get_child_type(i)!=xml_namespace) return *childx;
        childx+=1;
    }
    return XNULL;
}
/*returns any  child of the current node not from the dm:children() sequence*/
xptr giveAnyNonDmChildrenChild(n_dsc* source,shft size)
{
    schema_node_cptr scn=GETSCHEMENODE(source);;
    xptr* childx=(xptr*)((char*)source+size_of_node(GETBLOCKBYNODE_ADDR(source)));
    for (int i=0;i<size;i++)
    {
        if(*childx!=XNULL && (scn->get_child_type(i)==attribute||scn->get_child_type(i)==xml_namespace)) return *childx;
        childx+=1;
    }
    return XNULL;
}
/*returns last  child of the current node not from the dm:children() sequence*/
xptr getLastNonDmChildrenChild(n_dsc* source,shft size)
{
    schema_node_cptr scn=GETSCHEMENODE(source);;
    xptr* childx=(xptr*)((char*)source+size_of_node(GETBLOCKBYNODE_ADDR(source))+(size-1)*sizeof(xptr));
    for (int i=size-1;i>-1;i--)
    {
        if(*childx!=XNULL && (scn->get_child_type(i)==attribute||scn->get_child_type(i)==xml_namespace))
        {
            xptr node= *childx;
            CHECKP(node);
            xptr right=GETRIGHTPOINTER(node);
            while (right!=XNULL)
            {
                CHECKP(right);
                t_item typ= (GETBLOCKBYNODE(right))->snode->type;
                if (typ!=attribute && typ!=xml_namespace) return node;
                node=right;
                right=GETRIGHTPOINTER(node);
            }
            return node;
        }
        childx-=1;
    }
    return XNULL;
}


/*returns any attribute child of the current node*/
xptr giveAnyAttributeChild(n_dsc* source,shft size)
{
    schema_node_cptr scn=GETSCHEMENODE(source);;
    xptr* childx=(xptr*)((char*)source+size_of_node(GETBLOCKBYNODE_ADDR(source)));
    for (int i=0;i<size;i++)
    {
        if(*childx!=XNULL && scn->get_child_type(i)==attribute) return *childx;
        childx+=1;
    }
    return XNULL;
}
/*returns last  namespace child*/
xptr getLastNamespaceChild(n_dsc* source,shft size)
{
    schema_node_cptr scn=GETSCHEMENODE(source);;
    xptr* childx=(xptr*)((char*)source+size_of_node(GETBLOCKBYNODE_ADDR(source)));
    for (int i=0;i<size;i++)
    {
        if(*childx!=XNULL && scn->get_child_type(i)==xml_namespace)
        {
            xptr node=*childx;
            xptr tmp;
            while ((tmp=getNextSiblingOfSameSortXptr(node))!=XNULL)
            node=tmp;
            return node;
        }
        childx+=1;
    }
    return XNULL;
}

int getNearestBorder(node_blk_hdr * block,n_dsc* node)
{
    shft i=0;
    n_dsc*  med=(n_dsc*)((char*)block+block->desc_first);
    while (i<block->count /2)
    {
        if (med==node) return -1;
        med=GETNEXTDESCRIPTOR_BL(block,med);
        i++;
    }
    return 1;

}

xptr findMedianNodeDescriptor (node_blk_hdr * block)
{
    shft i=2;
    n_dsc*  med=(n_dsc*)((char*)block+block->desc_first);
    while (i<block->count)
    {
        med=GETNEXTDESCRIPTOR_BL(block,med);
        i+=2;
    }
    return ADDR2XPTR(med);
}
n_dsc* getPreviousDescriptorOfSameSort(n_dsc* node)
{
    if (node->desc_prev!=0) return GETPREVIOUSDESCRIPTOR(node);
    else
    {
        xptr new_block = GETBLOCKBYNODE_ADDR(node)->pblk;
        node_blk_hdr* bl_head=(node_blk_hdr*)XADDR(new_block);
        while (new_block!=XNULL)
        {
            CHECKP(new_block);
            if (bl_head->count!=0)
                    break;
            new_block = bl_head->pblk;
            bl_head=(node_blk_hdr*)XADDR(new_block);
        }
        if (new_block==XNULL) return NULL;
        if (bl_head->desc_last!=0)
            return GETPOINTERTODESC(bl_head,bl_head->desc_last);
        else
            throw SYSTEM_EXCEPTION("Bad Consistency: Empty block");
    }
}

/* returns the next in document order descriptor corresponding to the same scheme node*/
n_dsc* getNextDescriptorOfSameSort(n_dsc* node)
{
    if (node->desc_next!=0) return GETNEXTDESCRIPTOR(node);
    else
    {
        xptr new_block = GETBLOCKBYNODE_ADDR(node)->nblk;
        node_blk_hdr* bl_head=(node_blk_hdr*)XADDR(new_block);
        while (new_block!=XNULL)
        {
            CHECKP(new_block);

            if (bl_head->count!=0)
                    break;
            new_block = bl_head->nblk;
            bl_head=(node_blk_hdr*)XADDR(new_block);
        }
        if (new_block==XNULL) return NULL;

        if (bl_head->desc_first!=0)
            return GETPOINTERTODESC(bl_head,bl_head->desc_first);
        else
            throw SYSTEM_EXCEPTION("Bad Consistency: Empty block");
    }
}

/* returns the xptr to the first child of the node identified by name and type*/
xptr getChildPointer(n_dsc* node,const char* name,t_item type,xmlns_ptr ns)
{
    xptr* shift= elementContainsChild(node,name,type,ns);
    if (shift!=NULL)
        return *shift;
    else
        return XNULL;
}

xptr isAttributePointerSet(n_dsc* node,const char* name,const char* uri)
{
    node_blk_hdr* block=GETBLOCKBYNODE_ADDR(node);
    schema_node_cptr scm_node=block->snode;
    sc_ref_item* sc=scm_node->children.first;
    xptr child;
    int cnt=-1;
    while (sc!=NULL)
    {
        ++cnt;
        if (
             sc->object.type==attribute &&
             my_strcmp(name,sc->object.name)==0 &&
             (    (sc->object.get_xmlns()==NULL && NULL == uri)
               || (sc->object.get_xmlns()!=NULL && my_strcmp(uri,sc->object.get_xmlns()->uri)==0)) &&
             (block->dsc_size >= size_of_node(block)+(cnt+1) * sizeof(xptr)) &&
             ((child = GET_NODE_CHILD(node, cnt)) != XNULL)
         )
            return child;

        sc=sc->next;
    }
    return XNULL;
}
/* returns the xptr to the nearest left neighboring descriptor of the attribute*/
xptr findAttributeWithSameNameToInsertAfter_CP(xptr parent,const char* name,xmlns_ptr ns)
{
    CHECKP(parent);
    return findAttributeWithSameNameToInsertAfter(parent,name,ns);
}
xptr findAttributeWithSameNameToInsertAfter(xptr parent,const char* name,xmlns_ptr ns)
{
    n_dsc* tmp=((n_dsc*)XADDR(parent));
    while ( getChildPointer(tmp,name,attribute,ns)==XNULL)
    {
        tmp= getPreviousDescriptorOfSameSort(tmp) ;
        if (tmp==NULL) return XNULL;
    }
    return getChildPointer(tmp,name,attribute,ns);
}
xptr findNodeWithSameNameToInsertAfter_CP(xptr left_sib, xptr right_sib, xptr parent, const char* name,t_item node_type,xmlns_ptr ns)
{
    CHECKP(parent);
    return findNodeWithSameNameToInsertAfter(left_sib, right_sib, parent, name,node_type,ns);
}
/* returns the xptr to the nearest left neighboring descriptor*/
xptr findNodeWithSameNameToInsertAfter(xptr left_sib, xptr right_sib, xptr parent, const char* name,t_item node_type,xmlns_ptr ns)
{
    n_dsc* tmp=((n_dsc*)XADDR(parent));
    n_dsc* tmp2;
    while ( getChildPointer(tmp,name,node_type,ns)==XNULL)
    {
        tmp= getPreviousDescriptorOfSameSort(tmp) ;
        if (tmp==NULL) return XNULL;
    }
    if (tmp!=((n_dsc*)XADDR(parent)))
    {
        xptr indirect=getChildPointer(tmp,name,node_type,ns);
        CHECKP(indirect);
        tmp=(n_dsc*)XADDR(indirect);
        indirect= tmp->pdsc;
        n_dsc* tmp2;
        xptr tmpx1 = ADDR2XPTR(tmp);
        while (1==1)
        {
            tmp2=getNextDescriptorOfSameSort(tmp) ;
            if (tmp2==NULL || tmp2->pdsc!=indirect) break;
            else {tmpx1=ADDR2XPTR(tmp2);tmp=tmp2;}
        }
        CHECKP(tmpx1);
        return tmpx1;
    }
    else
    {
        xptr tmpx=getChildPointer(tmp,name,node_type,ns);
        CHECKP(tmpx);
        n_dsc* curn=(n_dsc*)XADDR(tmpx);
        xptr indir=curn->pdsc;
        if (left_sib!=XNULL && right_sib==XNULL)
        {
            while (true)
            {
                tmp2=getNextDescriptorOfSameSort(curn);
                if (tmp2==NULL) return ADDR2XPTR(curn);
                if (tmp2->pdsc!=indir)
                {
                    n_dsc *tt = getPreviousDescriptorOfSameSort(tmp2);
                    return ADDR2XPTR(tt);
                }
                curn=tmp2;
            }
        }
        xptr test_sib=left_sib;
        if (test_sib==XNULL) test_sib=right_sib;
        else
            if (tmpx==left_sib)
            {
                CHECKP(tmpx);
                return left_sib;
            }
        while (nid_cmp(tmpx,test_sib)<0)
        {
            CHECKP(tmpx);
            tmp2=getNextDescriptorOfSameSort((n_dsc*)XADDR(tmpx));
            if (tmp2!=NULL)
            {
                if (tmp2->pdsc!=indir) return ADDR2XPTR(getPreviousDescriptorOfSameSort(tmp2));
                tmpx=ADDR2XPTR(tmp2);
            }
            else
                break;
        }
        if (nid_cmp(tmpx,test_sib)<0)
        {
            CHECKP(tmpx);
            return tmpx;
        }
        else
        {
            CHECKP(tmpx);
            tmp=getPreviousDescriptorOfSameSort((n_dsc*)XADDR(tmpx));
            if (tmp!=NULL)
            {
                if (ADDR2XPTR(tmp)==right_sib) tmp=getPreviousDescriptorOfSameSort((n_dsc*)XADDR(tmpx));
                if (tmp!=NULL)
                return ADDR2XPTR(tmp);
                else return XNULL;
            }
            else
                return XNULL;
        }
    }
    return XNULL;
}

/* returns the xptr to the nearest right neighboring descriptor of the attribute*/
xptr findAttributeWithSameNameToInsertBefore_CP(xptr parent,const char* name,xmlns_ptr ns)
{
    CHECKP(parent);
    return findAttributeWithSameNameToInsertBefore(parent,name,ns);
}
xptr findAttributeWithSameNameToInsertBefore(xptr parent,const char* name,xmlns_ptr ns)
{
    n_dsc* tmp=((n_dsc*)XADDR(parent));
    while ( getChildPointer(tmp,name,attribute,ns)==XNULL)
    {
        tmp= getNextDescriptorOfSameSort(tmp) ;
        if (tmp==NULL) return XNULL;
    }
    return getChildPointer(tmp,name,attribute,ns);
}

/* returns the xptr to the nearest right neighboring descriptor*/
xptr findNodeWithSameNameToInsertBefore_CP(xptr left_sib, xptr right_sib, xptr parent,const  char* name,t_item node_type,xmlns_ptr ns)
{
    CHECKP(parent);
    return findNodeWithSameNameToInsertBefore(left_sib, right_sib, parent, name,node_type,ns);
}
xptr findNodeWithSameNameToInsertBefore(xptr left_sib, xptr right_sib, xptr parent, const char* name,t_item node_type,xmlns_ptr ns)
{
    n_dsc* tmp=((n_dsc*)XADDR(parent));
    n_dsc* tmp2;
    CHECKP(parent);
    while ( getChildPointer(tmp,name,node_type,ns)==XNULL)
    {
        tmp= getNextDescriptorOfSameSort(tmp) ;
        if (tmp==NULL) return XNULL;
    }
    if (tmp!=((n_dsc*)XADDR(parent)))
    {
        xptr res=getChildPointer(tmp,name,node_type,ns);
        CHECKP(res);
        return res;
    }
    else
    {
        xptr tmpx=getChildPointer(tmp,name,node_type,ns);
        xptr test_sib=right_sib;
        if (test_sib==XNULL) test_sib=left_sib;
        else
            if (tmpx==right_sib)
            {
                CHECKP(tmpx);
                return right_sib;
            }
        while (nid_cmp(tmpx,test_sib)<0)
        {
            CHECKP(tmpx);
            tmp2=getNextDescriptorOfSameSort((n_dsc*)XADDR(tmpx));
            if (tmp2!=NULL)
                tmpx=ADDR2XPTR(tmp2);
            else
                return XNULL;
        }
        CHECKP(tmpx);
        return tmpx;
    }
}


/* returns shift position either element descriptor contains pointer to the child of that type -1 otherwise*/
xptr* elementContainsChild(n_dsc* parent,const char* name,t_item type,xmlns_ptr ns)
{
    node_blk_hdr* block=GETBLOCKBYNODE_ADDR(parent);
    schema_node_cptr scm_node=block->snode;
    int posit=scm_node->find_first_child(ns,name,type);
    if (posit<0) return NULL;
    if (block->dsc_size<((shft)size_of_node(block)+((shft)posit+1)*((shft)sizeof(xptr))))
        return NULL;
    else
        return (xptr*)((char*)parent+(shft)size_of_node(block)+(shft)posit*((shft)sizeof(xptr)));
}
/* utils for persistent string library */
xptr    getLeftmostDescriptorWithPstrInThisBlock(xptr blk, xptr node)
{
    CHECKP(node);
    t_item type=GETTYPE(GETSCHEMENODEX(node));
    if (type==element || type==xml_namespace)
        throw SYSTEM_EXCEPTION("Wrong type of node:  either element or namespace");
    t_dsc* node_d=(t_dsc*) XADDR(node);
    t_dsc* node_a=node_d;
    t_dsc* node_l;
    while (
           (node_l=(t_dsc*)getPreviousDescriptorOfSameSort(node_d))!=NULL &&
           ((node_l->data==XNULL)||node_l->size>PSTRMAXSIZE||(BLOCKXPTR(node_l->data)==blk))
           )
    {
           node_d=node_l;
           if (node_l->data!=XNULL) node_a=node_l;
    }
    return ADDR2XPTR(node_a);
}

xptr    getRightmostDescriptorWithPstrInThisBlock(xptr blk,xptr node)
{
    CHECKP(node);
    t_item type=GETTYPE(GETSCHEMENODEX(node));
    if (type==element || type==xml_namespace)
        throw SYSTEM_EXCEPTION("Wrong type of node:  either element or namespace");
    t_dsc* node_d=(t_dsc*) XADDR(node);
    t_dsc* node_a=node_d;
    t_dsc* node_l;
    while (
           (node_l=(t_dsc*)getNextDescriptorOfSameSort(node_d))!=NULL &&
           (node_l->data==XNULL||node_l->size>PSTRMAXSIZE||BLOCKXPTR(node_l->data)==blk)
           )
    {
           node_d=node_l;
           if (node_l->data!=XNULL) node_a=node_l;

    }
    return ADDR2XPTR(node_a);
}

/* returns the next in document order descriptor corresponding to the same scheme node in xptr*/
xptr getNextDescriptorOfSameSortXptr(xptr nodex)
{
    CHECKP(nodex);
    n_dsc* node= (n_dsc*)XADDR(nodex);
    node=getNextDescriptorOfSameSort(node);
    if (node!=NULL)
        return ADDR2XPTR(node);
    else return XNULL;
}
/* returns the previous in document order descriptor corresponding to the same scheme node in xptr*/
xptr getPreviousDescriptorOfSameSortXptr(xptr nodex)
{
    CHECKP(nodex);
    n_dsc* node= (n_dsc*)XADDR(nodex);
    node=getPreviousDescriptorOfSameSort(node);
    if (node!=NULL)
        return ADDR2XPTR(node);
    else return XNULL;
}

xptr getNextSiblingOfSameSortXptr(xptr nodex)
{
    CHECKP(nodex);
    n_dsc* node= (n_dsc*)XADDR(nodex);
    xptr parent = node->pdsc;
    n_dsc* next_node = getNextDescriptorOfSameSort(node);
    return (next_node!=NULL&&next_node->pdsc == parent) ? ADDR2XPTR(next_node) : XNULL;
}

n_dsc* getNextSiblingOfSameSort(n_dsc* node)
{
    xptr parent = node->pdsc;
    n_dsc* next_node = getNextDescriptorOfSameSort(node);
    return (next_node!=NULL&&next_node->pdsc == parent) ? next_node : NULL;
}

void getSchemeDescendantsOrSelf(schema_node_cptr scm,const char* uri,const char* name, t_item type, comp_schema cfun, vector<schema_node_xptr> &result)
{
    //if (scm->type==type && my_strcmp(scm->name,name)==0 &&((uri==NULL && scm->get_xmlns()==NULL) || (scm->get_xmlns()!=NULL && my_strcmp(scm->get_xmlns()->uri,uri)==0 )))
    if (cfun(scm,uri,name,type))
        result.push_back(scm.ptr());
    getSchemeDescendants(scm,uri,name,type,cfun,result);

}
void getSchemeDescendants(schema_node_cptr scm,const char* uri,const char* name, t_item type,  comp_schema cfun,vector<schema_node_xptr> &result)
{
    sc_ref_item* sc=scm->children.first;
    while (sc!=NULL)
    {
//      if (my_strcmp(name,sc->object.name)==0 && sc->object.type==type &&((uri==NULL && sc->get_xmlns()==NULL) || (sc->get_xmlns()!=NULL && my_strcmp(sc->get_xmlns()->uri,uri)==0 ))) result.push_back(sc->object.snode);
        if (cfun(sc->object.snode,uri,name,type)) result.push_back(sc->object.snode);
        getSchemeDescendants(sc->object.snode,uri,name,type,cfun,result);
        sc=sc->next;
    }
}
int getChildrenXptr(const xptr& parent,const char* uri,const char* name, t_item type, comp_schema cfun,xptr& first, xptr*& res)
{
    CHECKP(parent);
    int chcnt=CHILDCOUNT(parent);
    xptr* ptr=(xptr*)((char*)XADDR(parent)+size_of_node(GETBLOCKBYNODE(parent)));
    int cur=0;
    int ctr=0;
    schema_node_cptr scm=GETSCHEMENODEX(parent);
    sc_ref_item* sc=scm->children.first;
    while (cur<chcnt && sc!=NULL)
    {
//      if (my_strcmp(name,sc->object.name)==0 && sc->object.type==type &&((uri==NULL && sc->get_xmlns()==NULL) || (sc->get_xmlns()!=NULL && my_strcmp(sc->get_xmlns()->uri,uri)==0 ))) result.push_back(sc->object.snode);
        if (*ptr!=XNULL && cfun(sc->object.snode,uri,name,type))
        {
            if (ctr)
            {
                if (ctr==1)
                {
                    res=se_new xptr[chcnt];
                    res[0]=first;
                }
                res[ctr++]=*ptr;
            }
            else
            {
                first=*ptr;
                ctr++;
            }
        }
        sc=sc->next;
        ptr+=1;
        cur++;
    }
    if (!ctr)
            first=XNULL;
    return ctr;
}
void getSchemeChilds(schema_node_cptr scm,const char* uri,const char* name, t_item type,  comp_schema cfun,vector<schema_node_xptr> &result)
{
    sc_ref_item* sc=scm->children.first;
    while (sc!=NULL)
    {
//      if (my_strcmp(name,sc->object.name)==0 && sc->object.type==type &&((uri==NULL && sc->get_xmlns()==NULL) || (sc->get_xmlns()!=NULL && my_strcmp(sc->get_xmlns()->uri,uri)==0 ))) result.push_back(sc->object.snode);
        if (cfun(sc->object.snode,uri,name,type)) result.push_back(sc->object.snode);
        sc=sc->next;
    }
}
xptr scanDescandants(xptr node,vector<schema_node_xptr> &path,int posit)
{
    if (posit==0) return XNULL;
    xptr child=getChildPointer((n_dsc*)XADDR(node),path[posit-1]->name,path[posit-1]->type,path[posit-1]->get_xmlns());
    if (child==XNULL) return XNULL;
    xptr indir=((n_dsc*)XADDR(node))->indir;
    CHECKP(child);
    if (posit==1) return child;
    n_dsc* tmp=(n_dsc*)XADDR(child);
    while (true)
    {
        xptr res=scanDescandants(ADDR2XPTR(tmp),path,posit-1);
        if (res!=XNULL) return res;
        tmp=getNextDescriptorOfSameSort(tmp);
        if (tmp==NULL || tmp->pdsc!=indir)
        {
            CHECKP(node);
            return XNULL;
        }
    }
}
/*xptr getFirstDescandantByScheme(xptr ancestor,schema_node_cptr scm)
{
    CHECKP(ancestor);
    schema_node_cptr sc_anc=(GETBLOCKBYNODE(ancestor))->snode;
    if(sc_anc==scm) return ancestor;
    vector<schema_node_xptr> path;
    schema_node_cptr tmp=scm;
    while (tmp!=sc_anc)
    {
        path.push_back(tmp);
        tmp=tmp->parent;
        if (tmp==NULL) return XNULL;
    }
    path.push_back(sc_anc);
    return scanDescandants(ancestor,path,path.size()-1);
}*/
xptr getFirstDescandantByScheme(xptr ancestor,schema_node_cptr scm)
{
    CHECKP(ancestor);
    schema_node_cptr sc_anc=(GETBLOCKBYNODE(ancestor))->snode;
    if(sc_anc==scm) return ancestor;
    vector<schema_node_xptr> path;
    schema_node_cptr tmp=scm;

    while (tmp.ptr()!=sc_anc.ptr())
    {
        path.push_back(tmp.ptr());
        tmp=tmp->parent;
        if (!tmp.found()) return XNULL;
    }

    vector<schema_node_xptr>::iterator it=path.end()-1;
    CHECKP(ancestor);
    xptr child=getChildPointer((n_dsc*)XADDR(ancestor),(*it)->name,(*it)->type,(*it)->get_xmlns());
    if (child==XNULL) return XNULL;

    while (true)
    {
        if (it==path.begin())
                return child;
        --it;
        CHECKP(child);
        n_dsc * ch_ptr=(n_dsc*)XADDR(child);

        while (ch_ptr!=NULL)
        {
            xptr grchild=getChildPointer(ch_ptr,(*it)->name,(*it)->type,(*it)->get_xmlns());
            if (grchild!=XNULL)
            {
                //1.check ancestor-descendant with ancestor
                if (ADDR2XPTR(ch_ptr)==child||nid_cmp_effective(grchild,ancestor)==2)
                    child=grchild;
                else return XNULL;
                break;
            }
            else
            {
                if (ch_ptr->desc_next!=0)
                    ch_ptr=getNextDescriptorOfSameSort(ch_ptr);
                else
                {
                    xptr tmp = ADDR2XPTR(ch_ptr);
                    if (nid_cmp_effective(tmp,ancestor)!=2)
                        return XNULL;
                    else
                    {
                        CHECKP(tmp);
                        ch_ptr=getNextDescriptorOfSameSort(ch_ptr);
                    }
                }
            }
        }
        if (ch_ptr==NULL)
            return XNULL;
    }
}
xptr getNextDescandantofSameSort (xptr ancestor,xptr node)
{
    CHECKP(node);
    n_dsc* desc=getNextDescriptorOfSameSort((n_dsc*) XADDR(node));
    if (desc!=NULL)
    {
        xptr res=ADDR2XPTR(desc);
        if (nid_ancestor(ancestor,res)) return res;
        else return XNULL;
    }
    else return XNULL;
}
xptr getFirstAttributeDescendantAndFillPath(std::vector<xptr> &descstack)
{
    xptr node=descstack[descstack.size()-1];
    CHECKP(node);
    xptr child=getFirstByOrderElementChild(node);
    xptr attr=XNULL;
    while (child!=XNULL)
    {
        descstack.push_back(child);
        attr=getFirstByOrderAttributeChild(child);
        if (attr!=XNULL) return attr;
        attr=getFirstAttributeDescendantAndFillPath(descstack);
        if (attr!=XNULL) return attr;
        descstack.pop_back();
        child=getNextByOrderElement(child);
    }
    return XNULL;
}

/* returns the inderection pointer to the ancestor of the descriptor that corresponds
to the selected ancestor by scheme */
xptr getAncestorIndirectionByScheme (n_dsc* node, const schema_node_cptr scm_node, const schema_node_cptr scm_anc)
{
    if (scm_node==scm_anc) return node->indir;
    schema_node_cptr tmp=scm_node;
    n_dsc* tmp_node=node;
    xptr tmp_x;
    while (tmp->parent!=scm_anc.ptr())
    {
        tmp_x=removeIndirection(tmp_node->pdsc);
        CHECKP(tmp_x);
        tmp_node=(n_dsc*)XADDR(tmp_x);
        if (/*tmp==tmp->parent*/ tmp->parent==XNULL) return XNULL;
        tmp=tmp->parent;
    }
    return tmp_node->pdsc;
}

xptr getNodeAncestorBySchemeCP(xptr node, schema_node_xptr scm_node, schema_node_xptr scm_anc)
{
    while (scm_node != scm_anc) {
        node = getParentCP(node);
        if (node == XNULL) return XNULL;
        scm_node = getBlockHeader(node)->snode;
    }
    return node;
}


/*comparison function for schema nodes*/
 bool comp_type(schema_node_cptr scm,const char* uri,const char* name, t_item type)
 {
     return scm->type==type;
 }
 bool comp_qname_type(schema_node_cptr scm,const char* uri,const char* name, t_item type)
 {
     return
         (scm->type==type && my_strcmp(scm->name,name)==0 &&
           ( (uri==NULL && scm->get_xmlns()==NULL) || 
             (scm->get_xmlns()!=NULL && my_strcmp(scm->get_xmlns()->uri,uri)==0) )) ;
 }
 bool comp_local_type(schema_node_cptr scm,const char* uri,const char* name, t_item type)
 {
     return
         (scm->type==type &&
         my_strcmp(scm->name,name)==0 );
 }
 bool comp_uri_type(schema_node_cptr scm,const char* uri,const char* name, t_item type)
 {
     return
         (scm->type==type &&
         (
         (uri==NULL && scm->get_xmlns()==NULL) ||
         (scm->get_xmlns()!=NULL && my_strcmp(scm->get_xmlns()->uri,uri)==0 )
         )) ;
 }




bool is_text(t_item t)
{
    return t == text;
}

bool is_node(t_item t)
{
    return (   t == element
            || t == text
            || t == attribute
            || t == document
            || t == xml_namespace
            || t == comment
            || t == pr_ins);
}

bool dm_children_accessor_filter(t_item t)
{
    return (   t == element
            || t == text
            || t == comment
            || t == pr_ins);
}

bool dm_attribute_accessor_filter(t_item t)
{
    return t == attribute;
}
/*returns the next non-descendant node in document*/
xptr getNextNDNode(xptr node)
{
    xptr tmp=node;
    while(true)
    {
        CHECKP(tmp);
        if (((n_dsc*)XADDR(tmp))->rdsc!=XNULL)
        {
            if ((GETBLOCKBYNODE(tmp))->snode->parent->type==virtual_root) return XNULL;
            else
                return ((n_dsc*)XADDR(tmp))->rdsc;
        }
        else
        {
            tmp=((n_dsc*)XADDR(tmp))->pdsc;
            if (tmp==XNULL)
                return XNULL;
            else
                tmp=removeIndirection(tmp);
        }
    }
}
/*returns the next node in document */
xptr getNextDONode(xptr node)
{
    xptr tmp=getFirstByOrderChildNode(node);
    if (tmp!=XNULL) return tmp;
    else
        return getNextNDNode(node);

}
/*prereq: node is checked*/
bool hasLeftSiblingDM(xptr node)
{
    if (((n_dsc*)XADDR(node))->ldsc==XNULL)return false;
    xptr tmp=((n_dsc*)XADDR(node))->ldsc;
    CHECKP(tmp);
    bool res= dm_children_accessor_filter((GETBLOCKBYNODE(tmp))->snode->type);
    CHECKP(node);
    return res;
}
/*returns the previous node in document*/
xptr getPreviousDONode(xptr node)
{
    xptr tmp=node;
    while(true)
    {
        CHECKP(tmp);
        if (hasLeftSiblingDM(tmp))
        {
            if ((GETBLOCKBYNODE(tmp))->snode->parent->type==virtual_root)
                return XNULL;
            else
                break;
        }
        else
        {
            return removeIndirection(((n_dsc*)XADDR(tmp))->pdsc);
        }
    }
    tmp=((n_dsc*)XADDR(tmp))->ldsc;
    xptr tmp2;
    while(true)
    {
        CHECKP(tmp);
        tmp2=getLastByOrderChildNode(tmp);
        if (tmp2==XNULL || !dm_children_accessor_filter((GETBLOCKBYNODE(tmp2))->snode->type))
        {
            CHECKP(tmp);
            return tmp;
        }
        else
            tmp=tmp2;
    }
}
int getMedianDescriptor(int s,int r,node_blk_hdr* block,n_dsc* end, n_dsc** res)
{
    int cnt=(r-s)/2;
    *res=end;
    for (int i=0;i<cnt;i++)
        *res=GETPOINTERTODESC(block,(*res)->desc_prev);
    return r-cnt;
}
int getMedianDescriptor2(int s,int r,node_blk_hdr* block,n_dsc* start, n_dsc** res)
{
    int cnt=(r-s)/2;
    *res=start;
    for (int i=0;i<cnt;i++)
        *res=GETPOINTERTODESC(block,(*res)->desc_next);
    return s+cnt;
}
/*returns the next non-descendant node in document that fits input schema_node */
xptr getNextNDNode(xptr node,schema_node_cptr scn)
{
    xptr blk=scn->bblk;
    node_blk_hdr* block;
    //1.finding block
    while (blk!=XNULL)
    {
        CHECKP(blk);
        block=((node_blk_hdr*)XADDR(blk));
        n_dsc* nd=GETPOINTERTODESC(block,block->desc_last);
        if (block->desc_last!=0 && nid_cmp_effective(ADDR2XPTR(nd),node)==1)
            break;
        else
        {
            CHECKP(blk);
            blk=block->nblk;
            if (blk==XNULL) return XNULL;
        }
    }
    //2. finding node in block
    CHECKP(blk);
    if (nid_cmp_effective(ADDR2XPTR(GETPOINTERTODESC(block,block->desc_first)),node)==1)
    {
        CHECKP(blk);
        return ADDR2XPTR(GETPOINTERTODESC(block,block->desc_first));
    }
    CHECKP(blk);
    int s=0;
    int r=block->count;
    //n_dsc* left=GETPOINTERTODESC(block,block->desc_first);
    n_dsc* right=GETPOINTERTODESC(block,block->desc_last);
    n_dsc* med;
    while (s<r-1)
    {
        int i=getMedianDescriptor(s,r,block,right,&med);
        if (nid_cmp_effective(ADDR2XPTR(med),node)==1)
        {
            r=i;
            right=med;
        }
        else
        {
            s=i;
        }
        CHECKP(blk);
    }
    return ADDR2XPTR(right);
}
/*returns the previous node in document that fits input schema_node*/
/*xptr getPreviousDONode(xptr node,schema_node* scn)
{
    xptr blk=scn->bblk;
    node_blk_hdr* block;
    //1.finding block
    while (blk!=XNULL)
    {
        CHECKP(blk);
        block=((node_blk_hdr*)XADDR(blk));
        n_dsc* nd=GETPOINTERTODESC(block,block->desc_last);
        if (nid_cmp_effective(ADDR2XPTR(nd),node)>=0)
            break;
        else
        {
            CHECKP(blk);
            blk=block->nblk;
            if (blk==XNULL)
            {
                return ADDR2XPTR(GETPOINTERTODESC(block,block->desc_last));
            }
        }
    }
    //2. finding node in block
    CHECKP(blk);
    if (nid_cmp_effective(ADDR2XPTR(GETPOINTERTODESC(block,block->desc_first)),node)>=0)
    {
        CHECKP(blk);
        return getPreviousDescriptorOfSameSortXptr(ADDR2XPTR(GETPOINTERTODESC(block,block->desc_first)));
    }
    CHECKP(blk);
    int s=0;
    int r=block->count;
    n_dsc* left=GETPOINTERTODESC(block,block->desc_first);
    //n_dsc* right=GETPOINTERTODESC(block,block->desc_last);
    n_dsc* med;
    while (s<r-1)
    {
        int i=getMedianDescriptor2(s,r,block,left,&med);
        if (nid_cmp_effective(ADDR2XPTR(med),node)>=0)
        {
            r=i;
        }
        else
        {
            s=i;
            left=med;
        }
        CHECKP(blk);
    }
    return ADDR2XPTR(left);
}*/
/*returns the previous non-ancestor node in document that fits input schema_node*/
xptr getPreviousNANode(xptr node,schema_node_cptr scn)
{
    xptr blk=scn->bblk;
    node_blk_hdr* block;
    //1.finding block
    xptr lastn=XNULL;
    while (blk!=XNULL)
    {
        CHECKP(blk);
        block=((node_blk_hdr*)XADDR(blk));
        n_dsc* nd=GETPOINTERTODESC(block,block->desc_last);
        if (block->desc_last!=0 )
        {
            lastn= ADDR2XPTR(nd);
            if  (nid_cmp_effective(lastn,node)!=-1)
                break;
        }
        CHECKP(blk);
        blk=block->nblk;
        if (blk==XNULL)
        {
                return lastn;
        }

    }
    //2. finding node in block
    CHECKP(blk);
    if (nid_cmp_effective(ADDR2XPTR(GETPOINTERTODESC(block,block->desc_first)),node)!=-1)
    {
        CHECKP(blk);
        return getPreviousDescriptorOfSameSortXptr(ADDR2XPTR(GETPOINTERTODESC(block,block->desc_first)));
    }
    CHECKP(blk);
    int s=0;
    int r=block->count;
    n_dsc* left=GETPOINTERTODESC(block,block->desc_first);
    //n_dsc* right=GETPOINTERTODESC(block,block->desc_last);
    n_dsc* med;
    while (s<r-1)
    {
        int i=getMedianDescriptor2(s,r,block,left,&med);
        if (nid_cmp_effective(ADDR2XPTR(med),node)!=-1)
        {
            r=i;
        }
        else
        {
            s=i;
            left=med;
        }
        CHECKP(blk);
    }
    return ADDR2XPTR(left);
}
/*returns the next sibling node in document that fits input schema_node */
xptr getNextSiblingNode(xptr node,schema_node_cptr scn)
{
    CHECKP(node);
    xptr indir=((n_dsc*)XADDR(node))->pdsc;
    xptr parent;
    if (indir!=XNULL)
        parent=removeIndirection(indir);
    else
        return XNULL;
    xptr child=getChildPointerXptr(parent,scn->name,scn->type,scn->get_xmlns());
    while (child!=XNULL)
    {
        if (nid_cmp_effective(child,node)>0)return child;
        else
        {
            child=getNextDescriptorOfSameSortXptr(child);
            if (child!=XNULL && ((n_dsc*)XADDR(child))->pdsc!=indir)
                return XNULL;
        }
    }
    return XNULL;

}
/*returns the previous sibling node in document that fits input schema_node */
xptr getPreviousSiblingNode(xptr node,schema_node_cptr scn)
{
    CHECKP(node);
    xptr indir=((n_dsc*)XADDR(node))->pdsc;
    xptr parent;
    if (indir!=XNULL)
        parent=removeIndirection(indir);
    else
        return XNULL;
    xptr child=getChildPointerXptr(parent,scn->name,scn->type,scn->get_xmlns());
    if (nid_cmp_effective(child,node)>=0)return XNULL;
    while (child!=XNULL)
    {
        xptr child2=getNextDescriptorOfSameSortXptr(child);
        if (child2==XNULL || (((n_dsc*)XADDR(child2))->pdsc!=indir ||nid_cmp_effective(child2,node)>=0))
            return child;
        else
            child=child2;
    }
    return XNULL;
}
bool is_scmnode_has_ancestor_or_self(schema_node_cptr scm_node, std::set<schema_node_xptr>* scm_nodes_set )
{
    while((scm_node->type != document) && (scm_node.found()))
    {
        if(scm_nodes_set->find(scm_node.ptr()) != scm_nodes_set->end()) return true;
        scm_node = scm_node->parent;
    }
    return false;
}

/* return base uri if exists*/
xptr getBaseUri(xptr node)
{
    xptr tmp=node;
    while (tmp!=XNULL)
    {
        CHECKP(tmp);
        node_blk_hdr* block=GETBLOCKBYNODE(tmp);
        schema_node_cptr scm_node=block->snode;
        sc_ref_item* sc=scm_node->children.first;
        int cnt=-1;
        while (sc!=NULL)
        {
            ++cnt;
            if (sc->object.type==attribute && sc->object.get_xmlns()!=NULL && my_strcmp("base",sc->object.name)==0  && my_strcmp("xml",sc->object.get_xmlns()->prefix)==0)
            {
                if (
                    block->dsc_size>=((shft)size_of_node(block)+((shft)cnt+1)*
                                    ((shft)sizeof(xptr)))
                                    &&
                    ((*(xptr*)(((char*)XADDR(tmp))+(shft)size_of_node(block)+(shft)cnt*
                    ((shft)sizeof(xptr))))!=XNULL))
                    return (*(xptr*)(((char*)XADDR(tmp))+(shft)size_of_node(block)+
                         (shft)cnt*((shft)sizeof(xptr))));
            }
        sc=sc->next;
    }
    tmp=removeIndirection(((n_dsc*)XADDR(tmp))->pdsc);
    }
    return XNULL;

}

xptr getRoot(xptr node)
{
    xptr cur = node;
    xptr p;
    CHECKP(cur);
    while (true)
    {
        p = GETPARENTPOINTER(cur);
        if (p == XNULL) return cur;
        p = removeIndirection(p);
        CHECKP(p);
        if (GETSCHEMENODE(XADDR(p))->type == virtual_root)
            return cur;
        cur = p;
    }
}


xptr getNextNonemptyBlock(xptr p)
{
    node_blk_hdr* x = getBlockHeaderCP(p);

    do {
        p = x->nblk;
        if (p == XNULL) { return XNULL; }
        x = getBlockHeaderCP(p);
    } while (x->count == 0);

    return p;
}

xptr getPrevNonemptyBlock(xptr p)
{
    node_blk_hdr* x = getBlockHeaderCP(p);

    do {
        p = x->pblk;
        if (p == XNULL) { return XNULL; }
        x = getBlockHeaderCP(p);
    } while (x->count == 0);

    return p;
}


/*
    Function scans the block chain foreward starting from the given one, skipping all empty ones.
    Side effect: blocks are mapped to memory (CHECKP).
    If there are no unempty blocks, function returns XNULL.
*/

xptr getNonemptyBlockLookFore(xptr p)
{
    node_blk_hdr* x;

    while (p != XNULL) {
        CHECKP(p);
        x = (node_blk_hdr*) XADDR(p);

        if (x->count > 0) return p;
        p = x->nblk;
    }

    return p;
}

/*
    Function scans the block chain backward starting from the given one, skipping all empty ones.
    Side effect: blocks are mapped to memory (CHECKP).
    If there are no unempty blocks, function returns XNULL.
*/

xptr getNonemptyBlockLookBack(xptr p)
{
    node_blk_hdr* x;

    while (p != XNULL) {
        CHECKP(p);
        x = (node_blk_hdr*) XADDR(p);

        if (x->count > 0) return p;
        p = x->pblk;
    }

    return p;
}

shft size_of_node(t_item type)
{
    switch(type)
    {
      case element:return (shft)sizeof(e_dsc);
      case document: case virtual_root: return (shft)sizeof(d_dsc);
      case attribute:return (shft)sizeof(a_dsc);
      case text: case comment: case cdata: return (shft)sizeof(t_dsc);
      case xml_namespace:return (shft)sizeof(ns_dsc);
      case pr_ins:return (shft)sizeof(pi_dsc);
    }
    return (shft)sizeof(n_dsc);
}

shft size_of_node(node_blk_hdr* block)
{
    t_item type=GETTYPE(block->snode);
    switch(type)
    {
    case element:return (shft)sizeof(e_dsc);
    case document: case virtual_root: return (shft)sizeof(d_dsc);
    case attribute:return (shft)sizeof(a_dsc);
    case text: case comment: case cdata: return (shft)sizeof(t_dsc);
    case xml_namespace:return (shft)sizeof(ns_dsc);
    case pr_ins:return (shft)sizeof(pi_dsc);
    }
    return (shft)sizeof(n_dsc);
}

