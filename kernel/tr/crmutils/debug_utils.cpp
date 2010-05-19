/*
* File:  debug_utils.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <math.h>
#include <map>

#include "common/sedna.h"
#include "common/u/uutils.h"

#include "tr/crmutils/crmutils.h"
#include "tr/crmutils/crminternals.h"
#include "tr/structures/schema.h"
#include "tr/vmm/vmm.h"
#include "tr/pstr/pstrblk.h"
#include "tr/crmutils/node_utils.h"
#include "tr/structures/metadata.h"
#include "tr/mo/mo.h"

/* predefined debug & error output stream */
se_stdlib_ostream crm_dbg(std::cerr);

/* some statistics counters */
struct debug_info
{
    long schema_count;
    long schema_str_count;
    long block_count;
    long block_fill;
    long inner_block_count;

    int64_t inner_block_fill;
    float inner_fill_percentage;
    float fill_percentage;
    int64_t node_count;
    int64_t ext_nid_count;
    int64_t str_size;
    long mdpth;           //max depth
    long cdp;             //current depth
};


static xptr
insertStatisticsInteger(xptr neighbour,const char* title, char* buf,int64_t count)
{
    xptr left=insert_element_i(neighbour,XNULL,XNULL,title,xs_untyped,NULL_XMLNS);
    u_i64toa(count,buf,10);
    insert_text_i(XNULL,XNULL,left,buf,strlen(buf));
    return left;
}

static xptr
insertStatisticsDouble(xptr neighbour,const char* title, char* buf,double count)
{
    xptr left=insert_element_i(neighbour,XNULL,XNULL,title,xs_untyped,NULL_XMLNS);
    u_gcvt(count,10,buf);
    insert_text_i(XNULL,XNULL,left,buf,strlen(buf));
    return left;
}


static void
insertNidAndStringsStatistics(xptr broot, xptr node)
{
    std::map<unsigned int,unsigned int> nidsz;
    std::map<unsigned int,unsigned int> strsz;
    std::map<schema_node_xptr,int> xsfo;
    std::map<schema_node_xptr,int> ncnt;
    std::vector<int> fo;
    int maxfo=0;
    int64_t midfo=0;
    unsigned int cnt=0;
    unsigned int strcnt=0;

    xptr root = broot;
    while (root!=XNULL)
    {
        xptr node = root;
        fo.push_back(0);
        while(node!=XNULL)
        {
            cnt++;
            //size count
            CHECKP(node);
            //scheme cnt
            schema_node_xptr sc =(GETBLOCKBYNODE(node))->snode;
            std::map<schema_node_xptr,int>::iterator cit=ncnt.find(sc);
            if (cit!=ncnt.end())
            {
                ncnt[sc]++;
            }
            else
                ncnt[sc]=1;
            t_nid& nd=((n_dsc*)XADDR(node))->nid;
            int sz=(nd.size>0)?nd.size:*((shft*)(nd.prefix+sizeof(xptr)));
            if (nidsz.find(sz)==nidsz.end())
                nidsz[sz]=1;
            else
                nidsz[sz]++;
            //string cnt
            if (sc->has_text())
            {
                strcnt++;
                unsigned int strz = getTextSize((t_dsc*)XADDR(node));
                if (strsz.find(strz)==strsz.end())
                    strsz[strz]=1;
                else
                    strsz[strz]++;
            }
            //next node
            xptr tmp= giveFirstByOrderChild(node,CHILDCOUNT(node));
            while (tmp==XNULL /*&& GETPARENTPOINTER(node)!=XNULL*/)
            {
                if (fo.back()>maxfo) maxfo=fo.back();
                midfo+=fo.back();
                schema_node_xptr scm =(GETBLOCKBYNODE(node))->snode;
                std::map<schema_node_xptr,int>::iterator it=xsfo.find(scm);
                if (it!=xsfo.end())
                {
                    if (fo.back()>xsfo[scm]) xsfo[scm]=fo.back();
                }
                else
                    xsfo[scm]=fo.back();
                fo.pop_back();
                tmp=GETRIGHTPOINTER(node);
                if (tmp==XNULL)
                {
                    if (GETPARENTPOINTER(node)==XNULL)break;
                    node=removeIndirection(GETPARENTPOINTER(node));
                    CHECKP(node);
                }
            }
            node=tmp;
            if(node!=XNULL)
            {
                fo.back()++;
                fo.push_back(0);
            }
        }
        root=getNextDescriptorOfSameSortXptr(root);
    }

    xptr parent=insert_element_i(node,XNULL,XNULL,"NID",xs_untyped,NULL_XMLNS);
    char buf[20];
    u_itoa(maxfo,buf,10);
    xptr left=insert_attribute_i(XNULL,XNULL,parent,"max_fan_out",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    u_gcvt((1.*midfo/cnt),10,buf);
    left=insert_attribute_i(left,XNULL,XNULL,"mid_fan_out",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);

    //counting total Nid
    int64_t cnt_sz=0;
    int64_t cnt_out=0;
    unsigned int max=0;
    std::map<unsigned int,unsigned int>::const_iterator it=nidsz.begin();
    while (it!=nidsz.end())
    {
        cnt_sz+=it->second*it->first;
        if (it->first>11)
            cnt_out+=it->second*it->first;
        if (it->first>max) max=it->first;
        it++;
    }
    u_gcvt((cnt_sz/1024./1024.),10,buf);
    left=insert_attribute_i(left,XNULL,XNULL,"total_size",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    u_gcvt((cnt_out/1024./1024.),10,buf);
    left=insert_attribute_i(left,XNULL,XNULL,"total_size_ext",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);


    left=insert_element_i(left,XNULL,XNULL,"histogram",xs_untyped,NULL_XMLNS);
    xptr lf=XNULL;
    int i=1;
    for (i=1;i<=max;i++)
    {
        it=nidsz.find(i);
        if (it!=nidsz.end())
        {
            lf=insert_element_i(lf,XNULL,left,"bucket",xs_untyped,NULL_XMLNS);
            u_itoa(i,buf,10);
            xptr att=insert_attribute_i(XNULL,XNULL,lf,"size",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
            u_itoa(it->second,buf,10);
            att=insert_attribute_i(att,XNULL,XNULL,"total",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
            u_gcvt((100.*it->second/cnt),10,buf);
            insert_attribute_i(att,XNULL,XNULL,"percentage",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
        }
    }
    nidsz.clear();

    //counting total String
    cnt_sz=0;
    max=0;
    it=strsz.begin();
    while (it!=strsz.end())
    {
        cnt_sz+=it->second*it->first;
        if (it->first>max) max=it->first;
        it++;
    }
    parent=insert_element_i(parent,XNULL,XNULL,"STRINGS",xs_untyped,NULL_XMLNS);
    u_gcvt((cnt_sz/1024./1024.),10,buf);
    left=insert_attribute_i(XNULL,XNULL,parent,"total_size",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    u_itoa(strcnt,buf,10);
    left=insert_attribute_i(left,XNULL,XNULL,"total_count",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    left=insert_element_i(left,XNULL,XNULL,"histogram",xs_untyped,NULL_XMLNS);
    lf=XNULL;
    for (i=0;i<=max;i++)
    {
        it=strsz.find(i);
        if (it!=strsz.end())
        {
            lf=insert_element_i(lf,XNULL,left,"bucket",xs_untyped,NULL_XMLNS);
            u_itoa(i,buf,10);
            xptr att=insert_attribute_i(XNULL,XNULL,lf,"size",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
            u_itoa(it->second,buf,10);
            att=insert_attribute_i(att,XNULL,XNULL,"total",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
            u_gcvt((100.*it->second/strcnt),10,buf);
            insert_attribute_i(att,XNULL,XNULL,"percentage",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);


        }
    }
    strsz.clear();
}

static void
getSimpleDebugInfo(schema_node_cptr snode, debug_info* d_in)
{
    d_in->schema_count++;
    if (snode->type!=element && snode->type!=xml_namespace)
        d_in->schema_str_count++;
    d_in->cdp++;
    xptr block = snode->bblk;

    while (block!=XNULL)
    {
        CHECKP(block);
        node_blk_hdr* blk=GETBLOCKBYNODE(block);
        d_in->block_fill+=blk->count*blk->dsc_size;
        d_in->block_count++;
        d_in->node_count+=blk->count;
        if (blk->nblk!=XNULL)
        {
            d_in->inner_block_fill+=blk->count*blk->dsc_size;
            d_in->inner_block_count++;
        }

        n_dsc* node= (n_dsc*)((char*)blk+blk->desc_first);
        while (node!=NULL)
        {
            if (node->nid.size==0)
            {
                d_in->ext_nid_count += *(shft*)(node->nid.prefix+sizeof(xptr));
            }
            if (blk->snode->type!=element && blk->snode->type!=xml_namespace)
            {
                d_in->str_size += getTextSize((t_dsc*)node);
            }
            if (node->desc_next!=0)
                node = (n_dsc*)((char*)blk+node->desc_next);
            else
                node=NULL;
        }
        block=blk->nblk;
    }
    sc_ref_item* sc= snode->children->first;
    while (sc!=NULL)
    {
        getSimpleDebugInfo(sc->object.snode, d_in);
        sc=sc->next;
    }
    if (d_in->cdp>d_in->mdpth) d_in->mdpth=d_in->cdp;
    d_in->cdp--;
}

static inline void
error_msg(const char * ermsg,se_ostream& crmout)
{
    crmout << ermsg;
}



void getDebugInfo(schema_node_cptr snode, xptr& node)
{
    debug_info d_in = {};
    getSimpleDebugInfo(snode, (debug_info*)&d_in);
    d_in.fill_percentage=100.*d_in.block_fill/(d_in.block_count*(PAGE_SIZE-sizeof(node_blk_hdr)));
    d_in.inner_fill_percentage=100.*d_in.inner_block_fill/(d_in.inner_block_count*(PAGE_SIZE-sizeof(node_blk_hdr)));
    char buf[40];

    xptr left=insert_element_i(XNULL,XNULL,node,"total_schema_nodes",xs_untyped,NULL_XMLNS);
    u_itoa(d_in.schema_count,buf,10);
    insert_text_i(XNULL,XNULL,left,buf,strlen(buf));

    left=insertStatisticsInteger(left,"total_schema_text_nodes",buf,d_in.schema_str_count);
    left=insertStatisticsInteger(left,"schema_depth",buf,d_in.mdpth);
    left=insertStatisticsInteger(left,"total_nodes",buf,d_in.node_count);
    left=insertStatisticsInteger(left,"total_desc_blk",buf,d_in.block_count);
    left=insertStatisticsDouble (left,"saturation",buf,d_in.fill_percentage);
    left=insertStatisticsDouble (left,"innr_blk_saturation",buf,d_in.inner_fill_percentage);
    left=insertStatisticsInteger(left,"total_innr_blk",buf,d_in.inner_block_count);
    left=insertStatisticsInteger(left,"total_innr_size",buf,d_in.inner_block_count*PAGE_SIZE);
    left=insertStatisticsDouble (left,"descriptor_size",buf,(0.0625*d_in.block_count));
    left=insertStatisticsDouble (left,"nids_size",buf,(0.00000095367431640625*d_in.ext_nid_count));

    left=insert_element_i(left,XNULL,XNULL,"distribution",xs_untyped,NULL_XMLNS);

    if (snode->bblk!=XNULL)
    {
        CHECKP(snode->bblk);
        insertNidAndStringsStatistics(GETBLOCKFIRSTDESCRIPTORABSOLUTE((XADDR(snode->bblk))), left);
    }
}

void checkTreeConsistency(xptr node,se_ostream& crmout)
{
    CHECKP(node);
    n_dsc* node_d=(n_dsc*)XADDR(node);
    t_nid nd=node_d->nid;
    schema_node_cptr scn=(GETBLOCKBYNODE(node))->snode;
    //1. indirection test
    xptr indir=node_d->indir;
    if (removeIndirection(indir)!=node)
        error_msg("Indirection table error",crmout);
    //2. parent test
    CHECKP(node);
    xptr par_indir=node_d->pdsc;
    xptr parent;
    n_dsc* prev_dsc=getPreviousDescriptorOfSameSort(node_d);
    xptr prev_x=(prev_dsc==NULL)?XNULL:ADDR2XPTR(prev_dsc);
    if (par_indir!=XNULL &&(prev_dsc==NULL|| prev_dsc->pdsc!=par_indir))
    {
        parent=removeIndirection(par_indir);
        CHECKP(parent);
        xptr* ptr=elementContainsChild((n_dsc*)XADDR(parent),scn->name,scn->type,scn->get_xmlns());
        if (ptr==NULL || *ptr!=node)
            error_msg("First child pointer",crmout);
    }
    //3. left siblings + nid comparison
    CHECKP(node);
    xptr left=node_d->ldsc;
    if (left!=XNULL)
    {
        CHECKP(left);
        if (((n_dsc*)XADDR(left))->rdsc!=node)
            error_msg("sibling pointer errors",crmout);
        if (nid_cmp(left,node)>=0)
            error_msg("nid comparison error",crmout);
    }
    //4. descriptor's order
    if (prev_x!=XNULL)
    {
        CHECKP(prev_x);
        if (getNextDescriptorOfSameSort(prev_dsc)!=node_d)
            error_msg("wrong descriptors", crmout);
    }
    //recursive walkthrough
    CHECKP(node);
    xptr child=giveFirstByOrderChild(node,CHILDCOUNT(node));
    while (child!=XNULL)
    {
        checkTreeConsistency(child,crmout);
        CHECKP(child);
        child=((n_dsc*)XADDR(child))->rdsc;
    }
}
