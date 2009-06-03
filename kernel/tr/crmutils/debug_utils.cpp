/*
 * File:  debug_utils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include <math.h>
#include "tr/crmutils/crmutils.h"
#include "tr/structures/schema.h"
#include "tr/vmm/vmm.h"
#include "tr/pstr/pstrblk.h"
#include "tr/structures/indirection.h"
#include "tr/crmutils/node_utils.h"
#include "tr/structures/metadata.h"
#include "tr/mo/micro.h"


void printMFO (schema_node_cptr node, std::map<schema_node_xptr, std::pair<int,int> > &mfo,int par_pref,int indent)
{
    int size=1;
    int increment=0;
    if (node->parent!=NULL)
    {
        size=par_pref+mfo[node->parent].first;
        increment=mfo[node->parent].second;
    }
    crm_out << "\n";
    for (int i=0;i<indent;i++) crm_out << " ";
    crm_out << "<NODE ";// << node;
    if (node->name!=NULL)
        crm_out << "local_name=\"" << node->name <<"\"";
    if (node->get_xmlns()!=NULL)
    {
        if (node->get_xmlns()->prefix!=NULL)
            crm_out << " prefix=\"" << node->get_xmlns()->prefix <<"\"";
        else
            crm_out << " prefix=\"\"";
        if (node->get_xmlns()->uri!=NULL)
            crm_out << " uri=\"" << node->get_xmlns()->uri <<"\"";     
        else
            crm_out << " uri=\"http://www.w3.org/XML/1998/namespace\"";
    }
    crm_out << " type=\"" <<convert_type(node->type)<<"\"";
    //crm_out << " fan-out=\"" <<((node->children.first)?mfo[node]:0 )<<"\"";
    crm_out << " pref_length=\"" <<size <<"\"";
    crm_out << " incr=\"" <<increment <<"\"";
    //recursive walkthrough
    sc_ref_item* sc= node->children.first;
    while (sc!=NULL)
    {
        printMFO(sc->object.snode,mfo,size,indent+1);
        sc=sc->next;
    }       
}
void getNIDNEWDistribution(std::map<schema_node_xptr,int> &xsfo,
                           std::map<schema_node_xptr,int> &ncnt,
                           std::map<int,int> &nidsz,
                           schema_node_cptr node, int par_pref)
{
    int size=1;
    if (node->parent!=NULL)
        size=par_pref+
        (int)ceil(s_max (log10(1.*xsfo[node->parent])/log10((double)ALPHABET_SIZE),1.));
    std::map<int,int>::iterator it= nidsz.find(size);
    if (it!=nidsz.end())
        it->second+=ncnt[node.ptr()];
    else
        nidsz[size]=ncnt[node.ptr()];
    //recursive walkthrough
    sc_ref_item * sc= node->children.first;
    while (sc!=NULL)
    {
        getNIDNEWDistribution(xsfo,ncnt,nidsz,sc->object.snode,size);
        sc=sc->next;
    }       
}
void printNIDVariation(xptr root, se_ostream& crmout)
{
    std::map<int,int> nidsz;
    std::map<schema_node_xptr,int> xsfo;
    std::map<schema_node_xptr,int> ncnt;
    std::vector<int> fo;
    int maxfo=0;
    __int64 midfo=0;
    int cnt=0;
    xptr node=root;
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
        //next node
        xptr tmp= giveFirstByOrderChild(node,CHILDCOUNT(node));
        while (tmp==XNULL /*&& GETPARENTPOINTER(node)!=XNULL*/)
        {
            if (fo.back()>maxfo) maxfo=fo.back();
            midfo+=fo.back();
            //CHECKP(root)
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
    /*  else
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
        }*/
    }
    crmout<<"\n max Fan-out="<<maxfo;
    crmout<<"\n mid Fan-out="<<(1.*midfo/cnt);
    //counting total
    __int64 cnt_sz=0;
    __int64 cnt_out=0;
    int max=0;
    std::map<int,int>::const_iterator it=nidsz.begin();
    while (it!=nidsz.end())
    {
        cnt_sz+=it->second*it->first;
        if (it->first>11)
            cnt_out+=it->second*it->first;
        if (it->first>max) max=it->first;
        it++;
    }
    crmout<<"\n Nid size total,MGB="<<(cnt_sz/1024./1024.);
    crmout<<"\n Nid size external total,MGB="<<(cnt_out/1024./1024.);
    crmout<<"\n Nid size distribution";
    for (int i=1;i<=max;i++)
    {
        it=nidsz.find(i);
        if (it!=nidsz.end())
            crmout<<"\n "<<i<<"{--} "<<(100.*it->second/cnt)<<"%"<<"  total="<<it->second;
        else
            crmout<<"\n "<<i<<"{--} 0%" <<"  total=0";
    }
    nidsz.clear();
    CHECKP(root);
    schema_node_xptr sc =(GETBLOCKBYNODE(root))->snode;
/*  getNIDNEWDistribution(xsfo,ncnt,nidsz,sc,0);
    cnt_sz=0;
    cnt_out=0;
    max=0;
    it=nidsz.begin();
    while (it!=nidsz.end())
    {
        cnt_sz+=it->second*it->first;
        if (it->first>11)
            cnt_out+=it->second*it->first;
        if (it->first>max) max=it->first;
        it++;
    }
    crmout<<"\n Theory Nid size total,MGB="<<(cnt_sz/1024./1024.);
    crmout<<"\n Theory Nid size external total,MGB="<<(cnt_out/1024./1024.);
    crmout<<"\n Theory Nid size distribution";
    for (int i=1;i<=max;i++)
    {
        it=nidsz.find(i);
        if (it!=nidsz.end())
            crmout<<"\n "<<i<<"<--> "<<(100.*it->second/cnt)<<"%"<<"  total="<<it->second;
        else
            crmout<<"\n "<<i<<"<--> 0%" <<"  total=0";
    }
    */

}
void printFullNIDVariation(xptr broot, se_ostream& crmout)
{
    std::map<int,int> nidsz;
    std::map<int,int> strsz;
    std::map<schema_node_xptr,int> xsfo;
    std::map<schema_node_xptr,int> ncnt;
    std::vector<int> fo;
    int maxfo=0;
    __int64 midfo=0;
    int cnt=0;
    int strcnt=0;
    xptr root=broot;
    while (root!=XNULL)
    {
        xptr node=root;
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
            if (sc->type!=element &&sc->type!=xml_namespace)
            {
                strcnt++;
                int strz=((t_dsc*)XADDR(node))->size;
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
            /*  else
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
        }*/
        }
        root=getNextDescriptorOfSameSortXptr(root);
    }
    crmout<<"\n max Fan-out="<<maxfo;
    crmout<<"\n mid Fan-out="<<(1.*midfo/cnt);
    //counting total Nid
    __int64 cnt_sz=0;
    __int64 cnt_out=0;
    int max=0;
    std::map<int,int>::const_iterator it=nidsz.begin();
    while (it!=nidsz.end())
    {
        cnt_sz+=it->second*it->first;
        if (it->first>11)
            cnt_out+=it->second*it->first;
        if (it->first>max) max=it->first;
        it++;
    }
    crmout<<"\n Nid size total,MGB="<<(cnt_sz/1024./1024.);
    crmout<<"\n Nid size external total,MGB="<<(cnt_out/1024./1024.);
    crmout<<"\n Nid size distribution";
        int i=1;
    for (i=1;i<=max;i++)
    {
        it=nidsz.find(i);
        if (it!=nidsz.end())
            crmout<<"\n "<<i<<"{--} "<<(100.*it->second/cnt)<<"%"<<"  total="<<it->second;
        else
            crmout<<"\n "<<i<<"{--} 0%" <<"  total=0";
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
    crmout<<"\n String size total,MGB="<<(cnt_sz/1024./1024.);
    crmout<<"\n Strings total="<<strcnt;
    crmout<<"\n Strings size distribution";
    for (i=0;i<=max;i++)
    {
        it=strsz.find(i);
        if (it!=strsz.end())
            crmout<<"\n "<<i<<"{--} "<<(100.*it->second/strcnt)<<"%"<<"  total="<<it->second;
        else
            crmout<<"\n "<<i<<"{--} 0%" <<"  total=0";
    }
    strsz.clear();

    

}
void printFullNIDVariation(xptr broot, xptr node)
{
    std::map<unsigned int,unsigned int> nidsz;
    std::map<unsigned int,unsigned int> strsz;
    std::map<schema_node_xptr,int> xsfo;
    std::map<schema_node_xptr,int> ncnt;
    std::vector<int> fo;
    int maxfo=0;
    __int64 midfo=0;
    unsigned int cnt=0;
    unsigned int strcnt=0;
    xptr root=broot;
    while (root!=XNULL)
    {
        xptr node=root;
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
            if (sc->type!=element &&sc->type!=xml_namespace)
            {
                strcnt++;
                unsigned int strz=((t_dsc*)XADDR(node))->size;
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
            /*  else
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
        }*/
        }
        root=getNextDescriptorOfSameSortXptr(root);
    }
    xptr parent=insert_element(node,XNULL,XNULL,"NID",xs_untyped,NULL_XMLNS);
    char buf[20];
    u_itoa(maxfo,buf,10);
    xptr left=insert_attribute(XNULL,XNULL,parent,"max_fan_out",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    u_gcvt((1.*midfo/cnt),10,buf);
    left=insert_attribute(left,XNULL,XNULL,"mid_fan_out",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    
    //counting total Nid
    __int64 cnt_sz=0;
    __int64 cnt_out=0;
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
    left=insert_attribute(left,XNULL,XNULL,"total_size",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    u_gcvt((cnt_out/1024./1024.),10,buf);
    left=insert_attribute(left,XNULL,XNULL,"total_size_ext",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    
    
    left=insert_element(left,XNULL,XNULL,"histogram",xs_untyped,NULL_XMLNS);
    xptr lf=XNULL;
    int i=1;
    for (i=1;i<=max;i++)
    {
        it=nidsz.find(i);
        if (it!=nidsz.end())
        {
            lf=insert_element(lf,XNULL,left,"bucket",xs_untyped,NULL_XMLNS);
            u_itoa(i,buf,10);
            xptr att=insert_attribute(XNULL,XNULL,lf,"size",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
            u_itoa(it->second,buf,10);
            att=insert_attribute(att,XNULL,XNULL,"total",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
            u_gcvt((100.*it->second/cnt),10,buf);
            insert_attribute(att,XNULL,XNULL,"percentage",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
}
        else
        {
            /*
            lf=insert_element(lf,XNULL,left,"bucket",xs_untyped,NULL);
            u_itoa(i,buf,10);
            xptr att=insert_attribute(XNULL,XNULL,lf,"size",xs_untypedAtomic,buf,strlen(buf),XNULL);
            att=insert_attribute(att,XNULL,XNULL,"total",xs_untypedAtomic,"0",1,NULL);
            insert_attribute(att,XNULL,XNULL,"percentage",xs_untypedAtomic,"0",1,NULL);
            */
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
    parent=insert_element(parent,XNULL,XNULL,"STRINGS",xs_untyped,NULL_XMLNS);
    u_gcvt((cnt_sz/1024./1024.),10,buf);
    left=insert_attribute(XNULL,XNULL,parent,"total_size",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    u_itoa(strcnt,buf,10);
    left=insert_attribute(left,XNULL,XNULL,"total_count",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    left=insert_element(left,XNULL,XNULL,"histogram",xs_untyped,NULL_XMLNS);
    lf=XNULL;
    for (i=0;i<=max;i++)
    {
        it=strsz.find(i);
        if (it!=strsz.end())
        {
            lf=insert_element(lf,XNULL,left,"bucket",xs_untyped,NULL_XMLNS);
            u_itoa(i,buf,10);
            xptr att=insert_attribute(XNULL,XNULL,lf,"size",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
            u_itoa(it->second,buf,10);
            att=insert_attribute(att,XNULL,XNULL,"total",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
            u_gcvt((100.*it->second/strcnt),10,buf);
            insert_attribute(att,XNULL,XNULL,"percentage",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
        
            
        }
        else
        {
            /*lf=insert_element(lf,XNULL,left,"bucket",xs_untyped,NULL);
            u_itoa(i,buf,10);
            xptr att=insert_attribute(XNULL,XNULL,lf,"size",xs_untypedAtomic,buf,strlen(buf),NULL);
            att=insert_attribute(att,XNULL,XNULL,"total",xs_untypedAtomic,"0",1,NULL);
            insert_attribute(att,XNULL,XNULL,"percentage",xs_untypedAtomic,"0",1,NULL);*/
        }
    }
    strsz.clear();

    

}
#ifdef VMM_GATHER_STATISTICS
void printDebugInfo(schema_node_cptr snode, se_ostream& crmout)
{
    debug_info d_in;
    getDebugInfo(snode,(debug_info*)&d_in);
    d_in.fill_percentage=100.*d_in.block_fill/(d_in.block_count*(PAGE_SIZE-sizeof(node_blk_hdr)));
    d_in.inner_fill_percentage=100.*d_in.inner_block_fill/(d_in.inner_block_count*(PAGE_SIZE-sizeof(node_blk_hdr)));
    crmout<<"TOTAL NODES IN SCHEMA="<<d_in.schema_count;
    crmout<<"TOTAL NODES="<<d_in.node_count;    
    crmout<<"\nDEPTH="<<d_in.mdpth;
    crmout<<"\n\n TOTAL BLOCKS="<< vmm_data_block_count;
    crmout<<"\n TOTAL DESCRIPTOR BLOCKS="<< d_in.block_count;
    crmout<<"\n TOTAL STRING BLOCKS ="<<d_in.str_blocks;
    crmout<<"\n TOTAL NID BLOCKS ="<<nid_block_count;
    crmout<<"\n TOTAL INDIR BLOCKS ="<<indir_block_count;
    crmout<<"\n TOTAL LEAKED BLOCKS="<<(vmm_data_block_count-nid_block_count-indir_block_count-d_in.block_count-d_in.str_blocks);
    //crmout<<"\n TOTAL SIZE="<<(d_in.block_count*PAGE_SIZE);
    
    
    crmout<<"\n\n FILL PERCENTAGE="<<d_in.fill_percentage;
    crmout<<"\n TOTAL INNER BLOCKS="<< d_in.inner_block_count;
    crmout<<"\n TOTAL INNER NODES SIZE="<<(d_in.inner_block_count*PAGE_SIZE);
    crmout<<"\n INNER FILL PERCENTAGE="<<d_in.inner_fill_percentage;
        
    
    crmout<<"\n\n STRINGS percentage="<<(100.*d_in.str_blocks/vmm_data_block_count)<<"%";
    crmout<<"\n DESCRIPTORS percentage="<<(100.*d_in.block_count/vmm_data_block_count)<<"%";
    crmout<<"\n NIDS percentage="<<(100.*nid_block_count/vmm_data_block_count)<<"%";
    crmout<<"\n INDIRECTION percentage="<<(100.*indir_block_count/vmm_data_block_count)<<"%";
    crmout<<"\n LEAKED BLOCKS percentage="<<(100./vmm_data_block_count*(vmm_data_block_count-nid_block_count-indir_block_count-d_in.block_count-d_in.str_blocks))<<"%";
    
    crmout<<"\n\n TOTAL SIZE Mgb="<<(0.0625*vmm_data_block_count);
    crmout<<"\n STRINGS size,MGB="<<(0.0625*d_in.str_blocks);
    crmout<<"\n DESCRIPTORS size,MGB="<<(0.0625*d_in.block_count);
    crmout<<"\n NIDS size,MGB="<<(0.0625*nid_block_count);
    crmout<<"\n INDIRECTION size,MGB="<<(0.0625*indir_block_count);
    crmout<<"\n LEAKED BLOCKS size,MGB="<<(0.0625*(vmm_data_block_count-nid_block_count-indir_block_count-d_in.block_count-d_in.str_blocks));

    if (snode->bblk!=XNULL)
    {
        CHECKP(snode->bblk);
        printNIDVariation(GETBLOCKFIRSTDESCRIPTORABSOLUTE((XADDR(snode->bblk))), crmout);
    }
    crmout<<"\n";
}
#endif
xptr fillStatistics(xptr neighbour,const char* title, char* buf,__int64 count)
{
    xptr left=insert_element(neighbour,XNULL,XNULL,title,xs_untyped,NULL_XMLNS);
#ifdef WIN32
    _i64toa(count,buf,10);
#else
    sprintf(buf,"%lld",count);
#endif
    insert_text(XNULL,XNULL,left,buf,strlen(buf));
    return left;
}
xptr fillStatistics2(xptr neighbour,const char* title, char* buf,double count)
{
    xptr left=insert_element(neighbour,XNULL,XNULL,title,xs_untyped,NULL_XMLNS);
    u_gcvt(count,10,buf);
    insert_text(XNULL,XNULL,left,buf,strlen(buf));
    return left;
}
void getDebugInfo(schema_node_cptr snode, xptr& node)
{
    debug_info d_in;
    getSimpleDebugInfo(snode,(debug_info*)&d_in);
    d_in.fill_percentage=100.*d_in.block_fill/(d_in.block_count*(PAGE_SIZE-sizeof(node_blk_hdr)));
    d_in.inner_fill_percentage=100.*d_in.inner_block_fill/(d_in.inner_block_count*(PAGE_SIZE-sizeof(node_blk_hdr)));
    char buf[40];
    
    xptr left=insert_element(XNULL,XNULL,node,"total_schema_nodes",xs_untyped,NULL_XMLNS);
    u_itoa(d_in.schema_count,buf,10);
    insert_text(XNULL,XNULL,left,buf,strlen(buf));

    left=fillStatistics(left,"total_schema_text_nodes",buf,d_in.schema_str_count);
    left=fillStatistics(left,"schema_depth",buf,d_in.mdpth);
    left=fillStatistics(left,"total_nodes",buf,d_in.node_count);
    left=fillStatistics(left,"total_desc_blk",buf,d_in.block_count);
    left=fillStatistics(left,"total_str_blk",buf,d_in.str_blocks);
    left=fillStatistics2(left,"saturation",buf,d_in.fill_percentage);
    left=fillStatistics2(left,"innr_blk_saturation",buf,d_in.inner_fill_percentage);
    left=fillStatistics(left,"total_innr_blk",buf,d_in.inner_block_count);
    left=fillStatistics(left,"total_innr_size",buf,d_in.inner_block_count*PAGE_SIZE);
    double dbc=0.0625*(d_in.str_blocks+d_in.block_count)+0.00000095367431640625*(d_in.ext_nid_count);
    left=fillStatistics2(left,"strings",buf,(100.*0.0625*d_in.str_blocks/dbc));
    left=fillStatistics2(left,"descriptors",buf,(100.*0.0625*d_in.block_count/dbc));
    /*left=fillStatistics2(left,"indirection",buf,(100.*0.00000095367431640625*sizeof(xptr)*d_in.node_count/dbc));*/
    left=fillStatistics2(left,"nid",buf,(100.*0.00000095367431640625*d_in.ext_nid_count/dbc));
    left=fillStatistics2(left,"total_size",buf,dbc);
    left=fillStatistics2(left,"string_size",buf,(0.0625*d_in.str_blocks));
    left=fillStatistics2(left,"descriptor_size",buf,(0.0625*d_in.block_count));
    left=fillStatistics2(left,"nids_size",buf,(0.00000095367431640625*d_in.ext_nid_count));
    left=fillStatistics2(left,"free_space_in_str_blocks",buf,(0.00000095367431640625*d_in.freestrspace));
    /*left=fillStatistics(left,"indirection_size",buf,(0.00000095367431640625*sizeof(xptr)*d_in.node_count));*/
    left=insert_element(left,XNULL,XNULL,"distribution",xs_untyped,NULL_XMLNS);
    if (snode->bblk!=XNULL)
    {
        CHECKP(snode->bblk);
        printFullNIDVariation(GETBLOCKFIRSTDESCRIPTORABSOLUTE((XADDR(snode->bblk))), left);
    }
    
}
void printSimpleDebugInfo(schema_node_cptr snode, se_ostream& crmout)
{
    debug_info d_in;
    getSimpleDebugInfo(snode,(debug_info*)&d_in);
    d_in.fill_percentage=100.*d_in.block_fill/(d_in.block_count*(PAGE_SIZE-sizeof(node_blk_hdr)));
    d_in.inner_fill_percentage=100.*d_in.inner_block_fill/(d_in.inner_block_count*(PAGE_SIZE-sizeof(node_blk_hdr)));
    crmout<<"\n TOTAL NODES IN SCHEMA="<<d_in.schema_count;
    crmout<<"\n TOTAL TEXT NODES IN SCHEMA="<<d_in.schema_str_count;
    crmout<<"TOTAL NODES="<<d_in.node_count;    
    crmout<<"\nDEPTH="<<d_in.mdpth;
    crmout<<"\n\n TOTAL BLOCKS EXCEPT NID and INDIR="<< d_in.block_count+d_in.str_blocks;
    crmout<<"\n TOTAL DESCRIPTOR BLOCKS="<< d_in.block_count;
    crmout<<"\n TOTAL STRING BLOCKS ="<<d_in.str_blocks;
    
        
    crmout<<"\n\n FILL PERCENTAGE="<<d_in.fill_percentage;
    crmout<<"\n TOTAL INNER BLOCKS="<< d_in.inner_block_count;
    crmout<<"\n TOTAL INNER NODES SIZE="<<(d_in.inner_block_count*PAGE_SIZE);
    crmout<<"\n INNER FILL PERCENTAGE="<<d_in.inner_fill_percentage;
    
    double dbc=0.0625*(d_in.str_blocks+d_in.block_count)+0.00000095367431640625*(/*sizeof(xptr)*d_in.node_count+*/d_in.ext_nid_count);

    crmout<<"\n\n STRINGS percentage="<<(100.*0.0625*d_in.str_blocks/dbc)<<"%";
    crmout<<"\n DESCRIPTORS percentage="<<(100.*0.0625*d_in.block_count/dbc)<<"%";
    /*crmout<<"\n NIDS percentage="<<(100.*0.00000095367431640625*sizeof(xptr)*d_in.node_count/dbc)<<"%";*/
    crmout<<"\n NIDS percentage="<<(100.*0.00000095367431640625*d_in.ext_nid_count/dbc)<<"%";
    

    crmout<<"\n\n TOTAL SIZE Mgb="<<dbc;
    crmout<<"\n STRINGS size,MGB="<<(0.0625*d_in.str_blocks);
    crmout<<"\n DESCRIPTORS size,MGB="<<(0.0625*d_in.block_count);
    crmout<<"\n NIDS size,MGB="<<(0.00000095367431640625*d_in.ext_nid_count);
    crmout<<"\n FREE SPACE IN STRING BLOCKS size,MGB="<<(0.00000095367431640625*d_in.freestrspace);
    /*crmout<<"\n INDIRECTION size,MGB="<<(0.00000095367431640625*sizeof(xptr)*d_in.node_count);*/
    if (snode->bblk!=XNULL)
    {
        CHECKP(snode->bblk);
        printFullNIDVariation(GETBLOCKFIRSTDESCRIPTORABSOLUTE((XADDR(snode->bblk))), crmout);
    }
    crmout<<"\n";
}
void getDebugInfo(schema_node_cptr snode, debug_info* d_in)
{
    d_in->schema_count++;
    d_in->cdp++;
    xptr block=snode->bblk;
    node_blk_hdr* bltxt=NULL;
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
        block=blk->nblk;
        if (blk->snode->type!=element && blk->snode->type!=xml_namespace)
        {
            t_dsc* node= (t_dsc*)((char*)blk+blk->desc_first);
            
            while (node!=NULL)
            {
                node_blk_hdr* tmp=GETBLOCKBYNODE(node->data);
                if (tmp!=NULL&& tmp!=bltxt)
                {
                    d_in->str_blocks++;
                    bltxt=tmp;
                }
                if (node->desc_next!=0)
                    node= (t_dsc*)((char*)blk+node->desc_next);
                else
                    node=NULL;
            }

        }
    }
    sc_ref_item* sc= snode->children.first;
    while (sc!=NULL)
    {
        getDebugInfo(sc->object.snode, d_in);
        sc=sc->next;
    }
    if (d_in->cdp>d_in->mdpth) d_in->mdpth=d_in->cdp;
    d_in->cdp--;
}
void getSimpleDebugInfo(schema_node_cptr snode, debug_info* d_in)
{
    d_in->schema_count++;
    if (snode->type!=element && snode->type!=xml_namespace)
        d_in->schema_str_count++;
    d_in->cdp++;
    xptr block=snode->bblk;
    node_blk_hdr* bltxt=NULL;
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
                d_in->ext_nid_count+=*(shft*)(node->nid.prefix+sizeof(xptr));
            }
            if (blk->snode->type!=element && blk->snode->type!=xml_namespace)
            {
                node_blk_hdr* tmp=GETBLOCKBYNODE(((t_dsc*)node)->data);
                if (tmp!=NULL&& tmp!=bltxt)
                {
                    d_in->str_blocks++;
                    CHECKP(((t_dsc*)node)->data);
                    d_in->freestrspace+=BFS(ADDR2XPTR(tmp));
                    bltxt=tmp;
                    CHECKP(block);
                }
            }
            if (node->desc_next!=0)
                node= (t_dsc*)((char*)blk+node->desc_next);
            else
                node=NULL;
        }
        block=blk->nblk;
    }
    sc_ref_item* sc= snode->children.first;
    while (sc!=NULL)
    {
        getSimpleDebugInfo(sc->object.snode, d_in);
        sc=sc->next;
    }
    if (d_in->cdp>d_in->mdpth) d_in->mdpth=d_in->cdp;
    d_in->cdp--;
}
void isSchemaPCAllRight(schema_node_cptr snode)
{
    sc_ref_item* sc= snode->children.first;
    while (sc!=NULL)
    {
        if (sc->object.snode->parent!=snode.ptr())
        {
            crm_out<<"Error";
        }
        isSchemaPCAllRight(sc->object.snode);
        sc=sc->next;
    }
}
//verifies that there is no two neighboring text nodes
void checkTextNodeCorrectness(xptr node)
{
    CHECKP(node);
    xptr left=GETLEFTPOINTER(node);
    xptr right=GETRIGHTPOINTER(node);
    if (left!=XNULL)
    {
        CHECKP(left);
        if (GETTYPE((GETBLOCKBYNODE(left))->snode)==text)
        {
            crm_out<<"ERROR-DEBUG-UTILS";
        }
    }
    if (right!=XNULL)
    {
        CHECKP(right);
        if (GETTYPE((GETBLOCKBYNODE(right))->snode)==text)
        {
            crm_out<<"ERROR-DEBUG-UTILS";
        }
    }
}
void checkChildReferenceValidity(xptr node)
{
    CHECKP(node);
    n_dsc* node_dsc=(n_dsc*)XADDR(node);
    n_dsc* prev_dsc=getPreviousDescriptorOfSameSort(node_dsc);
    bool test1;
    bool test2=true;
    xptr prev_par=XNULL;
    if (prev_dsc!=NULL)  prev_par=prev_dsc->pdsc;
    CHECKP(node);
    xptr parind=(node_dsc)->pdsc;
    test1=(prev_par==parind);
    CHECKP(parind);
    xptr parent=*((xptr*)XADDR(parind));
    CHECKP (parent);
    node_blk_hdr* block=GETBLOCKBYNODE(parent);
    xptr* childx=(xptr*)((char*)XADDR(parent)+size_of_node(block));
    int chcnt=COUNTREFERENCES(block,size_of_node(block));
    int cnt=0;
    while (cnt!=chcnt)
    {
      if(*childx==node) test2=false;
      cnt++;
      childx+=1;
    }
    if (test1!=test2)
    {
        childx=(xptr*)((char*)XADDR(parent)+size_of_node(block));
        cnt=0;
        while (cnt!=chcnt)
        {
            crm_out<<"\nAddr= "<<XADDR(*childx);
            childx+=1;
            cnt++;
        }
        crm_out<<"ERROR-DEBUG-UTILS-WP";
    }
    CHECKP(node);
}

void error_msg(const char * ermsg,se_ostream& crmout)
{
    crmout<< ermsg;
}
void checkBlockSequenceConsistency(schema_node_cptr snode, se_ostream& crmout)
{
    xptr block=snode->bblk;
    while (block!=XNULL)
    {
        CHECKP(block);
        node_blk_hdr* blk=GETBLOCKBYNODE(block);
        if (blk->pblk==NULL && block!=snode->bblk)
            error_msg("First block inconsistency",crmout);
        xptr tmp=blk->nblk;
        if (tmp!=XNULL)
        {
            CHECKP(tmp);
            if ((GETBLOCKBYNODE(tmp))->pblk!=block)
                error_msg("block sequence inconsistency",crmout);
        }
        block=tmp;
    }
    //recursive walkthrough
    sc_ref_item* sc= snode->children.first;
    while (sc!=NULL)
    {
        checkBlockSequenceConsistency(sc->object.snode, crmout);
        sc=sc->next;
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
    if (prev_x!=NULL)
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
void testSaDoc(const char* docname)
{
    schema_node_cptr rscm=find_document(docname);
    if (rscm.found())
    {
        checkBlockSequenceConsistency(rscm,crm_out);
        if (rscm->bblk!=XNULL)
        {
            CHECKP(rscm->bblk);
            xptr rnode=GETBLOCKFIRSTDESCRIPTORABSOLUTE(XADDR(rscm->bblk));
            checkTreeConsistency(rnode,crm_out);
        }
    }
}
