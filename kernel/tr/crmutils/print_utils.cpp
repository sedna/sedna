/*
 * File:  print_utils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/crmutils/crmutils.h"
#include "tr/structures/metadata.h"
#include "tr/structures/nodes.h"
#include "tr/structures/schema.h"
#include "tr/idx/index_data.h"
#include "tr/strings/strings.h"
#include "tr/mo/micro.h"
#include "tr/crmutils/node_utils.h"
#include "tr/vmm/vmm.h"
#include "tr/pstr/pstr.h"
#include "tr/pstr/pstr_long.h"
#include "tr/strings/e_string.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/PPBase.h"
#include "tr/idx/btree/btstruct.h"
#include "tr/idx/btree/btree.h"
using namespace tr_globals;
se_stdlib_ostream crm_out(std::cerr);
typedef std::pair<std::string,std::string> ns_pair;
typedef  std::map< ns_pair ,xml_ns*> nspt_map;
static  std::set<std::string> nspt_pref;
static nspt_map  xm_nsp;
static bool def_set=false;




/* prints information in  descriptor */
void print_descriptor(n_dsc* node,int shift, se_ostream& crmout)
{
	crmout << "\n====================";
	crmout << "\n Shift = " << shift;
	crmout << "\n Next descriptor = " << node->desc_next;
	crmout << "\n Previous descriptor = " << node->desc_prev;
	crmout << "\n Left sibling = " << XADDR(node->ldsc);
	crmout << "\n Right sibling = " << XADDR(node->rdsc);
	crmout << "\n Parent indirection = " << XADDR(node->pdsc);
	crmout << "\n Numbering Scheme = " ;
	xptr nodex=ADDR2XPTR(node);
	nid_print(nodex,crmout);
	CHECKP(nodex);
}

/* prints information in element descriptor */
void print_element(e_dsc* node,int shift, shft size, schema_node* scm, se_ostream& crmout)
{
	print_descriptor(node,shift,crmout);
	crmout << "\n Type = " << convertTypeToName(node->type);
	int cnt_ptrs=(size-sizeof(e_dsc))/sizeof(xptr);
	crmout <<"\nChilds=======";
	xptr* childx=(xptr*)((char*)node+sizeof(e_dsc));
	for (int i=0;i<cnt_ptrs;i++)
	{
		char* str=scm->get_name(i);
		if (str!=NULL)
			crmout <<"\n"<<	str << " = " << XADDR(*childx);
		else
			crmout <<"\nText node = " << XADDR(*childx);

	 childx+=1;
	}
	crmout <<"\n=============";
    
}

/* prints information in document descriptor */
void print_document(d_dsc* node,int shift, shft size, schema_node* scm, se_ostream& crmout)
{
	print_descriptor(node,shift,crmout);
	crmout << "\nName position = " << XADDR(node->data);
	crmout << "\nName size = " << node->size;
	xptr nodex=ADDR2XPTR(node);
	crmout <<"\n Name = ";
	print_text(nodex,  crmout,xml,document);
	CHECKP(nodex);
	int cnt_ptrs=(size-sizeof(d_dsc))/sizeof(xptr);
	crmout <<"\nChilds=======";
	xptr* childx=(xptr*)((char*)node+sizeof(d_dsc));
	for (int i=0;i<cnt_ptrs;i++)
	{
	 crmout <<"\n"<<	scm->get_name(i) << " = " << XADDR(*childx);
	 childx+=1;
	}
	crmout <<"\n=============";
    
}

/* prints information in text descriptor */
void print_text(t_dsc* node,int shift,  se_ostream& crmout,t_item xq_type)
{
	print_descriptor(node,shift,crmout);
	crmout << "\n Text position = " << XADDR(node->data);
	crmout << "\n Text size = " << XADDR(node->data);
	xptr nodex=ADDR2XPTR(node);
	crmout <<"Text = ";
	print_text(nodex,  crmout,xml,xq_type);
	CHECKP(nodex);
}

/* prints information in attribute descriptor */
void print_attribute(a_dsc* node,int shift,  se_ostream& crmout)
{
	print_descriptor(node,shift,crmout);
	crmout << "\n Type = " << convertTypeToName(node->type);
	crmout << "\n Text position = " << XADDR(node->data);
	crmout << "\n Text size = " << XADDR(node->data);
	xptr nodex=ADDR2XPTR(node);
	crmout <<"Text = ";
	print_text(nodex,  crmout,xml,attribute);
	CHECKP(nodex);
}

/* prints information in block header */
void print_desc_block_hdr(node_blk_hdr* block, se_ostream& crmout)
{
	crmout <<"\nBLOCK address = " << block;
	crmout <<"\nTotal descriptors = " << block->count;
	crmout <<"\nFirst descriptor = " << block->desc_first;
	crmout <<"\nLast descriptor = " << block->desc_last;
	crmout <<"\nDescriptor size = " << block->dsc_size;
	crmout <<"\nFirst free space = " << block->free_first;
	crmout <<"\nNext block address = " << XADDR(block->nblk);
	crmout <<"\nPrevious block address = " << XADDR(block->pblk);
}

/* prints information in block */
void print_desc_block(xptr block, se_ostream& crmout)
{
	CHECKP(block);
	node_blk_hdr* header= (node_blk_hdr*)XADDR(block);
	shft size=((shft)(PAGE_SIZE-sizeof(node_blk_hdr)))/header->dsc_size;
	t_item type=GETTYPE(header->snode);
	bool *mark=se_new bool[size];
    shft i = 0;
	for (i=0;i<size;i++) mark[i]=true;
	shft shift=header->free_first;
	int empcnt=0;
	while (shift!=0) 
	{
		mark[(shft)((shift-sizeof(node_blk_hdr))/header->dsc_size)]=false;
		shift=*((shft*) ( (char*)header+shift ));
		empcnt++;
	}
	crmout << "\nFree space count = " <<empcnt;
	crmout << "\n============================================================================";
	print_desc_block_hdr(header, crmout);
	int begfr=-1;
	int endfr=-1;
	for ( i=0;i<size;i++)
	{
	 if (mark[i]==false)
	 {
		if (begfr==-1) begfr=i;
		endfr=i;
	 }
	 else
	 {
		 if (begfr!=-1)
		 {
		  	crmout << "\n====================";
			crmout << "Empty Space start=" <<(sizeof(node_blk_hdr)+begfr*header->dsc_size);
			crmout << " end=" <<(sizeof(node_blk_hdr)+endfr*header->dsc_size);
		 }
		 switch(type)
		 { 
		 case element:
			 {
				 print_element(
					 (e_dsc*)(GETPOINTERTODESC(header,sizeof(node_blk_hdr)+i*header->dsc_size)),
					 sizeof(node_blk_hdr)+i*header->dsc_size,
					 header->dsc_size,
					 header->snode,
					 crmout);
				 break;
			 }
		 case document: case virtual_root:
			 {
				print_document(
					 (d_dsc*)(GETPOINTERTODESC(header,sizeof(node_blk_hdr)+i*header->dsc_size)),
					 sizeof(node_blk_hdr)+i*header->dsc_size,
					 header->dsc_size,
					 header->snode,
					 crmout);
				 break;
			 }
		 case text:
			 {
				 print_text(
					 (t_dsc*)(GETPOINTERTODESC(header,sizeof(node_blk_hdr)+i*header->dsc_size)),
					 sizeof(node_blk_hdr)+i*header->dsc_size,
					 crmout,text);
				 break;
			 }
		 case attribute:
			 {
				 print_attribute(
					 (a_dsc*)(GETPOINTERTODESC(header,sizeof(node_blk_hdr)+i*header->dsc_size)),
					 sizeof(node_blk_hdr)+i*header->dsc_size,
					 crmout);
				 break;
			 }
		 }
		 begfr=-1;
	 }
	}
	if (begfr!=-1)
		 {
		  	crmout << "\n====================";
			crmout << "Empty Space start=" <<(sizeof(node_blk_hdr)+begfr*header->dsc_size);
			crmout << " end=" <<(sizeof(node_blk_hdr)+endfr*header->dsc_size);
		 }
	crmout << "\n============================================================================";
	delete[] mark;
}
/* returns type of  node */
char* convert_type(t_item type)
{
	switch(type)
	{
	case element: return"element";
	case text: return"text";
	case attribute: return"attribute";
	case xml_namespace: return"namespace";
	case document: case virtual_root: return"document";
	}
	return "unknown";
}
/* prints information in  schema node */
void print_schema(schema_node* node, se_ostream& crmout)
{
	crmout << "\n============================================================================";
	crmout << "\n Schema node. Address= " << node;
    if (node->name!=NULL)
		crmout << "\nname=" <<node->name;
	crmout << "\ntype=" <<convert_type(node->type);
	crmout<< "\nChildren: ";
	sc_ref *sc= node->first_child;
	while (sc!=NULL)
	{
		crmout << "\n name= " <<sc->name <<" type=" << convert_type(sc->type)
			<< "address= "<<sc->snode;
		sc=sc->next;
	}
	crmout<< "\nData: ";
	xptr block=node->bblk;
	while (block!=XNULL)
	{
		CHECKP(block);
		print_desc_block( block,  crmout);
		block=(GETBLOCKBYNODE(block))->nblk;
	}
	crmout << "\n============================================================================";
	sc= node->first_child;
	while (sc!=NULL)
	{
		print_schema(sc->snode, crmout);
		sc=sc->next;
	}
}

void inline print_indent( se_ostream& crmout,int indent)
{
 for (int i=0;i<indent;i++) crmout << " ";
}
/* prints information in  schema node */
void print_descriptive(schema_node* node, se_ostream& crmout, int indent)
{
	crmout << "\n";
	print_indent(crmout,indent);
	crmout << "<NODE ";// << node;
    if (node->name!=NULL)
		crmout << "local_name=\"" << node->name <<"\"";
	if (node->xmlns!=NULL)
	{
		if (node->xmlns->prefix!=NULL)
			crmout << " prefix=\"" << node->xmlns->prefix <<"\"";
		else
			crmout << " prefix=\"\"";
		if (node->xmlns->uri!=NULL)
			crmout << " uri=\"" << node->xmlns->uri <<"\"";	 
		else
			crmout << " uri=\"http://www.w3.org/XML/1998/namespace\"";
	}
	crmout << " type=\"" <<convert_type(node->type)<<"\"";
	crmout << " nodes_count=\"" <<node->nodecnt<<"\"";
	crmout << " block_count=\"" <<node->blockcnt<<"\"";
	crmout << " ext_nid_size=\"" <<node->extnids<<"\"";
	if (node->first_child==NULL)
	{
		crmout << "/>";
		return;
	}
	else
		crmout << ">\n";
	sc_ref *sc= node->first_child;
	while (sc!=NULL)
	{
		print_descriptive(sc->snode, crmout, indent+1);
		sc=sc->next;
	}
	crmout << "</NODE>";
}
/* prints descriptive schema  of stand-alone document*/
void print_descriptive_schema(const char * docname, se_ostream& crmout)
{
	if (docname==NULL)
		throw USER_EXCEPTION(SE2006);
	crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
	crmout << "\n<TREE_DESCRIPTIVE_SCHEMA document=\"";
	crmout<<docname;
	crmout<<"\">";
	//metadata_sem_down();
	schema_node* sn=find_document(docname);
	//metadata_sem_up();
	if (sn!=NULL)print_descriptive(sn,crmout,0);
	crmout << "\n</TREE_DESCRIPTIVE_SCHEMA>";
 
}

void printNameSpace(xml_ns* nsd,se_ostream& crmout,t_print ptype)
{
	if (ptype==xml )
	{
		if (nsd->prefix==NULL)
			crmout <<" xmlns=\""<<nsd->uri<<"\"";
		else
			crmout <<" xmlns:"<< nsd->prefix << "=\""<<nsd->uri<<"\"";
	}
}
/* prints descriptive schema  of collection*/
void print_descriptive_schema_col(const char * colname, se_ostream& crmout)
{
	if (colname==NULL)
		throw USER_EXCEPTION(SE2003);
	crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
	crmout << "\n<XML_DESCRIPTIVE_SCHEMA collection=\"";
	crmout<<colname;
	crmout<<"\">";
	//metadata_sem_down();
	schema_node* sn=find_collection(colname);
	//metadata_sem_up();
	if (sn!=NULL)print_descriptive(sn,crmout,0);
	crmout << "\n</XML_DESCRIPTIVE_SCHEMA>";
}
inline const ns_pair pref_to_str(xml_ns* ns)
{
	return ns_pair((ns->prefix!=NULL)?ns->prefix:"",(ns->uri!=NULL)?ns->uri:"http://www.w3.org/XML/1998/namespace");
}
inline const std::string prefix_to_str(char* pref)
{
	return std::string((pref!=NULL)?pref:"");
}
void print_attribute_prefix(se_ostream& crmout,schema_node* scm, int indent)
{
 char* pref=NULL;
 if (scm->xmlns==NULL)
 {
	 pref=NULL;
 }
 else if  (!indent)
	 pref=scm->xmlns->prefix;
 else
	pref=xm_nsp[pref_to_str(scm->xmlns)]->prefix;
 if (pref!=NULL)
 {
	 crmout<<pref<<":";
 }

}
void print_node_with_indent(xptr node, se_ostream& crmout,bool wi, int indent,t_print ptype, dynamic_context *cxt)
{
	switch(GETTYPE(GETSCHEMENODEX(node)))
	{
	case document: case virtual_root:
		{
			if (IS_DATA_BLOCK(node))
			crmout <<((ptype==xml)? "<?xml version=\"1.0\" standalone=\"yes\"":"(*TOP*");
			xptr child=giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),sizeof(d_dsc)));
			if(child==XNULL)
			{
				if (IS_DATA_BLOCK(node)) crmout << ((ptype==xml)? "?>": ")");
				return;			
			}
			else CHECKP(child);			
			if (GETTYPE(GETSCHEMENODEX(child))==attribute)
			{
				if (ptype==sxml)  crmout << "(@";
				do
				{	
					
					print_node_with_indent(child,crmout,wi,0,ptype,cxt);
					child=((n_dsc*)XADDR(child))->rdsc;
					if (child==XNULL) break;
					CHECKP(child);
				}
				while (GETTYPE(GETSCHEMENODEX(child))==attribute);
				if (ptype==sxml)  crmout << ")";
			}

			if (IS_DATA_BLOCK(node)) crmout << ((ptype==xml )? "?>": "");
			while (child!=XNULL)
			{
				CHECKP(child);
				print_node_with_indent(child,crmout,wi,0,ptype,cxt);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;
			}
			if (ptype==sxml)  crmout << ")";
			break;
		}
	case element:
		{
			if(wi&&indent) 
			{
				crmout<< "\n";
				print_indent(crmout,indent) ;
			}
			bool curwi=wi;
			bool lit=false;			
			bool def_inset=false;			
			crmout <<((ptype==xml)? "<": "(");
			schema_node* scn=GETSCHEMENODEX(node);
			xptr first_ns=XNULL;
			std::vector<ns_pair> *att_ns=NULL;
			std::vector<std::string> *pref_ns=NULL;
			//bool outerns=false;
			char* name=GETNAME(scn);
			if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
				crmout<<scn->xmlns->prefix<<":";
			crmout <<name;
			xptr child=giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),sizeof(e_dsc)));
			if(child==XNULL)
			{
				
				if (scn->xmlns!=NULL && xm_nsp.find(pref_to_str(scn->xmlns))==xm_nsp.end()&&	my_strcmp(scn->xmlns->prefix,"xml"))
				printNameSpace(scn->xmlns,crmout,ptype);	
				crmout << ((ptype==xml)? "/>": ")");			
				return;			
			}
			else
				CHECKP(child);
			//namespaces print and add
			while (GETTYPE(GETSCHEMENODEX(child))==xml_namespace)
			{	
				if (first_ns==XNULL)
					first_ns=child;
				ns_pair str=pref_to_str(((ns_dsc*)XADDR(child))->ns);
				nspt_map::iterator it_map=xm_nsp.find(str);
				if (it_map==xm_nsp.end())
				{
					xml_ns* sns=((ns_dsc*)XADDR(child))->ns;
					xm_nsp[str]=sns;
					if (!att_ns) 
						att_ns= se_new std::vector<ns_pair> ;
					att_ns->push_back(str);
					
					if (nspt_pref.find(str.first)==
					nspt_pref.end())
					{
						if (!pref_ns) pref_ns= se_new std::vector<std::string> ;
						pref_ns->push_back(str.first);
						nspt_pref.insert(str.first);
					}
					if (sns->prefix==NULL)
					{
						if (!def_set)def_inset=false;
						def_set=true;

					}
				}
				print_node_with_indent(child,crmout,wi,0,ptype,cxt);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;
				if (child==XNULL)  break;
				CHECKP(child);
			}
			//self namespace
			if (scn->xmlns!=NULL && xm_nsp.find(pref_to_str(scn->xmlns))==xm_nsp.end()&&
				my_strcmp(scn->xmlns->prefix,"xml"))
			{
				ns_pair str=pref_to_str(scn->xmlns);
				xm_nsp[str]=scn->xmlns;
				if (!att_ns) 
						att_ns= se_new std::vector<ns_pair> ;
				att_ns->push_back(str);
				printNameSpace(scn->xmlns,crmout,ptype);
				std::string prf=prefix_to_str(scn->xmlns->prefix);
				if (nspt_pref.find(prf)==
					nspt_pref.end())
				{
					if (!pref_ns) pref_ns= se_new std::vector<std::string> ;
					pref_ns->push_back(prf);
					nspt_pref.insert(prf);

				}
				if (scn->xmlns->prefix==NULL)
					def_set=true;
			}
			else
			{
				if (def_set&&scn->xmlns==NULL)
				{
					
					def_set=false;
					crmout <<" xmlns=\"\"";
				}
			}
			//case of def_ns

			xptr* ptr=NULL;
			sc_ref* sch=NULL;
			int cnt=0;
			int ctr=0;
			if (child==XNULL)
			{
				crmout << ((ptype==xml )? "/>": "))");
				goto nsfree;
			}
			//namespaces for attributes
			CHECKP(node);
			ptr= (xptr*)((char*)XADDR(node)+sizeof(e_dsc));
			sch=scn->first_child;
			while (sch!=NULL)
			{
				if (sch->xmlns!=NULL && sch->type==attribute &&  xm_nsp.find(pref_to_str(sch->xmlns))== xm_nsp.end()  && *(ptr+cnt)!=XNULL )
				{
					if (!(/*sch->xmlns->uri==NULL && */my_strcmp(sch->xmlns->prefix,"xml")==0))
					{
						ns_pair str=pref_to_str(sch->xmlns);
						xml_ns* xmn=NULL;
						if (nspt_pref.find(str.first)==nspt_pref.end())
						{
							xmn=sch->xmlns;							
							if (!pref_ns)pref_ns= se_new std::vector<std::string>;
							pref_ns->push_back(str.first);
							nspt_pref.insert(str.first);
						}
						else
						{
							xmn=generate_pref(ctr++,sch->xmlns->uri,cxt);
						}						
						xm_nsp[str]=xmn;
						if (!att_ns) 
							att_ns= se_new std::vector<ns_pair> ;
						att_ns->push_back(str);
						printNameSpace(xmn,crmout,ptype);					
					}
					else
					{

						xml_ns* t=cxt->st_cxt->get_ns_pair("xml","");//xml_ns::init(NULL,"xml",false);
						ns_pair str=pref_to_str(t);
						xm_nsp[str]=t;
					}

				}				
				sch=sch->next;
				cnt++;
			}			
			//attributes			
			CHECKP(child);
			if (GETTYPE(GETSCHEMENODEX(child))==attribute)
			{
				if (ptype==sxml )  crmout << "(@";
				do
				{	
					print_node_with_indent(child,crmout,wi,indent+1,ptype,cxt);
					CHECKP(child);
					child=((n_dsc*)XADDR(child))->rdsc;
					if (child==XNULL)  break;
					CHECKP(child);
				} while (GETTYPE(GETSCHEMENODEX(child))==attribute);
				if (ptype==sxml )  crmout << ")";
			}
						
			if (child==XNULL)
			{
				crmout << ((ptype==xml )? "/>": ")");
				//return;
				goto nsfree;
			}
			else
			crmout<< ((ptype==xml )? ">": "");

			while (child!=XNULL)
			{
				CHECKP(child);
				bool cit=(GETSCHEMENODEX(child)->type==text);
                if (cit)
					curwi=false;
				else
				{
					if (!lit) curwi=wi;
				}
				print_node_with_indent(child,crmout,curwi,indent+1,ptype,cxt);

				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;
				lit=cit;
			}
			if(curwi||(wi && !lit)) 
			{
				crmout<< "\n";
				print_indent(crmout,indent) ;
			}
			if (ptype==xml )
			{
				crmout << "</";
				if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
					crmout<<scn->xmlns->prefix<<":";
				crmout<< name <<">";
			}
			else
				crmout <<")";
			//namespaces remove
nsfree:
		//	if (outerns) xm_nsp.erase(pref_to_str(scn->xmlns->prefix));
		/*	if (first_ns!=XNULL)
			{
				CHECKP(first_ns);
				ns_dsc* nsd=(ns_dsc*)XADDR(first_ns);
				while (nsd!=NULL)
				{
					std::string str=pref_to_str(nsd->ns->prefix);
					if(--xm_nsp[str]==0)
						xm_nsp.erase(str);
					nsd=(ns_dsc*)getNextSiblingOfSameSort(nsd);				 
				}
			}
			*/
			if (att_ns)
			{
				std::vector<ns_pair>::const_iterator it=att_ns->begin();
				while(it!=att_ns->end())
				{
					/*if(--xm_nsp[*it]==0)
						xm_nsp.erase(*it);
					it++;*/
					xm_nsp.erase(*it);
					it++;
				}	
				delete att_ns;
			}
			if (pref_ns)
			{
				std::vector<std::string>::const_iterator it=pref_ns->begin();
				while(it!=pref_ns->end())
				{
					/*if(--xm_nsp[*it]==0)
						xm_nsp.erase(*it);
					it++;*/
					nspt_pref.erase(*it);
					it++;
				}	
				delete pref_ns;
			}			
			if (def_inset)
				def_set=false;
			
			break;
		}
	case xml_namespace:
		{
			printNameSpace(((ns_dsc*)XADDR(node))->ns,crmout,ptype);
			break;
		}

	case attribute:
		{
			schema_node* scn=GETSCHEMENODEX(node);
			if (ptype==xml )
			{
				
				crmout <<" ";
				print_attribute_prefix(crmout,scn,indent);
				crmout<< scn->name << "=\"";
			}
			else
			{				
                crmout <<" (";
				print_attribute_prefix(crmout,scn,indent);
				crmout<< scn->name <<"  ";
			}
			print_text(node,crmout,ptype,attribute);
			crmout <<((ptype==xml )?"\"":")");
			return;
		}
	case text:
		{
			if(wi&&indent) 
			{
				crmout<< "\n";
				print_indent(crmout,indent) ;
			}
			print_text(node,crmout,ptype,text);
			break;
		}
	case comment:
		{
			if(wi&&indent) 
			{
				crmout<< "\n";
				print_indent(crmout,indent) ;
			}
			crmout<< "<!--";
			print_text(node,crmout,ptype,text);
			crmout<< "-->";
			break;
		}
	case cdata:
		{
			if(wi) 
			{
				crmout<< "\n";
				print_indent(crmout,indent) ;
			}
			crmout<< "<![CDATA[";
			print_text(node,crmout,ptype,cdata);
			crmout<< "]]>";
			break;
		}
	case pr_ins:
		{
			if(wi&&indent) 
			{
				crmout<< "\n";
				print_indent(crmout,indent) ;
			}
			crmout<< "<?";
			print_text(node,crmout,ptype,pr_ins);
			crmout<< "?>";
			break;
		}
	}
}
void print_node_with_prefixes(xptr node, se_ostream& crmout, int indent)
{
	switch(GETTYPE(GETSCHEMENODEX(node)))
	{
	case document: case virtual_root:
		{
			crmout << "DOCUMENT NODE PREFIX=" ;
			nid_print(node,crmout) ;
			crmout<< ">";
			CHECKP(node);
			xptr child=giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),sizeof(d_dsc)));
			while (child!=XNULL)
			{
				CHECKP(child);
				print_node_with_prefixes(child,crmout,1);
				child=((n_dsc*)XADDR(child))->rdsc;
			}
			break;
		}
	case element:
		{
			crmout<< "\n";
			print_indent(crmout,indent) ;
			char* name=GETNAME(GETSCHEMENODEX(node));
			crmout <<"<" <<name << " PREFIX=";
			nid_print(node,crmout);
			crmout<< ">";
			xptr child=giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),sizeof(e_dsc)));
			
			while (child!=XNULL)
			{
				CHECKP(child);
				print_node_with_prefixes(child,crmout,indent+1);
				child=((n_dsc*)XADDR(child))->rdsc;
			}
			crmout<< "\n";
			print_indent(crmout,indent) ;
			crmout <<"</" << name <<">";
			break;
		}
	case attribute:
		{
			crmout<< "\n";
			print_indent(crmout,indent) ;
			crmout <<"<ATTRIBUTE "<< GETNAME(GETSCHEMENODEX(node)) << " PREFIX=";
			nid_print(node,crmout);
			crmout << "/>";
			break;
		}
	case text:
		{
			crmout<< "\n";
			print_indent(crmout,indent) ;
			crmout <<"<TEXT NODE" << " PREFIX=";
			nid_print(node,crmout);
			crmout << "/>";
			break;
		}
	}
}
void print_node(xptr node, se_ostream& crmout, t_print ptype, dynamic_context *cxt)
{ 
	CHECKP(node);
	print_node_with_indent(node,crmout,false,0,ptype,cxt);
}

void print_node_indent(xptr node, se_ostream& crmout, t_print ptype, dynamic_context *cxt)
{ 
	CHECKP(node);
	print_node_with_indent(node,crmout,true,0,ptype,cxt);
	//print_node_with_prefixes(node, crmout, 0);
}


//TEMPORARY UNREALIZED!!!
void print_text(xptr txt, se_ostream& crmout,t_print ptype, t_item xq_type)
{
	int size =((t_dsc*)XADDR(txt))->size;
	if (size<=PSTRMAXSIZE)
	{
		xptr ind_ptr=((t_dsc*)XADDR(txt))->data;
		if (ind_ptr==XNULL)return;
		CHECKP(ind_ptr);
		shft shift= *((shft*)XADDR(ind_ptr));
		char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
		if (ptype==xml)
		{
			if (xq_type!=text && xq_type!=attribute)
				crmout.write(data,size);
			else if (xq_type == attribute)
				crmout.writeattribute(data, size);
			else
				crmout.writextext(data,size);
		}
		else
		{
			crmout<<"\"";
			if (xq_type!=text && xq_type!=attribute)
				crmout.write(data,size);
			else crmout.writextext(data,size);
			crmout<<"\"";
		}
	}
	else
	{
		if (ptype!=xml)
		 crmout<<"\"";
		pstr_long_writextext(txt,crmout);
		if (ptype!=xml)
		 crmout<<"\"";
		//crmout.writextext(data,size);
	}
	dynamic_context::stm.flush(write_func,&crmout);
		
}
void print_tuple(const tuple &tup, se_ostream& crmout,bool ind,t_print ptype,bool is_first,dynamic_context *cxt)
{
	if (tup.is_eos()) return;
	if (ind && !is_first) crmout<<"\n";
	for (int i=0;i<tup.cells_number;i++)
	{
		if (tup.cells[i].is_node())	
			(ind)? print_node_indent(tup.cells[i].get_node(),crmout,ptype,cxt):print_node(tup.cells[i].get_node(),crmout,ptype,cxt);
		else
		{
			if (tup.cells[i].is_light_atomic())
			{
				if (is_fixed_size_type(tup.cells[i].get_atomic_type()))
				{
                    get_lexical_representation_for_fixed_size_atomic(tr_globals::mem_str_buf2, tup.cells[i], ptype);
					crmout.writextext(tr_globals::mem_str_buf2,strlen(tr_globals::mem_str_buf2));
				}
                else
				{
                    if (tup.cells[i].get_atomic_type() == xs_QName)
                    {
                        const char *prefix = xs_QName_get_prefix(tup.cells[i].get_str_mem());
                        if (prefix && strlen(prefix) != 0)
                        {
                            crmout.writextext((char*)prefix, strlen(prefix));
                            crmout.writextext(":", 1);
                        }
                        crmout.writextext((char*)xs_QName_get_local_name(tup.cells[i].get_str_mem()), 
                                          strlen(xs_QName_get_local_name(tup.cells[i].get_str_mem())));
                    }
					//crmout<<tup.cells[i].get_str_mem();
					else crmout.writextext(tup.cells[i].get_str_mem(), tup.cells[i].get_strlen_mem());					
				}
			}
			else
			{
				print_tuple_cell(crmout,tup.cells[i]);				
			}
			dynamic_context::stm.flush(write_func,&crmout);
		}
		if (ind && i<(tup.cells_number-1)) crmout<<" ,";
	}
}
void print_tuple(const tuple &tup, se_ostream& crmout,t_print ptype,dynamic_context *cxt)
{print_tuple(tup,crmout,false,ptype,false,cxt);}

void print_tuple_indent(const tuple &tup, se_ostream& crmout,t_print ptype,bool is_first,dynamic_context *cxt)
{print_tuple(tup,crmout,true,ptype,is_first,cxt);}
/* prints the list of metadata features*/
void print_metadata(se_ostream& crmout)
{
	crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
	crmout << "\n<METADATA>";
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (mdc->obj->document_name==NULL)
		{
			crmout<<"\n <Collection name=\""<<mdc->obj->collection_name<<"\">";
			printSimpleDebugInfo(mdc->obj->snode,crmout);
			crmout<<"\n </Collection>";
		}
		else
		{
			crmout<<"\n<Document name=\""<<mdc->obj->document_name<<"\"";
			crmout<<">";
			printSimpleDebugInfo(((sn_metadata_cell*)mdc->obj)->snode,crmout);
			crmout<<"\n </Document>";
		}
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();
	crmout << "\n</METADATA>";
}

/* prints the list of documents*/
void print_documents(se_ostream& crmout, bool ps)
{
	crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
	crmout << "\n<XML_DOCUMENTS>";
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (mdc->obj->document_name!=NULL)
		{
			crmout<<"\n<Document name=\""<<mdc->obj->document_name<<"\"";
			crmout<<">";
			if (ps) printSimpleDebugInfo(((sn_metadata_cell*)mdc->obj)->snode,crmout);
			crmout<<"\n </Document>";
		}
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();
	crmout << "\n</XML_DOCUMENTS>";
}

/* prints the list of documents in the selected collection*/
void print_documents_in_collection(se_ostream& crmout,const char* collection)
{
	crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
	crmout << "\n<XML_DOCUMENTS_Collection=\"";
	crmout<<collection;
	crmout<<"\">";
	metadata_sem_down();
	sn_metadata_cell* mdc=metadata->find(collection,NULL);
	if (mdc!=NULL)
	{
		col_schema_node* coll=(col_schema_node*)mdc->snode;
		bt_key key;
		key.setnew("");
		bt_cursor cursor=bt_find_gt((coll->metadata)->btree_root, key);
		while(cursor.bt_next_key())
		{
			crmout<<"\n<Document name=\""<<(char*)cursor.get_key().data()<<"\"";
			crmout<<"/>";		
		}
		/*pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* dc=coll->metadata->rb_minimum(coll->metadata->root);
		while (dc!=NULL)
		{
			crmout<<"\n<Document name=\""<<dc->obj->document_name<<"\"";
			crmout<<"/>";		
			dc=coll->metadata->rb_successor(dc); 
		}*/
		
	}	
	metadata_sem_up();
	crmout << "\n</XML_DOCUMENTS_Collection>";
}

/* prints the list of collections*/
void print_collections(se_ostream& crmout, bool ps)
{
	crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
	crmout << "\n<COLLECTIONS>";
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (mdc->obj->document_name==NULL)
		{
			crmout<<"\n <Collection name=\""<<mdc->obj->collection_name<<"\">";
			if (ps) printSimpleDebugInfo(((sn_metadata_cell*)mdc->obj)->snode,crmout);
			crmout<<"\n </Collection>";
		}
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();
	crmout << "\n</COLLECTIONS>";
}


//=================================================================
// Print descriptive schema in SXML

/* prints information in  schema node */
void sxml_print_descriptive(schema_node* node, se_ostream& crmout, int indent)
{
 	crmout << " (NODE (@";
        if (node->name!=NULL)
		crmout << " (local_name \"" << node->name <<"\")";
	if (node->xmlns!=NULL)
	{
		if (node->xmlns->prefix!=NULL)
			crmout << " (prefix \"" << node->xmlns->prefix <<"\")";
		else
			crmout << " (prefix \"\")";
		if (node->xmlns->uri!=NULL)
			crmout << " (uri \"" << node->xmlns->uri <<"\")";	 
		else
			crmout << " (uri \"http://www.w3.org/XML/1998/namespace\")";
	}
	crmout << " (type \"" <<convert_type(node->type)<<"\")";
	if (node->first_child==NULL)
	{
		crmout << "))";    // closing the attr-list and element
		return;
	}
	else
		crmout << ")";   // closing just the attr-list
	sc_ref *sc= node->first_child;
	while (sc!=NULL)
	{
		sxml_print_descriptive(sc->snode, crmout, indent+1);
		sc=sc->next;
	}
	crmout << ")";
}
/* prints descriptive schema  of stand-alone document*/
void sxml_print_descriptive_schema(const char * docname, se_ostream& crmout)
{
	if (docname==NULL)
		throw USER_EXCEPTION(SE2006);
	crmout << "(*TOP* (*PI* xml \"version=\\\"1.0\\\" standalone=\\\"yes\\\"\")";
	crmout << " (TREE_DESCRIPTIVE_SCHEMA (@ (document \"";
	crmout<<docname;
	crmout<<"\"))";
	//metadata_sem_down();
	schema_node* sn=find_document(docname);
	//metadata_sem_up();
	if (sn!=NULL) sxml_print_descriptive(sn,crmout,0);
	crmout << "))";  // end tag for </TREE_DESCRIPTIVE_SCHEMA> and *TOP*
 
}
/* prints descriptive schema  of collection*/
void sxml_print_descriptive_schema_col(const char * colname, se_ostream& crmout)
{
	if (colname==NULL)
		throw USER_EXCEPTION(SE2003);
	crmout << "(*TOP* (*PI* xml \"version=\\\"1.0\\\" standalone=\\\"yes\\\"\")";
	crmout << " (XML_DESCRIPTIVE_SCHEMA (@ (collection \"";
	crmout<<colname;
	crmout<<"\"))";   // end attr-list
	//metadata_sem_down();
	schema_node* sn=find_collection(colname);
	//metadata_sem_up();
	if (sn!=NULL) sxml_print_descriptive(sn,crmout,0);
	crmout << "))";    // end-tag for </XML_DESCRIPTIVE_SCHEMA> and *TOP*
}

//printings to buffer
#ifdef SE_ENABLE_FTSEARCH
void print_name_space(xml_ns* nsd,op_str_buf& tbuf,ft_index_type type)
{
	switch (type)
			{
			case ft_xml: 
			case ft_xml_ne: 
			case ft_xml_hl: 
				{
			if (nsd->prefix==NULL)
				tbuf<<" xmlns=\""<<	nsd->uri<<"\"";
			else
				tbuf<<" xmlns:"<<nsd->prefix<<"=\""<<nsd->uri<<"\"";
				break;
				}
			case ft_string_value:
				{
					tbuf<<	nsd->uri;
					break;
				}
			case ft_delimited_value:
				{
					tbuf<<" "<<	nsd->uri;
					break;
				}			
			}	
	
			
}
static StrMatcher *escape_sm = NULL;
static void make_escape_sm()
{
	//TODO: assert escape_sm == NULL
	escape_sm = se_new StrMatcher();
	escape_sm->add_str("&", "&amp;", ~pat_attribute);
	escape_sm->add_str("<", "&lt;", ~pat_attribute);
	escape_sm->add_str(">", "&gt;", ~pat_attribute);
	escape_sm->add_str("\"", "&quot;", pat_attribute);
	escape_sm->add_str("\"", "\xEE\xA0\x83", pat_custom1);
}
static void tbuf_write_cb(void *param, const char *str, int len)
{
	op_str_buf* tbuf = (op_str_buf*)param;
	tbuf->append(str, len);
}
static void print_text(xptr txt, op_str_buf& tbuf, t_item xq_type, bool escapes = true)
{
	int size =((t_dsc*)XADDR(txt))->size;
	xptr ind_ptr=((t_dsc*)XADDR(txt))->data;
	if (size == 0)
		return;
	if (size<=PSTRMAXSIZE)
	{
		CHECKP(ind_ptr);
		ind_ptr=ADDR2XPTR((char*)XADDR(BLOCKXPTR(ind_ptr))+*((shft*)XADDR(ind_ptr)));
	}
	tuple_cell tc=tuple_cell::atomic_pstr(xs_string,size,ind_ptr);
	if (!escapes)
	{
		if (xq_type == attribute)
		{
			if (escape_sm == NULL)
				make_escape_sm();
			escape_sm->parse_tc(&tc, tbuf_write_cb, &tbuf, pat_custom1);
			escape_sm->flush(tbuf_write_cb, &tbuf);
		}
		else
			tbuf.append(tc);
	}
	else
	{
		if (escape_sm == NULL)
			make_escape_sm();
		if (xq_type!=text && xq_type!=attribute)
			tbuf.append(tc);
		else if (xq_type == attribute)
		{
			escape_sm->parse_tc(&tc, tbuf_write_cb, &tbuf, pat_attribute);
			escape_sm->flush(tbuf_write_cb, &tbuf);
		}
		else
		{
			escape_sm->parse_tc(&tc, tbuf_write_cb, &tbuf, pat_element);
			escape_sm->flush(tbuf_write_cb, &tbuf);
		}
	}
}


void print_node_to_buffer(xptr node,op_str_buf& tbuf,ft_index_type type,pers_sset<ft_custom_cell,unsigned short> * custom_tree, const char *opentag, const char *closetag)
{
	switch(GETTYPE(GETSCHEMENODEX(node)))
	{
	case document: case virtual_root:
		{
			switch (type)
			{
			case ft_xml:case ft_xml_ne:case ft_xml_hl: tbuf<<opentag<<"?xml version=\"1.0\" standalone=\"yes\""; break;
			case ft_string_value:break;
			case ft_customized_value: ft_delimited_value:tbuf<<" ";break;
			}	
			CHECKP(node);
			xptr child=giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),sizeof(d_dsc)));
			if(child==XNULL)
			{
				if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<<"?"<<closetag;
				return;			
			}
			else CHECKP(child);
			while (GETTYPE(GETSCHEMENODEX(child))==attribute)
			{	
				
				if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) print_node_to_buffer(child,tbuf,type,custom_tree,opentag,closetag);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;
				if (child==XNULL) break;
				CHECKP(child);
			}
			if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<<"?"<<closetag;
			while (child!=XNULL)
			{
				CHECKP(child);
				print_node_to_buffer(child,tbuf,type,custom_tree,opentag,closetag);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;
			}			
			break;
		}
	case element:
		{
			schema_node* scn=GETSCHEMENODEX(node);
			if (custom_tree!=NULL)
			{
				pers_sset<ft_custom_cell,unsigned short>::pers_sset_entry* scget= custom_tree->get(scn->name,scn->xmlns);
				if (scget!=NULL) type=scget->obj->cm;
				else
					if (type==ft_customized_value) type=ft_xml;
			}
			switch (type)
			{
			case ft_xml:case ft_xml_ne:tbuf<<opentag; break;
			case ft_xml_hl: tbuf<<opentag<<"_"; break;
			case ft_string_value:break;
			case ft_delimited_value:tbuf<<" ";break;			
			}
			//std::vector<std::string> *att_ns=NULL;
			char* name=GETNAME(scn);
			if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
				if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<<scn->xmlns->prefix<<":";
			if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<<name;
			CHECKP(node);
			xptr child=giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),sizeof(e_dsc)));
			if(child==XNULL)
			{
				if (type==ft_xml || type==ft_xml_ne) tbuf<<"/"<<closetag;
				if (type==ft_xml_hl) tbuf<<"/"<<closetag<<" ";
				return;			
			}
			else
				CHECKP(child);			
			while (GETTYPE(GETSCHEMENODEX(child))==attribute)
			{	
				if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) print_node_to_buffer(child,tbuf,type,custom_tree,opentag,closetag);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;
				if (child==XNULL)  break;
				CHECKP(child);
			}
			if (child==XNULL)
			{
				if (type==ft_xml || type==ft_xml_ne) tbuf<<"/"<<closetag;
				if (type==ft_xml_hl) tbuf<<"/"<<closetag<<"  ";
				return;
				
			}
			else
			{
				if (type==ft_xml || type==ft_xml_ne) tbuf<<closetag;
				if (type==ft_xml_hl) tbuf<<closetag<<" ";
			}
			bool cit=false;
			while (child!=XNULL)
			{
				CHECKP(child);
				//CHECKP(child);
				cit=(GETSCHEMENODEX(child)->type==element);
				print_node_to_buffer(child,tbuf,type,custom_tree,opentag,closetag);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;				
			}
			if (type==ft_xml || type==ft_xml_ne) tbuf<<opentag<<"/";
			else
			if (type==ft_xml_hl) tbuf<<opentag<<"/_";
			else
			if (type==ft_delimited_value && !cit) tbuf<<" ";
			if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
				if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<<scn->xmlns->prefix<<":";
			if (type==ft_xml || type==ft_xml_ne) tbuf<<name<<closetag;			
			if (type==ft_xml_hl) tbuf<<name<<closetag<<" ";
			break;
		}
	case xml_namespace:
		{
			print_name_space(((ns_dsc*)XADDR(node))->ns,tbuf,type);
			break;
		}
	case attribute:
		{
			schema_node* scn=GETSCHEMENODEX(node);
			switch (type)
			{
			case ft_xml:
			case ft_xml_ne:
			case ft_xml_hl:
				{
					if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
						tbuf <<" "<<scn->xmlns->prefix<<":"<< scn->name << "=\"";
					else
						tbuf <<" "<< scn->name << "=\"";
					CHECKP(node);
					print_text(node,tbuf,attribute,type!=ft_xml_ne);
					tbuf <<"\"";
					break;
				}
			case ft_string_value:print_text(node,tbuf,attribute);break;
			case ft_delimited_value:tbuf<<" ";break;
			}				
			return;
		}
	case text:
		{
			print_text(node,tbuf,text,type!=ft_xml_ne);
			break;
		}
	case comment:
		{
			
	switch (type)
			{
			case ft_xml:case ft_xml_ne:case ft_xml_hl: tbuf<< opentag<<"!--"; break;
			case ft_string_value:break;
			case ft_delimited_value:tbuf<<" ";break;
			}
			CHECKP(node);
			print_text(node,tbuf,text,type!=ft_xml_ne);
			if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<< "--" << closetag;
			break;
		}
	case cdata:
		{
			switch (type)
			{
			case ft_xml:case ft_xml_ne:case ft_xml_hl: tbuf<< opentag<<"![CDATA["; break;
			case ft_string_value:break;
			case ft_delimited_value:tbuf<<" ";break;
			}
			CHECKP(node);
			print_text(node,tbuf,cdata,type!=ft_xml_ne);
			if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<< "]]"<<closetag;
			break;
		}
	case pr_ins:
		{
			switch (type)
			{
			case ft_xml:case ft_xml_ne:case ft_xml_hl: tbuf<< opentag<<"?"; break;
			case ft_string_value:break;
			case ft_delimited_value:tbuf<<" ";break;
			}
			CHECKP(node);
			print_text(node,tbuf,pr_ins);
			if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<< "?"<<closetag;
			break;
		}		
	}
}
#endif

