/*
 * File:  print_utils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "crmutils.h"
#include "metadata.h"
#include "nodes.h"
#include "schema.h"
#include "strings.h"
#include "micro.h"
#include "node_utils.h"
#include "vmm.h"
#include "pstr.h"
#include "pstr_long.h"
#include "e_string.h"
#include "casting_operations.h"

se_stdlib_ostream crm_out(std::cerr);
typedef  std::map<  std::string,int> nspt_map;
static nspt_map  xm_nsp;

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
	bool *mark=new bool[size];
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
inline const std::string pref_to_str(const char* pref)
{
	return std::string((pref!=NULL)?pref:"");
}
void print_node_with_indent(xptr node, se_ostream& crmout,bool wi, int indent,t_print ptype)
{
	switch(GETTYPE(GETSCHEMENODEX(node)))
	{
	case document: case virtual_root:
		{
			crmout <<((ptype==xml)? "<?xml version=\"1.0\" standalone=\"yes\"":"(*TOP*");
			xptr child=giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),sizeof(d_dsc)));
			if(child==XNULL)
			{
				crmout << ((ptype==xml)? "?>": ")");
				return;			
			}
			else CHECKP(child);
			if (ptype==sxml)  crmout << "(@";
			while (GETTYPE(GETSCHEMENODEX(child))==attribute)
			{	
				
				print_node_with_indent(child,crmout,wi,0,ptype);
				child=((n_dsc*)XADDR(child))->rdsc;
				if (child==XNULL) break;
				CHECKP(child);
			}
			crmout << ((ptype==xml )? "?>": ")");
			while (child!=XNULL)
			{
				CHECKP(child);
				print_node_with_indent(child,crmout,wi,0,ptype);
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
			crmout <<((ptype==xml)? "<": "(");
			schema_node* scn=GETSCHEMENODEX(node);
			xptr first_ns=XNULL;
			std::vector<std::string> *att_ns=NULL;
			bool outerns=false;
			char* name=GETNAME(scn);
			if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
				crmout<<scn->xmlns->prefix<<":";
			crmout <<name;
			xptr child=giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),sizeof(e_dsc)));
			if(child==XNULL)
			{
				
				if (scn->xmlns!=NULL && xm_nsp.find(pref_to_str(scn->xmlns->prefix))==xm_nsp.end()&&
				my_strcmp(scn->xmlns->prefix,"xml"))
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
				std::string str=pref_to_str(((ns_dsc*)XADDR(child))->ns->prefix);
				nspt_map::iterator it_map=xm_nsp.find(str);
				if (it_map!=xm_nsp.end())
					it_map->second++;
				else
					xm_nsp[str]=1;
				print_node_with_indent(child,crmout,wi,0,ptype);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;
				if (child==XNULL)  break;
				CHECKP(child);
			}
			//self namespace
			if (scn->xmlns!=NULL && xm_nsp.find(pref_to_str(scn->xmlns->prefix))==xm_nsp.end()&&
				my_strcmp(scn->xmlns->prefix,"xml"))
			{
				xm_nsp[pref_to_str(scn->xmlns->prefix)]=1;
				printNameSpace(scn->xmlns,crmout,ptype);
				outerns=true;
			}
			if (child==XNULL)
			{
				crmout << ((ptype==xml )? "/>": "))");
				return;
			}
			//namespaces for attributes
			CHECKP(node);
			xptr* ptr= (xptr*)((char*)XADDR(node)+sizeof(e_dsc));
			sc_ref* sch=scn->first_child;
			int cnt=0;
			while (sch!=NULL)
			{
				if (sch->xmlns!=NULL && sch->type==attribute &&  xm_nsp.find(pref_to_str(sch->xmlns->prefix))== xm_nsp.end()  && *(ptr+cnt)!=XNULL &&!(/*sch->xmlns->uri==NULL && */ my_strcmp(sch->xmlns->prefix,"xml")==0))
				{
					std::string str=pref_to_str(sch->xmlns->prefix);
					xm_nsp[str]=1;
					if (!att_ns) 
						att_ns= new std::vector<std::string> ;
					att_ns->push_back(str);
					printNameSpace(sch->xmlns,crmout,ptype);
				}
				sch=sch->next;
				cnt++;
			}
			//attributes
			if (ptype==sxml )  crmout << "(@";
			CHECKP(child);
			while (GETTYPE(GETSCHEMENODEX(child))==attribute)
			{	
				print_node_with_indent(child,crmout,wi,0,ptype);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;
				if (child==XNULL)  break;
				CHECKP(child);
			}
			if (child==XNULL)
			{
				crmout << ((ptype==xml )? "/>": "))");
				//return;
				goto nsfree;
			}
			else
			crmout<< ((ptype==xml )? ">": ")");
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
				print_node_with_indent(child,crmout,curwi,indent+1,ptype);

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
			if (outerns) xm_nsp.erase(pref_to_str(scn->xmlns->prefix));
			if (first_ns!=XNULL)
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
			if (att_ns)
			{
				std::vector<std::string>::const_iterator it=att_ns->begin();
				while(it!=att_ns->end())
				{
					if(--xm_nsp[*it]==0)
						xm_nsp.erase(*it);
					it++;
				}	
				delete att_ns;
			}
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
				if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
					crmout <<" "<<scn->xmlns->prefix<<":"<< scn->name << "=\"";
				else
					crmout <<" "<< scn->name << "=\"";
			}
			else
			{
				if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
					crmout <<" ("<<scn->xmlns->prefix<<":"<< scn->name << "  ";
				else
					crmout <<" ("<< scn->name <<"  ";
			}
			print_text(node,crmout,ptype,attribute);
			crmout <<((ptype==xml )?"\"":")");
			return;
		}
	case text:
		{
			if(wi) 
			{
				crmout<< "\n";
				print_indent(crmout,indent) ;
			}
			print_text(node,crmout,ptype,text);
			break;
		}
	case comment:
		{
			if(wi) 
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
			if(wi) 
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
void print_node(xptr node, se_ostream& crmout,t_print ptype)
{ 
	CHECKP(node);
	print_node_with_indent(node,crmout,false,0,ptype);
}

void print_node_indent(xptr node, se_ostream& crmout,t_print ptype)
{ 
	CHECKP(node);
	print_node_with_indent(node,crmout,true,0,ptype);
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
	
		
}
void print_tuple(const tuple &tup, se_ostream& crmout,bool ind,t_print ptype,bool is_first)
{
	if (tup.is_eos()) return;
	if (ind && !is_first) crmout<<"\n";
	for (int i=0;i<tup.cells_number;i++)
	{
		if (tup.cells[i].is_node())	
			(ind)? print_node_indent(tup.cells[i].get_node(),crmout,ptype):print_node(tup.cells[i].get_node(),crmout,ptype);
		else
		{
			if (tup.cells[i].is_light_atomic())
			{
				if (tup.cells[i].is_string_type())
				{
					crmout<<tup.cells[i].get_str_mem();
				}
				else
				{
					xmlscm_type typ=tup.cells[i].get_atomic_type();
					char*z=new char[50];
					switch (typ)
					{
					case xs_integer:
						{
							crmout<<tup.cells[i].get_xs_integer();
							break;
						}
					case xs_decimal:
						{
							tup.cells[i].get_xs_decimal().get_string_value(z);
							crmout<<z;
							break;
						}
					case xs_float:
						{
							crmout<<tup.cells[i].get_xs_float();
							break;
						}
					case xs_double:
						{
							crmout<<(cast_to_xs_string(tup.cells[i])).get_str_mem();
							break;
						}
					case xs_boolean:
						{
							if (ptype==xml)
								crmout<<((tup.cells[i].get_xs_boolean())? "true":"false");
							else
								crmout<<((tup.cells[i].get_xs_boolean())? "#t":"#f");
							break;
						}
					case xs_date:
						{
							tup.cells[i].get_xs_date().get_string_value(z);
							if (ptype==xml)
								crmout<<z;
							else
								crmout<<"\""<<z<<"\"";
							break;
						}
					}
					delete[]z;
				}
			}
			else
			{
				print_tuple_cell(crmout,tup.cells[i]);				
			}
		}
		if (ind && i<(tup.cells_number-1)) crmout<<" ,";
	}
}
void print_tuple(const tuple &tup, se_ostream& crmout,t_print ptype)
{print_tuple(tup,crmout,false,ptype,false);}

void print_tuple_indent(const tuple &tup, se_ostream& crmout,t_print ptype,bool is_first)
{print_tuple(tup,crmout,true,ptype,is_first);}
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
		pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* dc=coll->metadata->rb_minimum(coll->metadata->root);
		while (dc!=NULL)
		{
			crmout<<"\n<Document name=\""<<dc->obj->document_name<<"\"";
			crmout<<"/>";		
			dc=coll->metadata->rb_successor(dc); 
		}
		
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
void print_name_space(xml_ns* nsd,t_str_buf& tbuf,ft_index_type type)
{
	switch (type)
			{
			case ft_xml: 
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
void print_text(xptr txt, t_str_buf& tbuf, t_item xq_type)
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
	if (xq_type!=text && xq_type!=attribute)
		tbuf.append(tc);
	else 
		tbuf.append(tc);
	
	
		
}


void print_node_to_buffer(xptr node,t_str_buf& tbuf,ft_index_type type,pers_sset<ft_custom_cell,unsigned short> * custom_tree)
{
	switch(GETTYPE(GETSCHEMENODEX(node)))
	{
	case document: case virtual_root:
		{
			switch (type)
			{
			case ft_xml:case ft_xml_hl: tbuf<<"<?xml version=\"1.0\" standalone=\"yes\""; break;
			case ft_string_value:break;
			case ft_customized_value: ft_delimited_value:tbuf<<" ";break;			
			}	
			CHECKP(node);
			xptr child=giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),sizeof(d_dsc)));
			if(child==XNULL)
			{
				if (type==ft_xml || type==ft_xml_hl) tbuf<<"?>";
				return;			
			}
			else CHECKP(child);
			while (GETTYPE(GETSCHEMENODEX(child))==attribute)
			{	
				
				if (type==ft_xml || type==ft_xml_hl) print_node_to_buffer(child,tbuf,type,custom_tree);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;
				if (child==XNULL) break;
				CHECKP(child);
			}
			if (type==ft_xml || type==ft_xml_hl) tbuf<<"?>";
			while (child!=XNULL)
			{
				CHECKP(child);
				print_node_to_buffer(child,tbuf,type,custom_tree);
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
			case ft_xml:tbuf<<"<"; break;
			case ft_xml_hl: tbuf<<"<_"; break;
			case ft_string_value:break;
			case ft_delimited_value:tbuf<<" ";break;			
			}	
			//std::vector<std::string> *att_ns=NULL;
			char* name=GETNAME(scn);
			if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
				if (type==ft_xml || type==ft_xml_hl) tbuf<<scn->xmlns->prefix<<":";
			if (type==ft_xml || type==ft_xml_hl) tbuf<<name;
			CHECKP(node);
			xptr child=giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),sizeof(e_dsc)));
			if(child==XNULL)
			{
				if (type==ft_xml) tbuf<<"/>";			
				if (type==ft_xml_hl) tbuf<<"/> ";			
				return;			
			}
			else
				CHECKP(child);			
			while (GETTYPE(GETSCHEMENODEX(child))==attribute)
			{	
				if (type==ft_xml || type==ft_xml_hl) print_node_to_buffer(child,tbuf,type,custom_tree);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;
				if (child==XNULL)  break;
				CHECKP(child);
			}
			if (child==XNULL)
			{
				if (type==ft_xml) tbuf<<"/>";
				if (type==ft_xml_hl) tbuf<<"/> ";
				return;
				
			}
			else
			{
				if (type==ft_xml) tbuf<<">";
				if (type==ft_xml_hl) tbuf<<"> ";
			}
			bool cit=false;
			while (child!=XNULL)
			{
				CHECKP(child);
				//CHECKP(child);
				cit=(GETSCHEMENODEX(child)->type==element);
				print_node_to_buffer(child,tbuf,type,custom_tree);
				CHECKP(child);
				child=((n_dsc*)XADDR(child))->rdsc;				
			}
			if (type==ft_xml) tbuf<<"</";
			else
			if (type==ft_xml_hl) tbuf<<"</_";
			else
			if (type==ft_delimited_value && !cit) tbuf<<" ";
			if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
				if (type==ft_xml || type==ft_xml_hl) tbuf<<scn->xmlns->prefix<<":";
			if (type==ft_xml) tbuf<<name<<">";			
			if (type==ft_xml_hl) tbuf<<name<<"> ";			
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
			case ft_xml_hl: 
				{
					if (scn->xmlns!=NULL && scn->xmlns->prefix!=NULL)
						tbuf <<" "<<scn->xmlns->prefix<<":"<< scn->name << "=\"";
					else
						tbuf <<" "<< scn->name << "=\"";
					CHECKP(node);
					print_text(node,tbuf,attribute);
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
			print_text(node,tbuf,text);
			break;
		}
	case comment:
		{
			
	switch (type)
			{
			case ft_xml:case ft_xml_hl: tbuf<< "<!--"; break;
			case ft_string_value:break;
			case ft_delimited_value:tbuf<<" ";break;
			}
			CHECKP(node);
			print_text(node,tbuf,text);
			if (type==ft_xml || type==ft_xml_hl) tbuf<< "-->";
			break;
		}
	case cdata:
		{
			switch (type)
			{
			case ft_xml:case ft_xml_hl: tbuf<< "<![CDATA["; break;
			case ft_string_value:break;
			case ft_delimited_value:tbuf<<" ";break;
			}
			CHECKP(node);
			print_text(node,tbuf,cdata);
			if (type==ft_xml || type==ft_xml_hl) tbuf<< "]]>";
			break;
		}
	case pr_ins:
		{			
			switch (type)
			{
			case ft_xml:case ft_xml_hl: tbuf<< "<?"; break;
			case ft_string_value:break;
			case ft_delimited_value:tbuf<<" ";break;
			}
			CHECKP(node);
			print_text(node,tbuf,pr_ins);
			if (type==ft_xml || type==ft_xml_hl) tbuf<< "?>";
			break;
		}		
	}
}
#endif