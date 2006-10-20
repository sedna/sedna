/*
 * File:  PPConstructors.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <vector>

#include "sedna.h"

#include "PPConstructors.h"
#include "PPUtils.h"
#include "casting_operations.h"
#include "updates.h"
#include "crmutils.h"
#include "metadata.h"
#include "e_string.h"

using namespace std;
using namespace tr_globals;
bool PPConstructor::firstCons = true;
schema_node* PPConstructor::root_schema=NULL;
xptr PPConstructor::virt_root=XNULL;
xptr PPConstructor::last_elem=XNULL;
xptr PPConstructor::cont_parind=XNULL;
xptr PPConstructor::cont_leftind=XNULL;
int PPConstructor::conscnt=0;
t_str_buf str_val;
//UTILS
void separateLocalAndPrefix(char*& prefix,const char*& qname)
{
	for (int i=0; i<strlen(qname);i++)
		if (qname[i]==':')
		{
			//prefix=new NCName(qname, i);//string(qname,i);
            prefix = new char[i + 1];
            memcpy(prefix, qname, i);
            prefix[i] = '\0';
			qname=qname+i+1;
			return;
		}
}
tuple_cell getQnameParameter(PPOpIn qname)
{
	tuple name(qname.ts);
	qname.op->next(name);
	if (name.is_eos()) throw USER_EXCEPTION(XPTY0004);
	if (!(name.cells_number==1 )) throw USER_EXCEPTION(XPTY0004);
	tuple_cell res=atomize(name.cells[0]);
	xmlscm_type xtype=res.get_atomic_type();
	if (xtype==xs_untypedAtomic)
	{
		res=cast(res, xs_string);
		//res=cast(res, xs_QName);
	}
	else
		if	(is_derived_from_xs_string(xtype)||xtype==xs_string);
	else if(xtype!=xs_QName)
		throw USER_EXCEPTION(XPTY0004);
	res=tuple_cell::make_sure_light_atomic(res);
	qname.op->next(name);
	if (!(name.is_eos())) throw USER_EXCEPTION(XPTY0004);
	return res;
}
/*tuple_cell getStringParameter(PPOpIn content)
{
	return tuple_cell::atomic_deep(xs_string,"");	
}*/
bool getStringParameter(PPOpIn content)
{
	
	tuple value(content.ts);
	content.op->next(value);
	sequence at_vals(1);
	if (value.is_eos()) 
	{
		str_val.clear();
	 	str_val.append(EMPTY_STRING_TC);
		return true;
	}
	else
	{
		
		at_vals.add(value);
		/*tuple_cell res=atomize(value.cells[0]);
		res=cast_to_xs_string(res);
		sbuf->append(res);*/
		content.op->next(value);

	}
//	int charsize=1;
	while (!(value.is_eos()))
	{
		if (!(value.cells_number==1 )) throw USER_EXCEPTION2(SE1003, "in PPConstructor");
		at_vals.add(value);
		content.op->next(value);
	}
	str_val.clear();
	sequence::iterator it=at_vals.begin();
	do
	{
		tuple_cell res=atomize((*it).cells[0]);
		res=cast(res, xs_string);
		res=tuple_cell::make_sure_light_atomic(res);
	/*	if (it!=at_vals.begin())
		{
			str_val.append(" ");				
		}
		*/
		str_val.append(res);
		it++;
        
	}
	while (it!=at_vals.end());
	return false;
	//str_val.push_to_memory();
}
void getStringWSParameter(PPOpIn content)
{
	str_val.clear();
	tuple value(content.ts);
	content.op->next(value);
	sequence at_vals(1);
	if (value.is_eos()) 
	{
	 	//tuple_cell result=tuple_cell::atomic_deep(xs_string,"");
		//return result;
		//str_val.append(EMPTY_STRING_TC);
		return;
	}
	//vector<tuple_cell> at_vals;   
	//int charsize=1;
	while (!(value.is_eos()))
	{
		if (!(value.cells_number==1 )) throw USER_EXCEPTION2(SE1003, "in PPConstructor");
		at_vals.add(value);
		content.op->next(value);
	}
	str_val.clear();
	sequence::iterator it=at_vals.begin();
	do
	{
		tuple_cell res=atomize((*it).cells[0]);
		res=cast(res, xs_string);
		res=tuple_cell::make_sure_light_atomic(res);
		str_val.append(res);
		it++;
        
	}
	while (it!=at_vals.end());
	//str_val.push_to_memory();
	
}
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPElementConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
bool PPConstructor::checkInitial()
	{
		if (firstCons)
		{
			firstCons=false;
			root_schema =	schema_node::init(NULL,NULL,NULL, virtual_root,false);
			xptr blk=createNewBlock(root_schema,false);
			node_blk_hdr* block_hdr=(node_blk_hdr*) XADDR(blk);
			n_dsc* node= GETPOINTERTODESC(block_hdr,block_hdr->free_first);
			block_hdr->free_first=*((shft*)node);
			block_hdr->desc_first=CALCSHIFT(node,block_hdr);
			block_hdr->desc_last=block_hdr->desc_first;
			d_dsc::init(node);
			virt_root=ADDR2XPTR(node);
			block_hdr->count=1;
			xptr tmp=add_record_to_indirection_table(virt_root);
			CHECKP(virt_root);
			node->indir=tmp;
			cont_parind=XNULL;
			cont_leftind=XNULL;
			conscnt=0;
			//d_printf1("CREATE NID for virt_root\n");
			nid_create_root(virt_root,false);
            //nid_print(virt_root);
			last_elem=XNULL;
			return true;
		 }
		else return false;

	}
void PPConstructor::open  ()
{
	if (checkInitial()==true) schema_carrier=true;
}


PPElementConstructor::PPElementConstructor(variable_context *_cxt_, 
            PPOpIn _qname_, PPOpIn _content_,bool _deep_copy, bool _ns_inside): PPConstructor(_cxt_, _deep_copy),
                                   qname(_qname_), content(_content_),ns_inside(_ns_inside)
{
	el_name=NULL;
	
}
PPElementConstructor::PPElementConstructor(variable_context *_cxt_, 
					 const char* name, PPOpIn _content_,bool _deep_copy, bool _ns_inside): PPConstructor(_cxt_, _deep_copy),
                                    content(_content_),ns_inside(_ns_inside)
{
	el_name=new char[strlen(name)+1];
	strcpy(el_name,name);
	
}
PPElementConstructor::~PPElementConstructor()
{
	
	if (el_name!=NULL) 
    {
		delete [] el_name;
    }
	else
	{
		delete qname.op;
		qname.op = NULL;
	}

	delete content.op;

    content.op = NULL;
	

	if (schema_carrier)
	{
        //nid_print(virt_root);
		nid_delete(virt_root);

		root_schema->delete_scheme_node();

		firstCons=true; // !!! false !!! There was false before... (Andrey)
	}
}

void PPElementConstructor::open  ()
{
    schema_carrier=checkInitial();
	if (el_name==NULL)	qname.op->open();
	content.op->open();
    first_time = true;
    eos_reached = true;
}

void PPElementConstructor::reopen()
{
    if (el_name==NULL)	qname.op->reopen();
	content.op->reopen();

    first_time = true;
    eos_reached = true;
}

void PPElementConstructor::close ()
{
    if (el_name==NULL) qname.op->close();
	content.op->close();
}

void PPElementConstructor::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;
		
		//Name parameter
		const char* name=el_name;
		char *z;
		bool need_deall=false;
		tuple_cell res;
		if (name==NULL)
		{
			res=getQnameParameter(qname);
			name=res.get_str_mem();
		}
		//context save
		xptr parind=cont_parind;
		xptr leftind=cont_leftind;
		cont_parind=XNULL;
		cont_leftind=XNULL;
		int oldcnt=conscnt;
		//crm_out<<"\n befory body cnt in "<<name<<" = "<<conscnt;
		//Preliminaries for static context
		vector<xml_ns*> ns_list;
		vector<tuple> start_seq;
		tuple cont(content.ts);
		if (ns_inside)
		{
			content.op->next(cont);
			while (true)
			{
				start_seq.push_back(cont);
				if(cont.is_eos()||cont.cells[0].is_atomic()) break;
				tuple_cell tc=cont.cells[0];
				xptr node=tc.get_node();
				CHECKP(node);
				t_item typ=GETTYPE(GETSCHEMENODE(XADDR(node)));
				if(typ!=xml_namespace)
					break;
				//ns_list.push_back(((ns_dsc*)XADDR(node))->ns);
				content.op->next(cont);
			}
		}
		//namespace search
		
		char* prefix=NULL;
		xml_ns* ns=NULL;
		if (!res.is_eos()&&res.get_atomic_type()==xs_QName)
		{
			//prefix=(char*)xs_QName_get_prefix(name);
			ns=xs_QName_get_xmlns(name);
			name=xs_QName_get_local_name(name);
		}
		else
		{
			separateLocalAndPrefix(prefix,name);
			if (prefix!=NULL)
			{
				ns=st_ct.get_xmlns_by_prefix(prefix);
				delete prefix;
			}
		}
		//Element insertion
		xptr new_element;
		if (parind==XNULL || deep_copy)
		{
			new_element= insert_element(removeIndirection(last_elem),XNULL,virt_root,name,xs_untyped,ns);
			last_elem=((n_dsc*)XADDR(new_element))->indir;
		}
		else
		{
			if (leftind!=XNULL)
				new_element= insert_element(removeIndirection(leftind),XNULL,XNULL,name,xs_untyped,ns);
			else
				new_element= insert_element(XNULL,XNULL,removeIndirection(parind),name,xs_untyped,ns);
			conscnt++;
		}
		int cnt=conscnt;
		
		//xptr local_last=new_element;
		xptr indir=((n_dsc*)XADDR(new_element))->indir;
		//context change
		cont_parind=indir;
		cont_leftind=XNULL;
		//MAIN PART
		sequence at_vals(1);
		xptr left=XNULL;
		bool mark_attr=true;
		bool mark_vec=true;
		vector<tuple>::iterator it_st;
		tuple* cont_ptr=NULL;
		if (ns_inside)
		{
			it_st=start_seq.begin();
			cont_ptr=&(*it_st);
		}
		else
		{
			content.op->next(cont);
			it_st=start_seq.end();
			cont_ptr=&cont;
		}
		while (!cont_ptr->is_eos())
		{
			//print_tuple(cont,crm_out);
			tuple_cell tc=cont_ptr->cells[0];
			if (tc.is_atomic())
			{
				at_vals.add(*cont_ptr);
				//if (val->get_size()>0) val->append(" ");
				//val->append(cont_ptr);
			}
			else
			{
				if (at_vals.size()>0)
				{
				 //normalize
					tuple_cell tcc;
					str_val.clear();
					sequence::iterator it=at_vals.begin();
					do
					{
						/*if (it!=at_vals.begin())
						{
							str_val.append(" ");						
						}*/
						tcc=tuple_cell::make_sure_light_atomic((*it).cells[0]);
						tcc=cast(tcc, xs_string);
						str_val.append(tcc);
						it++;

					}
					while (it!=at_vals.end());
					at_vals.clear();
					if(str_val.get_size()>0)
					left=insert_text(left,XNULL,removeIndirection(indir),str_val.get_ptr_to_text(),str_val.get_size(),str_val.get_type());
					mark_attr=false;
				}
				xptr node=tc.get_node();
				CHECKP(node);
				t_item typ=GETTYPE(GETSCHEMENODE(XADDR(node)));
				switch (typ)
				{
				case xml_namespace:ns_list.push_back(((ns_dsc*)XADDR(node))->ns);break;
				case document:
				case element: 
					{
						mark_attr=false;
						break;
					}
				case text:
					{
						if (((t_dsc*)XADDR(node))->size==0) 
						{
							if (it_st!=start_seq.end())
			{
				it_st++;
				if (it_st==start_seq.end())	
				{
					content.op->next(cont);
					cont_ptr=&cont;
				}
				else cont_ptr=&(*it_st);

			}
							else
								content.op->next(cont);
							continue;
						}
						mark_attr=false;
						break;
					}
				case attribute:
					{
						if (!mark_attr) throw USER_EXCEPTION(XQTY0024);
					}
				}
				if (conscnt>cnt)
				{
					left=node;
					cnt=conscnt;
					CHECKP(left);
					cont_leftind=((n_dsc*)XADDR(left))->indir;
				}
				else
				{
					if (typ==document)
					{
						xptr res = copy_content(removeIndirection(indir),node,left,false);
						if (res!=XNULL)					
						{
							left=res;
							cont_leftind=((n_dsc*)XADDR(left))->indir;
						}						
					}
					else
					{
						left=deep_pers_copy(left,XNULL,removeIndirection(indir),node,false);
						cont_leftind=((n_dsc*)XADDR(left))->indir;
					}
						
				}
				
			}
			/*if (last_elem==local_last)
			{
				last_elem=removeIndirection(indir);
				local_last=last_elem;
			}*/
			if (it_st!=start_seq.end())
			{
				it_st++;
				if (it_st==start_seq.end())	
				{
					content.op->next(cont);
					cont_ptr=&cont;
				}
				else cont_ptr=&(*it_st);

			}
			else
				content.op->next(cont);
		}
		if (at_vals.size()>0)
		{
					str_val.clear();
					tuple_cell tcc;
					sequence::iterator it=at_vals.begin();
					do
					{
						/*if (it!=at_vals.begin())
						{
							str_val.append(" ");						
						}*/
						tcc=tuple_cell::make_sure_light_atomic((*it).cells[0]);
						tcc=cast(tcc, xs_string);
						str_val.append(tcc);
						it++;

					}
					while (it!=at_vals.end());
					at_vals.clear();
					if(str_val.get_size()>0)
					left=insert_text(left,XNULL,removeIndirection(indir),str_val.get_ptr_to_text(),str_val.get_size(),str_val.get_type());
		}
		//Result
		/*if (last_elem==local_last)
			last_elem=removeIndirection(indir);*/
		t.copy(tuple_cell::node(removeIndirection(indir)));
		//clear in-scope context deleteng local namespace declarations
		vector<xml_ns*>::iterator it=ns_list.begin();
		while (it!=ns_list.end())
		{
			st_ct.remove_from_context(*it);
			it++;
		}
		//context return;
		cont_parind=parind;
		cont_leftind=XNULL;
		if (deep_copy)
			conscnt=oldcnt;
		//crm_out<<"\n after body cnt in "<<name<<" = "<<conscnt;
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPElementConstructor::copy(variable_context *_cxt_)
{
	PPElementConstructor *res ;
	if (el_name!=NULL)
		res = new PPElementConstructor(_cxt_, el_name,content,deep_copy,ns_inside);
	else
	{
		res = new PPElementConstructor(_cxt_, qname,content,deep_copy,ns_inside);
		res->qname.op = qname.op->copy(_cxt_);
	}
	res->content.op = content.op->copy(_cxt_);
    return res;
}

bool PPElementConstructor::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    /*INSERT OPERATION HERE*/
    return true;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAttributeConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPAttributeConstructor::PPAttributeConstructor(variable_context *_cxt_, 
            PPOpIn _qname_, PPOpIn _content_,bool _deep_copy): PPConstructor(_cxt_, _deep_copy),
                                   qname(_qname_), content(_content_)
{
	at_name=NULL;
	at_value=NULL;
	//val=new ustring_buffer;
}
PPAttributeConstructor::PPAttributeConstructor(variable_context *_cxt_, 
					 const char* name, PPOpIn _content_,bool _deep_copy): PPConstructor(_cxt_, _deep_copy),
                                    content(_content_)
{
	at_name=new char[strlen(name)+1];
	strcpy(at_name,name);
	at_value=NULL;
	
}
PPAttributeConstructor::PPAttributeConstructor(variable_context *_cxt_, 
            PPOpIn _qname_, const char* value,bool _deep_copy): PPConstructor(_cxt_, _deep_copy),
                                   qname(_qname_)
{
	at_name=NULL;
	at_value=new char[strlen(value)+1];
	strcpy(at_value,value);
}
PPAttributeConstructor::PPAttributeConstructor(variable_context *_cxt_, 
					 const char* name, const char* value,bool _deep_copy): PPConstructor(_cxt_, _deep_copy)
{
	at_name=new char[strlen(name)+1];
	strcpy(at_name,name);
	at_value=new char[strlen(value)+1];
	strcpy(at_value,value);
}
PPAttributeConstructor::~PPAttributeConstructor()
{
	
	if (at_name!=NULL) 
		delete [] at_name;
	else
	{
		delete qname.op;
		qname.op = NULL;
	}
	if (at_value!=NULL) 
		delete [] at_value;
	else
	{
		delete content.op;
		content.op = NULL;		
	}
	if (schema_carrier)
	{
		nid_delete(virt_root);
		root_schema->delete_scheme_node();
		firstCons=true; // !!! false !!! There was false before... (Andrey)
	}
}

void PPAttributeConstructor::open  ()
{
    schema_carrier=checkInitial();
	if (at_name==NULL)	qname.op->open();
	if (at_value==NULL) content.op->open();
    first_time = true;
    eos_reached = true;
}

void PPAttributeConstructor::reopen()
{
    if (at_name==NULL)	qname.op->reopen();
	if (at_value==NULL) content.op->reopen();
	first_time = true;
    eos_reached = true;
}

void PPAttributeConstructor::close ()
{
    if (at_name==NULL)	qname.op->close();
	if (at_value==NULL) content.op->close();
}

void PPAttributeConstructor::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;
		
		//Name parameter
		const char* name=at_name;
		char *z;
		bool need_deall=false;
		tuple_cell res1;
		if (name==NULL)
		{
			res1=getQnameParameter(qname);
			name=res1.get_str_mem();
		}
		char* prefix=NULL;
		
		xml_ns* ns=NULL;
		if (!res1.is_eos()&&res1.get_atomic_type()==xs_QName)
		{
			ns=xs_QName_get_xmlns(name);
			name=xs_QName_get_local_name(name);
			if (((ns->prefix==NULL||my_strcmp(ns->prefix,"")==0) && my_strcmp(name,"xmlns")==0)
				||(ns->prefix!=NULL && my_strcmp(ns->prefix,"http://www.w3.org/2000/xmlns/")==0 ))
				throw USER_EXCEPTION(XQDY0044);
		}
		else
		{
			separateLocalAndPrefix(prefix,name);
			if (((prefix==NULL||my_strcmp(prefix,"")==0) && my_strcmp(name,"xmlns")==0)
				||(prefix!=NULL && my_strcmp(prefix,"http://www.w3.org/2000/xmlns/")==0 ))
				throw USER_EXCEPTION(XQDY0044);
			if (prefix!=NULL)
			{
				ns=st_ct.get_xmlns_by_prefix(prefix);
				delete prefix;			
			}
		}
		const char* value=at_value;
		tuple_cell res;
		int size;
		if (value==NULL)
		{
			getStringWSParameter(content);
			value=(char*)str_val.c_str();
			size=str_val.get_size();
		}
		else 
			size=strlen(value);

		//Attribute insertion
		xptr new_attribute;
		if (cont_parind==XNULL || deep_copy)
			new_attribute= insert_attribute(XNULL,XNULL,virt_root,name,xs_untypedAtomic,value,size,ns);
		else
		{
			if (cont_leftind!=XNULL)
				new_attribute= insert_attribute(removeIndirection(cont_leftind),XNULL,XNULL,name,xs_untypedAtomic,value,size,ns);
			else
				new_attribute= insert_attribute(XNULL,XNULL,removeIndirection(cont_parind),name,xs_untypedAtomic,value,size,ns);
			conscnt++;
			cont_leftind=((n_dsc*)XADDR(new_attribute))->indir;			
		}
		//Result
		t.copy(tuple_cell::node(new_attribute));
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPAttributeConstructor::copy(variable_context *_cxt_)
{
	PPAttributeConstructor *res ;
	if (at_name!=NULL)
	{
		if (at_value!=NULL)	res = new PPAttributeConstructor(_cxt_, at_name,at_value, deep_copy);
		else res = new PPAttributeConstructor(_cxt_, at_name,content, deep_copy);
	}
	else
	{
		if (at_value!=NULL)	res = new PPAttributeConstructor(_cxt_, qname,at_value, deep_copy);
		else res = new PPAttributeConstructor(_cxt_, qname,content, deep_copy);
	}
    if (at_name==NULL)res->qname.op = qname.op->copy(_cxt_);
	if (at_value==NULL)res->content.op = content.op->copy(_cxt_);
    return res;
}

bool PPAttributeConstructor::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    /*INSERT OPERATION HERE*/
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPNamespaceConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPNamespaceConstructor::PPNamespaceConstructor(variable_context *_cxt_, 
					 const char* name, PPOpIn _content_): PPConstructor(_cxt_, true),
                                    content(_content_)
{
	if (name!=NULL&&strlen(name)!=0)
	{
		at_name=new char[strlen(name)+1];
		strcpy(at_name,name);
	}
	else
		at_name=NULL;
	at_value=NULL;
	
}

PPNamespaceConstructor::PPNamespaceConstructor(variable_context *_cxt_, 
					 const char* name, const char* value): PPConstructor(_cxt_,true)
{
	if (name!=NULL&&strlen(name)!=0)
	{
		at_name=new char[strlen(name)+1];
		strcpy(at_name,name);
	}
	else
		at_name=NULL;
	at_value=new char[strlen(value)+1];
	strcpy(at_value,value);
}
PPNamespaceConstructor::~PPNamespaceConstructor()
{
	
	if (at_name!=NULL) 
		delete [] at_name;
	
	if (at_value!=NULL) 
		delete [] at_value;
	else
	{
		delete content.op;
		content.op = NULL;
		
	}
	if (schema_carrier)
	{
		nid_delete(virt_root);
		root_schema->delete_scheme_node();
		firstCons=true; // !!! false !!! There was false before... (Andrey)
	}
}

void PPNamespaceConstructor::open  ()
{
    schema_carrier=checkInitial();
	if (at_value==NULL) content.op->open();
    first_time = true;
    eos_reached = true;
}

void PPNamespaceConstructor::reopen()
{
    if (at_value==NULL) content.op->reopen();
	first_time = true;
    eos_reached = true;
}

void PPNamespaceConstructor::close ()
{
    if (at_value==NULL) content.op->close();
}

void PPNamespaceConstructor::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;
		//xml_ns* parameter
		const char* prefix=at_name;
		const char* uri=at_value;
		tuple_cell res;
		if (uri==NULL)
		{
			getStringParameter(content);
			uri=(char*)str_val.c_str();
		}
		xml_ns* ns=st_ct.add_to_context(prefix,uri);
		
		//Element insertion
		xptr new_namespace= insert_namespace(XNULL,XNULL,virt_root,ns);
		//Result
		t.copy(tuple_cell::node(new_namespace));
	
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPNamespaceConstructor::copy(variable_context *_cxt_)
{
	PPNamespaceConstructor *res ;
	if (at_value!=NULL)	res = new PPNamespaceConstructor(_cxt_, at_name,at_value);
	else res = new PPNamespaceConstructor(_cxt_, at_name,content);
	if (at_value==NULL)res->content.op = content.op->copy(_cxt_);
    return res;
}

bool PPNamespaceConstructor::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    /*INSERT OPERATION HERE*/
    return true;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPCommentConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPCommentConstructor::PPCommentConstructor(variable_context *_cxt_, 
					 PPOpIn _content_,bool _deep_copy): PPConstructor(_cxt_, _deep_copy),
                                    content(_content_)
{
	at_value=NULL;
	strm.add_str("--","-");
	
}

PPCommentConstructor::PPCommentConstructor(variable_context *_cxt_, 
					 const char* value,bool _deep_copy): PPConstructor(_cxt_, _deep_copy)
{
	at_value=new char[strlen(value)+1];
	strcpy(at_value,value);
	strm.add_str("--","-");
}
PPCommentConstructor::~PPCommentConstructor()
{
	
	if (at_value!=NULL) 
		delete [] at_value;
	else
	{
		delete content.op;
		content.op = NULL;
		
	}
	if (schema_carrier)
	{
		nid_delete(virt_root);
		root_schema->delete_scheme_node();
		firstCons=true; // !!! false !!! There was false before... (Andrey)
	}
}

void PPCommentConstructor::open  ()
{
    schema_carrier=checkInitial();
	if (at_value==NULL) content.op->open();
    first_time = true;
    eos_reached = true;
}

void PPCommentConstructor::reopen()
{
    if (at_value==NULL) content.op->reopen();
	first_time = true;
    eos_reached = true;
}

void PPCommentConstructor::close ()
{
    if (at_value==NULL) content.op->close();
}

void PPCommentConstructor::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;
		const char* value=at_value;
		int size=0;
		tuple_cell res;
		if (value==NULL)
		{
			getStringParameter(content);
			value=(char*)str_val.c_str();
			size=str_val.get_size();
		}
		else
			size=strlen(value);
		
		int rst=strm.parse(value,size,NULL,NULL);
		if (rst==1||(size>0 && value[size-1]=='-')) 
			throw USER_EXCEPTION(XQDY0072);
		xptr newcomm;
		if (cont_parind==XNULL || deep_copy )
			newcomm= insert_comment(XNULL,XNULL,virt_root,value,size);
		else
		{
			if (cont_leftind!=XNULL)
				newcomm= insert_comment(removeIndirection(cont_leftind),XNULL,XNULL,value,size);
			else
				newcomm= insert_comment(XNULL,XNULL,removeIndirection(cont_parind),value,size);
			conscnt++;
			cont_leftind=((n_dsc*)XADDR(newcomm))->indir;			
		}
		//Result
		t.copy(tuple_cell::node(newcomm));
	
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPCommentConstructor::copy(variable_context *_cxt_)
{
	PPCommentConstructor *res ;
	if (at_value!=NULL)	res = new PPCommentConstructor(_cxt_, at_value, deep_copy);
	else 
	{
		res = new PPCommentConstructor(_cxt_, content, deep_copy);
		res->content.op = content.op->copy(_cxt_);
	}
    return res;
}

bool PPCommentConstructor::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    /*INSERT OPERATION HERE*/
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPIConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPPIConstructor::PPPIConstructor(variable_context *_cxt_, 
            PPOpIn _qname_, PPOpIn _content_,bool _deep_copy): PPConstructor(_cxt_, _deep_copy),
                                   qname(_qname_), content(_content_)
{
	at_name=NULL;
	at_value=NULL;
	strm.add_str("?>","--");
	
}
PPPIConstructor::PPPIConstructor(variable_context *_cxt_, 
					 const char* name, PPOpIn _content_,bool _deep_copy): PPConstructor(_cxt_, _deep_copy),
                                    content(_content_)
{
	at_name=new char[strlen(name)+1];
	strcpy(at_name,name);
	at_value=NULL;
	strm.add_str("?>","--");
	
}
PPPIConstructor::PPPIConstructor(variable_context *_cxt_, 
            PPOpIn _qname_, const char* value,bool _deep_copy): PPConstructor(_cxt_, _deep_copy),
                                   qname(_qname_)
{
	at_name=NULL;
	at_value=new char[strlen(value)+1];
	strcpy(at_value,value);
	strm.add_str("?>","--");
}
PPPIConstructor::PPPIConstructor(variable_context *_cxt_, 
					 const char* name, const char* value,bool _deep_copy): PPConstructor(_cxt_, _deep_copy)
{
	at_name=new char[strlen(name)+1];
	strcpy(at_name,name);
	at_value=new char[strlen(value)+1];
	strcpy(at_value,value);
	strm.add_str("?>","--");
}
PPPIConstructor::~PPPIConstructor()
{
	
	if (at_name!=NULL) 
		delete [] at_name;
	else
	{
		delete qname.op;
		qname.op = NULL;
	}
	if (at_value!=NULL) 
		delete [] at_value;
	else
	{
		delete content.op;
		content.op = NULL;
		
	}
	if (schema_carrier)
	{
		nid_delete(virt_root);
		root_schema->delete_scheme_node();
		firstCons=true; // !!! false !!! There was false before... (Andrey)
	}
}

void PPPIConstructor::open  ()
{
    schema_carrier=checkInitial();
	if (at_name==NULL)	qname.op->open();
	if (at_value==NULL) content.op->open();
    first_time = true;
    eos_reached = true;
}

void PPPIConstructor::reopen()
{
    if (at_name==NULL)	qname.op->reopen();
	if (at_value==NULL) content.op->reopen();
	first_time = true;
    eos_reached = true;
}

void PPPIConstructor::close ()
{
    if (at_name==NULL)	qname.op->close();
	if (at_value==NULL) content.op->close();
}

void PPPIConstructor::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;
		
		//Name parameter
		const char* name=at_name;
		char *z;
		bool need_deall=false;
		tuple_cell res1;
		if (name==NULL)
		{
			res1=getQnameParameter(qname);
			name=res1.get_str_mem();
		}
		char* prefix=NULL;
		if (!res1.is_eos()&&res1.get_atomic_type()==xs_QName)
		{
			prefix=(char*)xs_QName_get_prefix(name);
			name=xs_QName_get_local_name(name);
			if (prefix!=NULL)
				throw USER_EXCEPTION(XQDY0041);			
		}
		else
		{
			separateLocalAndPrefix(prefix,name);
			if (prefix!=NULL)
			{
			    delete prefix;
				throw USER_EXCEPTION(XQDY0041);
			}
		}
		const char* value=at_value;
		tuple_cell res;
		int size;
		if (value==NULL)
		{
			getStringParameter(content);
			value=(char*)str_val.c_str();
			size=str_val.get_size();
		}
		else 
			size=strlen(value);
		int rst=strm.parse(value,size,NULL,NULL);
		if (rst==1) 
			throw USER_EXCEPTION(XQDY0026);
		//Attribute insertion
		xptr new_pi;
		if (cont_parind==XNULL || deep_copy)
			new_pi= insert_pi(XNULL,XNULL,virt_root,name,strlen(name),value,size);
		else
		{
			if (cont_leftind!=XNULL)
				new_pi= insert_pi(removeIndirection(cont_leftind),XNULL,XNULL,name,strlen(name),value,size);
			else
				new_pi= insert_pi(XNULL,XNULL,removeIndirection(cont_parind),name,strlen(name),value,size);
			conscnt++;
			cont_leftind=((n_dsc*)XADDR(new_pi))->indir;			
		}
		//Result
		t.copy(tuple_cell::node(new_pi));
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPPIConstructor::copy(variable_context *_cxt_)
{
	PPPIConstructor *res ;
	if (at_name!=NULL)
	{
		if (at_value!=NULL)	res = new PPPIConstructor(_cxt_, at_name,at_value, deep_copy);
		else res = new PPPIConstructor(_cxt_, at_name,content, deep_copy);
	}
	else
	{
		if (at_value!=NULL)	res = new PPPIConstructor(_cxt_, qname,at_value, deep_copy);
		else res = new PPPIConstructor(_cxt_, qname,content, deep_copy);
	}
    if (at_name==NULL)res->qname.op = qname.op->copy(_cxt_);
	if (at_value==NULL)res->content.op = content.op->copy(_cxt_);
    return res;
}

bool PPPIConstructor::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    /*INSERT OPERATION HERE*/
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPTextConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPTextConstructor::PPTextConstructor(variable_context *_cxt_, 
					 PPOpIn _content_,bool _deep_copy): PPConstructor(_cxt_, _deep_copy),
                                    content(_content_)
{
	at_value=NULL;
	
}

PPTextConstructor::PPTextConstructor(variable_context *_cxt_, 
					 const char* value,bool _deep_copy): PPConstructor(_cxt_, _deep_copy)
{
	at_value=new char[strlen(value)+1];
	strcpy(at_value,value);
}
PPTextConstructor::~PPTextConstructor()
{
	
	if (at_value!=NULL) 
		delete [] at_value;
	else
	{
		delete content.op;
		content.op = NULL;
		
	}
	if (schema_carrier)
	{
		nid_delete(virt_root);
		root_schema->delete_scheme_node();
		firstCons=true; // !!! false !!! There was false before... (Andrey)
	}
}

void PPTextConstructor::open  ()
{
    schema_carrier=checkInitial();
	if (at_value==NULL) content.op->open();
    first_time = true;
    eos_reached = true;
}

void PPTextConstructor::reopen()
{
    if (at_value==NULL) content.op->reopen();
	first_time = true;
    eos_reached = true;
}

void PPTextConstructor::close ()
{
    if (at_value==NULL) content.op->close();
}

void PPTextConstructor::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;
		const char* value=at_value;
		int size=0;
		tuple_cell res;
		if (value==NULL)
		{
			if (getStringParameter(content))
			{
				t.set_eos();
				return;
			}
			value=(char*)str_val.c_str();
			
			size=str_val.get_size();
		}
		else
			size=strlen(value);
		xptr newcomm;
		if (cont_parind==XNULL || deep_copy || size==0)
			newcomm= insert_text(XNULL,XNULL,virt_root,value,size);
		else
		{
			if (cont_leftind!=XNULL)
				newcomm= insert_text(removeIndirection(cont_leftind),XNULL,XNULL,value,size);
			else
				newcomm= insert_text(XNULL,XNULL,removeIndirection(cont_parind),value,size);
			conscnt++;
			cont_leftind=((n_dsc*)XADDR(newcomm))->indir;			
		}
		//Result
		t.copy(tuple_cell::node(newcomm));
	
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPTextConstructor::copy(variable_context *_cxt_)
{
	PPTextConstructor *res ;
	if (at_value!=NULL)	res = new PPTextConstructor(_cxt_, at_value, deep_copy);
	else 
	{
		res = new PPTextConstructor(_cxt_, content, deep_copy);
		res->content.op = content.op->copy(_cxt_);
	}
    return res;
}

bool PPTextConstructor::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    /*INSERT OPERATION HERE*/
    return true;
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDocumentConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPDocumentConstructor::PPDocumentConstructor(variable_context *_cxt_, 
					 PPOpIn _content_): PPConstructor(_cxt_,false),
                                    content(_content_)
{
	
	
}

PPDocumentConstructor::~PPDocumentConstructor()
{
	
	delete content.op;
	content.op = NULL;
}

void PPDocumentConstructor::open  ()
{
	content.op->open();
    first_time = true;
    eos_reached = true;
}

void PPDocumentConstructor::reopen()
{
    content.op->reopen();
	first_time = true;
    eos_reached = true;
}

void PPDocumentConstructor::close ()
{
    content.op->close();
}

void PPDocumentConstructor::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;
		
		//Name parameter
		bool need_deall=false;
		tuple_cell res;
		//document insertion insertion
		//context save
		xptr parind=cont_parind;
		xptr leftind=cont_leftind;		
		int cnt=conscnt;
		int oldcnt=conscnt;
		xptr new_doc=insert_document("tmp",false);
		st_ct.temp_docs.push_back(new_doc);
		xptr indir=((n_dsc*)XADDR(new_doc))->indir;
		cont_parind=indir;
		cont_leftind=XNULL;
		sequence at_vals(1);
		xptr left=XNULL;
		content.op->next(t);
		while (!t.is_eos())
		{
			//print_tuple(cont,crm_out);
			tuple_cell tc=t.cells[0];
			if (tc.is_atomic())
			{
				at_vals.add(t);
				//if (val->get_size()>0) val->append(" ");
				//val->append(cont_ptr);
			}
			else
			{
				if (at_vals.size()>0)
				{
					tuple_cell tcc;
					str_val.clear();
					sequence::iterator it=at_vals.begin();
					do
					{
						tcc=tuple_cell::make_sure_light_atomic((*it).cells[0]);
						tcc=cast(tcc, xs_string);
						str_val.append(tcc);
						it++;
					}
					while (it!=at_vals.end());
					at_vals.clear();
					if(str_val.get_size()>0)
					left=insert_text(left,XNULL,removeIndirection(indir),str_val.get_ptr_to_text(),str_val.get_size(),str_val.get_type());
				}
				xptr node=tc.get_node();
				CHECKP(node);
				t_item typ=GETTYPE(GETSCHEMENODE(XADDR(node)));
				switch (typ)
				{
				case document:
				case element: 
					{
						break;
					}
				case text: 
					{
						if (((t_dsc*)XADDR(node))->size==0) 
						{
							content.op->next(t);
							continue;
						}
						break;
					}
				case attribute:
					{
						throw USER_EXCEPTION(XPTY0004);
					}
				}
				if (conscnt>cnt)
				{
					left=node;
					cnt=conscnt;
					CHECKP(left);
				}
				else
				{
					if (typ==document)
					{
						xptr res = copy_content(removeIndirection(indir),node,left,false);
						if (res!=XNULL)					
							left=res;
						else 
						{
							content.op->next(t);
							continue;	
						}
						
					}
					else
						left=deep_pers_copy(left,XNULL,removeIndirection(indir),node,false);
						
				}
				cont_leftind=((n_dsc*)XADDR(left))->indir;
			}
			
			content.op->next(t);
		}
		if (at_vals.size()>0)
		{
					str_val.clear();
					tuple_cell tcc;
					sequence::iterator it=at_vals.begin();
					do
					{
						tcc=tuple_cell::make_sure_light_atomic((*it).cells[0]);
						tcc=cast(tcc, xs_string);
						str_val.append(tcc);
						it++;
					}
					while (it!=at_vals.end());
					at_vals.clear();
					if(str_val.get_size()>0)
					left=insert_text(left,XNULL,removeIndirection(indir),str_val.get_ptr_to_text(),str_val.get_size(),str_val.get_type());
		}
		t.copy(tuple_cell::node(removeIndirection(indir)));
		cont_parind=parind;
		cont_leftind=XNULL;
		conscnt=oldcnt;
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDocumentConstructor::copy(variable_context *_cxt_)
{
	PPDocumentConstructor *res ;
	res = new PPDocumentConstructor(_cxt_, content);
	res->content.op = content.op->copy(_cxt_);
    return res;
}

bool PPDocumentConstructor::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    /*INSERT OPERATION HERE*/
    return true;
}

