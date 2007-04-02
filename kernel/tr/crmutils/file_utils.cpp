/*
 * File:  file_utils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include <math.h>
#include "tr/crmutils/crmutils.h"
#include "tr/structures/metadata.h"
#include "expat/expat.h"
#include "tr/structures/nodes.h"
#include "common/xptr.h"
#include "tr/mo/micro.h"
#include "tr/crmutils/node_utils.h"
#include "common/persistent_db_data.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#include "tr/updates/updates.h"
#endif


#define BUFFSIZE        8192
#define SEPARATOR '>'
char Buff[BUFFSIZE];
int mark;
xptr parent;
xptr left;
schema_node* sc_parent;
bool wpstrip;
//bool last_op_text;
bool text_inserted;
bool print_p = true;
int is_ns;
char* wptail;
int wptailsize;
int maxwpsize=0;
char* nodenames;
int maxnm=0;
int nodescnt=0;
int curcnt=0;
int curproc=0;
bool cdata_mode=false;
bool is_coll=false;

//std::vector<schema_node*> * curvect;
typedef std::pair<std::string,std::string> str_pair;

typedef  std::map< str_pair, xml_ns*> ns_map;
static ns_map  xm_nsp;
ns_map::iterator it_map;
std::vector<xml_ns*> nss;
//fanout stat

std::vector<int> curr_fo;
std::map<schema_node*,stat_pair> max_fo;
std::vector<stat_pair*> curp;
//pair
void remove_hints(schema_node* nd)
{
	nd->cl_hint=0;
	sc_ref* sc=nd->first_child;
	while (sc!=NULL)
	{
		remove_hints(sc->snode);
		sc=sc->next;
	}
}
bool isWP(const char* data, int size )
{
	char s;
	for (int i=0;i<size;i++)
	{
		s=data[i];
		if (s!=32 && s!=9 && s!=10 && s!=13 )return false;
	}
	return true;
}
void separateName( const char* triplet,const char*& uri, const char*& local,const char*& prefix)
{
	const char* sec=NULL;
	const char* third=NULL;
	int sepcnt=0;
	for (int i=0; i<strlen(triplet);i++)
		if (triplet[i]==SEPARATOR)
		{
			if (sepcnt==0)
				sec=triplet+i+1;
			else
				third=triplet+i+1;
			sepcnt++;
		}
	if (sepcnt)
	{
		int tripsize=strlen(triplet)+1;
		if (tripsize>maxnm)
					{
						if (maxnm!=0)delete [] nodenames;
						maxnm=tripsize;
						nodenames= se_new char[tripsize];
					}
		memcpy(nodenames,triplet,tripsize);	
		uri=nodenames;
		local=nodenames+(sec-triplet);
		nodenames[sec-triplet-1]='\0';
		if (third!=NULL)
		{
			prefix=local+(third-sec);
			nodenames[third-triplet-1]='\0';
		}
		else prefix=NULL;
		if (strcmp(uri,"http://www.w3.org/XML/1998/namespace")==0)nodenames[0]='\0';
	
	}
	else
	{
		uri=NULL;
		local=triplet;
		prefix=NULL;
	}
}
void analyzeWP(const char** data, int& size )
{
 const char* ds=*data;
 const char* es=*data+(size-1);
 char s;
 int i;
 for ( i=0;i<size;i++)
 {
	 s=*ds;
	 if (s==32 || s==9 || s==10 || s==13 )
		 ds++;
	 else break;
 }
 size=size-i;
 *data=ds;
 for ( i=0;i<size;i++)
 {
	if (s==32 || s==9 || s==10 || s==13 )	es--;
	else break;
 }
 size=size-i;	
}
void processWP(const char** s, int& len)
{
	const char* d=*s;
	int size=len;
	analyzeWP(&d,size);
	if (size!=0||text_inserted)
	{
		if (wptailsize>0)
		{
			xptr new_node;
			if (mark)
				new_node=insert_text(XNULL,XNULL,parent,wptail,wptailsize);
			else
			{
				new_node=insert_text(left,XNULL,XNULL,wptail,wptailsize);
			}
#ifdef SE_ENABLE_FTSEARCH
			if (is_coll)
				update_insert_sequence(new_node,(GETBLOCKBYNODE(new_node))->snode->ft_index_object); 
#endif
			mark=0;
			left=new_node;
			CHECKP(left);
			xptr par_ind=((n_dsc*)XADDR(left))->pdsc;
			CHECKP(par_ind);
			parent=*((xptr*)XADDR(par_ind));
					//left=insert_text(left,XNULL,XNULL,wptail,wptailsize);
		}
		wptailsize=0;
	}
	else
	{
		if (wptailsize+len>maxwpsize)
		{
			char* z=se_new char[wptailsize+len];
			if (wptailsize>0) memcpy(z,wptail,wptailsize);
			memcpy(z+wptailsize,*s,len);
			wptailsize+=len;
			maxwpsize=wptailsize;
			delete [] wptail;
			wptail=z;
		}
		else
		{
			memcpy(wptail+wptailsize,*s,len);
			wptailsize+=len;
		}
		len=0;
	}
}
/*
	if (last_op_text)
		{
			if (size!=0)
			{
				if (wptailsize>0)
				{
					xptr new_node;
					if (mark)
						new_node=insert_text(XNULL,XNULL,parent,wptail,wptailsize);
					else
					{
						new_node=insert_text(left,XNULL,XNULL,wptail,wptailsize);
					}
					mark=0;
					left=new_node;
					CHECKP(left);
					xptr par_ind=((n_dsc*)XADDR(left))->pdsc;
					CHECKP(par_ind);
					parent=*((xptr*)XADDR(par_ind));
					//left=insert_text(left,XNULL,XNULL,wptail,wptailsize);
				}
				//checkChildReferenceValidity(left);
				wptailsize=len-((int)d-(int)(*s)+size);
				if (wptailsize>0)
				{
					if (wptailsize>maxwpsize)
					{
						if (maxwpsize!=0)delete [] wptail;
						maxwpsize=wptailsize;
						wptail= se_new char[maxwpsize];
					}
					memcpy(wptail,d+size,wptailsize);					
				}
			}
			else
			{
				if (wptailsize+len>maxwpsize)
				{
					char* z=se_new char[wptailsize+len];
					if (wptailsize>0) memcpy(z,wptail,wptailsize);
					memcpy(z+wptailsize,*s,len);
					wptailsize+=len;
					maxwpsize=wptailsize;
					delete [] wptail;
					wptail=z;
				}
				else
				{
					memcpy(wptail+wptailsize,*s,len);
					wptailsize+=len;
				}
				len=0;
			}
		}
		else
		{
			last_op_text=true;
			if (size!=0)
			{
				wptailsize=0;
			}
			else
			{
				if (len>maxwpsize)
				{
					if (maxwpsize!=0)delete [] wptail;
					maxwpsize=len;
					wptail= se_new char[len];
				}
				memcpy(wptail,*s,len);	
				wptailsize=len;
				len=0;
			}
		}
		
}
*/
void clear_text()
{
	text_inserted=false;
	wptailsize=0;
}
static void start(void *s, const char *el, const char **attr)
{
    //d_printf1("bs");fflush(stdout);
//	crm_out<<"\n In Start PARSING ELEMENT"<<el <<endl;
//	test_cnt++;
	const char* uri;
	const char* local;
	const char* prefix;
	separateName((char*)el,uri,local,prefix);
	clear_text();
	xptr new_node;
	xml_ns* ns=NULL;
	
	if (uri!=NULL || prefix!=NULL)
	{
		if (!uri) uri="";
		else
            if (!prefix) prefix="";
		ns=xm_nsp.find(str_pair(prefix,uri))->second;
	}
	/*if (my_strcmp(local,"asia")==0)
	{
		d_printf1("be"); fflush(stdout);

	}*/
    //d_printf1("be"); fflush(stdout);
	if (mark)
		new_node=insert_element(XNULL,XNULL,parent,local,xs_untyped,ns);
	else
	{
		new_node=insert_element(left,XNULL,XNULL,local,xs_untyped,ns);
		mark=1;
	}
#ifdef SE_ENABLE_FTSEARCH
if (is_coll)
 update_insert_sequence(new_node,(GETBLOCKBYNODE(new_node))->snode->ft_index_object); 
#endif
    //d_printf1("ae\n"); fflush(stdout);
	//checkChildReferenceValidity(new_node);
	xptr par_ind=((n_dsc*)XADDR(new_node))->indir;
	stat_pair* pr=&max_fo[(GETBLOCKBYNODE(new_node))->snode];
	curp.push_back(pr);
	sizehnt= pr;
	parent=new_node;
	xptr att=XNULL;
	if (is_ns)
	{
		std::vector<xml_ns*>::const_iterator it=nss.begin();
		while(it!=nss.end())
		{
			att=insert_namespace(att,XNULL,(att==XNULL)?new_node:XNULL,*it);
			it++;
		}
		is_ns=0;
		nss.clear();
	}
    
//	TEMP PLEASE UNCOMMENT NEXT THREE LINES
    //d_printf1("ba"); fflush(stdout);
    for (int i = 0; attr[i]; i += 2) 
	{
		separateName((char*)attr[i],uri,local,prefix);
		if (uri!=NULL || prefix!=NULL)
		{
			if (!uri) uri="";
			else
				if (!prefix) prefix="";
			ns=xm_nsp.find(str_pair(prefix,uri))->second;
		}
		else ns=NULL;
		att=insert_attribute(att,XNULL,(att==XNULL)?new_node:XNULL,local,xs_untypedAtomic,attr[i + 1],strlen(attr[i + 1]),ns);
#ifdef SE_ENABLE_FTSEARCH
		if (is_coll)
			update_insert_sequence(att,(GETBLOCKBYNODE(att))->snode->ft_index_object); 
#endif
		//checkChildReferenceValidity(att);
	}
    //d_printf1("aa\n"); fflush(stdout);

	if (att!=XNULL) 
	{
		left=att;
		mark=0;
		parent=removeIndirection(par_ind);
	}
	curcnt++;
	if ((curcnt*100.)/nodescnt>curproc)
	{
	 if(print_p)
	  *(se_ostream*)s << curproc <<"%"<<endl;
	 curproc++;
	}
    //d_printf1("as\n");fflush(stdout);
}
static void end(void *s, const char *el)
{
  /*crm_out<<"\n In End PARSING ELEMENT"<<el <<endl;
  if ( my_strcmp( ((node_blk_hdr*)((int)parent.addr & 0xFFFF0000))->snode->name,el)!=0)
  {
   crm_out<<"here";
  }*/
  clear_text();
  left=parent;
  CHECKP(left);
  xptr par_ind=((n_dsc*)XADDR(left))->pdsc;
  CHECKP(par_ind);
  parent=*((xptr*)XADDR(par_ind));
  /*
  CHECKP(parent);
  crm_out<<"\n In End PARENT ELEMENT"<<((node_blk_hdr*)((int)parent.addr & 0xFFFF0000))->snode->name <<endl;
  */
  mark=0;
  curp.pop_back();
  sizehnt=curp.back();
}

void data(void *userData, const char *s, int len)
{
    //d_printf1("bd");fflush(stdout);
	//return;
	//test_cnt++;
	if (cdata_mode)
	{
		if (wptailsize+len>maxwpsize)
		{
			char* z=se_new char[wptailsize+len];
			if (wptailsize>0) memcpy(z,wptail,wptailsize);
			memcpy(z+wptailsize,s,len);
			wptailsize+=len;
			maxwpsize=wptailsize;
			delete [] wptail;
			wptail=z;
		}
		else
		{
			memcpy(wptail+wptailsize,s,len);
			wptailsize+=len;
		}
		return;
	}
	if (wpstrip) 
	{
		processWP(&s,len);
		if (len==0) {/*d_printf1("ad\n");fflush(stdout);*/return; }
	}
	text_inserted=true;
	xptr new_node;
	if (mark)
		new_node=insert_text(XNULL,XNULL,parent,s,len);
	else
	{
		new_node=insert_text(left,XNULL,XNULL,s,len);
	}
#ifdef SE_ENABLE_FTSEARCH
	if (is_coll)
		update_insert_sequence(new_node,(GETBLOCKBYNODE(new_node))->snode->ft_index_object); 
#endif
	//checkTextNodeCorrectness(new_node);
	//checkChildReferenceValidity(new_node);
	mark=0;
	left=new_node;
    CHECKP(left);
	xptr par_ind=((n_dsc*)XADDR(left))->pdsc;
	CHECKP(par_ind);
	
#ifdef _MYDEBUG1
	if ((*((xptr*)XADDR(par_ind))).layer>0)
		{
			crm_out<<"Error";
		}
#endif
	parent=*((xptr*)XADDR(par_ind));
	/*if (parent!=XNULL)
	{
		CHECKP(parent);
		if (GETTYPE((GETBLOCKBYNODE(parent))->snode)!=element)
	 {
		crm_out<<"Error";
	 }
	}*/
	//mark=0;
    //d_printf1("ad\n");fflush(stdout);
}
static void sc_start(void *data, const char *el, const char **attr)
{
	const char* uri;
	const char* local;
	const char* prefix;
	separateName((char*)el,uri,local,prefix);
	xml_ns* ns=NULL;
	if (uri!=NULL || prefix!=NULL)
	{
		if (!uri) uri="";
		else
            if (!prefix) prefix="";
		ns=xm_nsp.find(str_pair(prefix,uri))->second;
	}
	schema_node* child=sc_parent->get_child(ns,local,element);
	if (child==NULL)child=sc_parent->add_child(ns,local,element);
	//statistics
	/*if (my_strcmp(sc_parent->name,"asia")==0)
	{
		d_printf1("be"); fflush(stdout);
	}*/
	child->cl_hint++;
	curr_fo.back()++;
	clear_text();
	curr_fo.push_back(0);
	sc_parent=child;
	if (is_ns)
	{
		if (sc_parent->get_child(NULL,NULL,xml_namespace)==NULL)
			sc_parent->add_child(NULL,NULL,xml_namespace);
		//statistics
		curr_fo.back()+=is_ns;
		
		schema_node* xsn=sc_parent->get_child(NULL,NULL,xml_namespace);
		xsn->cl_hint+=is_ns;
		is_ns=0;
	}
	schema_node* atts;
	nodescnt++;
    for (int i = 0; attr[i]; i += 2) 
	{
		separateName((char*)attr[i],uri,local,prefix);
		if (uri!=NULL || prefix!=NULL)
		{
			if (!uri) uri="";
			else
				if (!prefix) prefix="";
			ns=xm_nsp.find(str_pair(prefix,uri))->second;
		}
		else
			ns=NULL;
		atts=sc_parent->get_child(ns,local,attribute);
		if (atts==NULL)atts=sc_parent->add_child(ns,local,attribute);
		//statistics
		atts->cl_hint++;
		curr_fo.back()++;
	}
}
static void sc_end(void *data, const char *el)
{
	std::map<schema_node*,stat_pair>::iterator it= max_fo.find(sc_parent);
	if (it==max_fo.end())
		max_fo[sc_parent]=stat_pair(curr_fo.back(),0);
	else
		if (it->second.first<curr_fo.back()) it->second.first=curr_fo.back();
	curr_fo.pop_back();
	sc_parent=sc_parent->parent;
	clear_text();
}

void sc_data(void *userData, const char *s, int len)
{
	if (!text_inserted)
	{
		if (cdata_mode||(wpstrip && isWP(s,len))) return;
		schema_node* xsn=sc_parent->get_child(NULL,NULL,text);
		if (xsn==NULL)xsn=sc_parent->add_child(NULL,NULL,text);
		//statistics
       		xsn->cl_hint++;
		curr_fo.back()++;
		text_inserted=true;
	}
}

void el_ns (void *userData, const char *prefix, const char *uri)
{
	const char* prefixm;
	if (prefix==NULL)
		prefixm="";
	else
		prefixm=prefix;
	str_pair sp(prefixm,uri);
	it_map=xm_nsp.find(sp);
	if (it_map== xm_nsp.end() )
	{
		throw SYSTEM_EXCEPTION("Error of namespace parsing");
	}
	else
		nss.push_back(it_map->second);
	is_ns++;
}

void sc_ns (void *userData, const char *prefix, const char *uri)
{
	const char* prefixm;
	if (prefix==NULL)
		prefixm="";
	else
		prefixm=prefix;
	str_pair sp(prefixm,uri);
	it_map=xm_nsp.find(sp);
	if (it_map== xm_nsp.end() )
	{
		const char* urim=(strcmp(uri," ")==0)?NULL:uri;
		xml_ns* ns=(xml_ns*)(entry_point->nslist->find(urim,prefix));
		if (ns==NULL) 
		{
			ns=xml_ns::init(urim,prefix,true);
			entry_point->nslist->put(ns);
		}
		xm_nsp[sp]=ns;
	}
	is_ns++;
}
void sc_comment (void *userData, const char *data)
{
	schema_node* xsn=sc_parent->get_child(NULL,NULL,comment);
	if (xsn==NULL)xsn=sc_parent->add_child(NULL,NULL,comment);
	//statistics
	xsn->cl_hint++;
	curr_fo.back()++;
	clear_text();
}
void dt_comment (void *userData, const char *data)
{
	xptr new_node;
	clear_text();
	if (mark)
		new_node=insert_comment(XNULL,XNULL,parent,data,strlen(data));
	else
	{
		new_node=insert_comment(left,XNULL,XNULL,data,strlen(data));
	}
#ifdef SE_ENABLE_FTSEARCH
	if (is_coll)
		update_insert_sequence(new_node,(GETBLOCKBYNODE(new_node))->snode->ft_index_object); 
#endif
	mark=0;
	left=new_node;
    xptr par_ind=((n_dsc*)XADDR(left))->pdsc;
	parent=removeIndirection(par_ind);
}
void sc_cdata_start (void *userData)
{
	schema_node* xsn=sc_parent->get_child(NULL,NULL,cdata);
	if (xsn==NULL)xsn=sc_parent->add_child(NULL,NULL,cdata);
	//statistics
	xsn->cl_hint++;
	curr_fo.back()++;
	cdata_mode=true;
}
void sc_cdata_end (void *userData)
{
	cdata_mode=false;
}
void dt_cdata_start (void *userData)
{
	cdata_mode=true;
	clear_text();
}
void dt_cdata_end (void *userData)
{
	xptr new_node;
	cdata_mode=false;
	if (mark)
		new_node=insert_cdata(XNULL,XNULL,parent,wptail,wptailsize);
	else
	{
		new_node=insert_cdata(left,XNULL,XNULL,wptail,wptailsize);
	}
#ifdef SE_ENABLE_FTSEARCH
	if (is_coll)
		update_insert_sequence(new_node,(GETBLOCKBYNODE(new_node))->snode->ft_index_object); 
#endif
	mark=0;
	left=new_node;
    xptr par_ind=((n_dsc*)XADDR(left))->pdsc;
	parent=removeIndirection(par_ind);
	clear_text();	
}
void sc_pi (void *userData, const char *target, const char *data)
{
	schema_node* xsn=sc_parent->get_child(NULL,NULL,pr_ins);
	if (xsn==NULL)xsn=sc_parent->add_child(NULL,NULL,pr_ins);
	clear_text();
	//statistics
	xsn->cl_hint++;
	curr_fo.back()++;
}
void dt_pi (void *userData, const char *target, const char *data)
{
	xptr new_node;
	clear_text();
	if (mark)
		new_node=insert_pi(XNULL,XNULL,parent,target,strlen(target),data,strlen(data));
	else
	{
		new_node=insert_pi(left,XNULL,XNULL,target,strlen(target),data,strlen(data));
	}
#ifdef SE_ENABLE_FTSEARCH
	if (is_coll)
		update_insert_sequence(new_node,(GETBLOCKBYNODE(new_node))->snode->ft_index_object); 
#endif
	mark=0;
	left=new_node;
    xptr par_ind=((n_dsc*)XADDR(left))->pdsc;
	parent=removeIndirection(par_ind);
}
void parse_load(FILE* f, se_ostream &s)
{
	XML_Parser p = XML_ParserCreateNS(NULL,SEPARATOR);
    if (! p) throw USER_ENV_EXCEPTION("Couldn't allocate memory for parser\n",true);
	XML_SetReturnNSTriplet(p,1);
	int done;
    int len;
	XML_SetStartNamespaceDeclHandler(p,el_ns);
	XML_SetElementHandler(p, start, end);
	XML_SetCommentHandler(p, dt_comment);
	XML_SetProcessingInstructionHandler(p, dt_pi);
	XML_SetCharacterDataHandler(p, data);
	//XML_SetCdataSectionHandler(p,dt_cdata_start,dt_cdata_end);

    XML_SetUserData (p, &s);
	cdata_mode=false;
	len = fread(Buff, 1, BUFFSIZE, f);
    if (ferror(f)) 
	{
		XML_ParserFree(p);
        throw USER_ENV_EXCEPTION("Read error",true);
	}
	done = feof(f);
    while (!done) 
	{
		if (XML_Parse(p, Buff, len, done) != XML_STATUS_ERROR)
		{
			len = fread(Buff, 1, BUFFSIZE, f);
			if (ferror(f)) 
			{
				XML_ParserFree(p);
				throw USER_ENV_EXCEPTION("Read error",true);
			}
			done = feof(f);
		}
		else
		{
			char tmp[256]; 
			sprintf(tmp, "line %d:\n%s\n",
				XML_GetCurrentLineNumber(p),
				XML_ErrorString(XML_GetErrorCode(p)));
			XML_ParserFree(p);
			throw USER_EXCEPTION2(SE2005, tmp);
		}
	}
	if (XML_Parse(p, Buff, len, done)== XML_STATUS_ERROR)
	{
		char tmp[256]; 
		sprintf(tmp, "line %d:\n%s\n",
			XML_GetCurrentLineNumber(p),
			XML_ErrorString(XML_GetErrorCode(p)));
		XML_ParserFree(p);
		throw USER_EXCEPTION2(SE2005, tmp);
	}
	sizehnt=NULL;
	XML_ParserFree(p);
}

void parse_schema(FILE* f)
{
	xm_nsp.clear();
	xm_nsp[str_pair("xml","")]=(xml_ns*)entry_point->nslist->find(NULL,"xml");
	curr_fo.clear();
	curr_fo.push_back(0);
	max_fo.clear();
	sc_parent->cl_hint=1;
	XML_Parser p = XML_ParserCreateNS(NULL,SEPARATOR);
    if (! p) throw USER_ENV_EXCEPTION("file_utils.cpp,549,Couldn't allocate memory for parser\n",true);
	XML_SetReturnNSTriplet(p,1);
	int done;
    int len;
	is_ns=0;
	XML_SetElementHandler(p, sc_start,sc_end);
	XML_SetStartNamespaceDeclHandler(p,sc_ns);
	XML_SetCommentHandler(p, sc_comment);
	XML_SetProcessingInstructionHandler(p, sc_pi);
	XML_SetCharacterDataHandler(p, sc_data);
	//XML_SetCdataSectionHandler(p,dt_cdata_start,dt_cdata_end);
	cdata_mode=false;
	len = fread(Buff, 1, BUFFSIZE, f);
    if (ferror(f))  
	{
		XML_ParserFree(p);
		throw USER_ENV_EXCEPTION("file_utils.cpp,560,Read error",true);
	}
	done = feof(f);
    while (!done)
	{
		if (XML_Parse(p, Buff, len, done) != XML_STATUS_ERROR)
		{
			len = fread(Buff, 1, BUFFSIZE, f);
			if (ferror(f)) 
			{
				XML_ParserFree(p);
				throw USER_ENV_EXCEPTION("file_utils.cpp,567,Read error",true);
			}
			done = feof(f);
		}
		else
		{
			char tmp[256]; 
			sprintf(tmp, "line %d:\n%s\n",
				XML_GetCurrentLineNumber(p),
				XML_ErrorString(XML_GetErrorCode(p)));
			XML_ParserFree(p);
			throw USER_EXCEPTION2(SE2005, tmp);
			
		}
	}
	if (XML_Parse(p, Buff, len, done)== XML_STATUS_ERROR)
	{
		char tmp[256]; 
		sprintf(tmp, "line %d:\n%s\n",
			XML_GetCurrentLineNumber(p),
			XML_ErrorString(XML_GetErrorCode(p)));
		XML_ParserFree(p);
		throw USER_EXCEPTION2(SE2005, tmp);
	}
    /*it_map= xm_nsp.begin();
	while (it_map!=xm_nsp.end())
	{
		xml_ns* xns=it_map->second;
		crm_out<<"\n Namespace uri=";
		if (xns->uri!=NULL)
			crm_out<<xns->uri;
		crm_out<<" prefix=";
		if (xns->prefix!=NULL)
			crm_out<<xns->prefix;
		it_map++;
	}*/
	
	std::map<schema_node*,stat_pair>::iterator it= max_fo.find(sc_parent);
	if (it==max_fo.end())
		max_fo[sc_parent]=stat_pair(curr_fo.back(),0);
	else
		if (it->second.first<curr_fo.back()) it->second.first=curr_fo.back();
	curr_fo.pop_back(); 
	it= max_fo.begin();
	while (it!=max_fo.end())
	{
		int cnt=s_max(it->second.first,1);
		it->second.first=(int)ceil(s_max (log10(1.*cnt)/log10((double)MAX_LETTER),1.));
		it->second.second=s_min(DEF_LETTER,((int)pow((double)MAX_LETTER,it->second.first))/(2+cnt));
		it++;
	}
	curp.push_back(&max_fo[sc_parent]);
	sizehnt=curp.back();
	//printMFO (sc_parent,max_fo,0,0);
	XML_ParserFree(p);

}

xptr loadfile(FILE* f, se_ostream &s, const char* uri,bool stripped,int& need_cp, bool print_progress)
{
	//test_cnt=0;
	bool is_coll=false;
	if (!print_progress) print_p = print_progress;
	need_cp=vmm_data_blocks_allocated();
	wpstrip=stripped;
	nid_set_proportion(fnumber());
	xptr docnode=insert_document(uri);
	parent=docnode;
	left=XNULL;
	mark=1;
	sc_parent=(GETBLOCKBYNODE(docnode))->snode;
	try{
	parse_schema(f);
	fseek(f,0,SEEK_SET);
		//isSchemaPCAllRight(sc_parent);
		//print_descriptive_schema(uri,  crm_out);
	docnode=((n_dsc*)XADDR(docnode))->indir;
	parse_load(f, s);
		
		//printDebugInfo(sc_parent, crm_out);
		//print_descriptive_schema(uri,  crm_out);
	if(print_p)
		s <<"100%" << endl;
	//printDebugInfo(sc_parent, crm_out);
	nodescnt=0;
	curcnt=0;
	curproc=0;
	CHECKP(docnode);
	remove_hints(sc_parent);
	//test
	//CHECKP(xptr(0,(void*)0x3d800000));
	if (vmm_data_blocks_allocated()-need_cp>100)
		need_cp=1;
	else
		need_cp=0;
	}
	catch (SednaUserException &e)
	{
		delete_document(uri);
		throw;
	}
	if (!print_progress) print_p=true;

	return ((n_dsc*)XADDR(docnode))->indir;
}

xptr loadfile(FILE* f, se_ostream &s, const char* uri,const char * collection, bool stripped,int& need_cp, bool print_progress)
{
	bool is_coll=true;
	if (!print_progress) print_p = print_progress;
	need_cp=vmm_data_blocks_allocated();
	wpstrip=stripped;
	nid_set_proportion(fnumber());
	//schema_node* col_s=find_collection(collection);
	//if (col_s==NULL) col_s=insert_collection(collection);
	xptr docnode=insert_document_in_collection(collection,uri);
	parent=docnode;
	left=XNULL;
	mark=1;
	CHECKP(docnode);
	sc_parent=(GETBLOCKBYNODE(docnode))->snode;
#ifdef SE_ENABLE_FTSEARCH
clear_ft_sequences();
update_insert_sequence(docnode,sc_parent->ft_index_object); 
#endif
	try{
	parse_schema(f);
	fseek(f,0,SEEK_SET);
		//	printDebugInfo(sc_parent, crm_out);
		//isSchemaPCAllRight(sc_parent);
	docnode=((n_dsc*)XADDR(docnode))->indir;
	parse_load(f, s);
	if (print_p)
	  s <<"100%" << endl;
	//printDebugInfo(sc_parent, crm_out);
	nodescnt=0;
	curcnt=0;
	curproc=0;
	CHECKP(docnode);
	remove_hints(sc_parent);
	if (vmm_data_blocks_allocated()-need_cp>1000)
		need_cp=1;
	else
		need_cp=0;
	}
	catch (SednaUserException &e)
	{
		delete_document(collection,uri);
		throw;
	}
	if (!print_progress) print_p=true;

#ifdef SE_ENABLE_FTSEARCH
	execute_modifications();
#endif
	return ((n_dsc*)XADDR(docnode))->indir;
}


void basicTest()
{
/*	
	FILE* fl= fopen("c:\\test.xml","r");
	xptr docnode=loadfile(fl,"Testdoc");
	crm_out<<"OK DONE"<<endl;
	xptr docnode=insert_document("Testdoc");
	CHECKP(docnode);
	schema_node* snode= (GETBLOCKBYNODE(docnode))->snode;*/
//	printDebugInfo(snode, crm_out);
	/*	print_schema(snode, crm_out);
	xptr root=insert_element(XNULL, XNULL, docnode,"A", ANYTYPE);
	print_schema(snode, crm_out);
	xptr bnode=insert_element(XNULL, XNULL, root,"B", ANYTYPE);
	//print_schema(snode, crm_out);
	xptr cnode=insert_element(bnode, XNULL, root,"B", ANYTYPE);*/
	//print_node_indent(docnode,crm_out);
	//CHECKP(docnode);
	//print_node_with_prefixes(docnode, crm_out,0);
	crm_out<<"OK DONE"<<endl;
	//print_schema(snode, crm_out);
	/*
    xptr child=giveFirstByOrderChild(docnode,1);
	crm_out<<"DELETING NODE:"<<endl;
	nid_print(child,crm_out);
	child=giveFirstByOrderChild(child,1);
	crm_out<<"DELETING NODE:"<<endl;
	nid_print(child,crm_out);
	child=GETRIGHTPOINTER(child);
	crm_out<<"DELETING NODE:"<<endl;
	nid_print(child,crm_out);
	delete_node(child);
	crm_out<<"THE END"<<endl;
	CHECKP(docnode);
	print_node_with_prefixes(docnode, crm_out,0);*/
	//print_node(docnode,crm_out);
}