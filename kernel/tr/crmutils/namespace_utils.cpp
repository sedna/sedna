#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPBase.h"
//using namespace tr_globals;
#include <map>
#include <vector>
#include <set>
//using namespace std;
typedef std::map<std::string,xml_ns*> nms_map;
void get_in_scope_namespaces_local(xptr node,std::vector<xml_ns*> &result,dynamic_context *cxt)
{	
	nms_map mp;
	CHECKP(node);
	schema_node* scm=GETSCHEMENODEX(node);
	nms_map::iterator it;
	while ( scm->type!=virtual_root)
	{
		xptr ns=getChildPointerXptr(node,NULL,xml_namespace,NULL);
		
		int reps=0;
		while (ns!=XNULL)
		{
			CHECKP(ns);
			xml_ns* nsp=((ns_dsc*)XADDR(ns))->ns;
			const char* pref=(nsp->prefix==NULL)?"":nsp->prefix;
			if ((it=mp.find(pref))==mp.end())
			{
				mp[pref]=nsp;
			}			
			ns=getNextSiblingOfSameSortXptr(ns);
		}
		//2. self ns
		if (scm->xmlns!=NULL)
		{
			const char* pref=(scm->xmlns->prefix==NULL)?"":scm->xmlns->prefix;
			if ((it=mp.find(pref))==mp.end())
			{
				mp[pref]=scm->xmlns;
			}
		}
		//3. attributes
		xptr attr=getFirstByOrderAttributeChild(node);
		if (attr!=XNULL)
			CHECKP(attr);
		//3.1 filling set
		std::set<xml_ns*> atns;
		while (attr!=XNULL)
		{		
			GETSCHEMENODEX(attr);
			schema_node* sca=GETSCHEMENODEX(attr);
			if (sca->xmlns!=NULL)
				atns.insert(sca->xmlns);
			attr=getNextByOrderAttribute(attr);
		}
		//3.2 copying to map
		int ctr=0;
		std::set<xml_ns*>::iterator sit=atns.begin();
		while(sit!=atns.end())
		{
			const char* pref=((*sit)->prefix==NULL)?"":(*sit)->prefix;
			if ((it=mp.find(pref))!=mp.end())
			{
				if (it->second!=*sit)
				{
					xml_ns* new_ns=generate_pref(ctr++,(*sit)->uri,cxt);
					mp[new_ns->prefix]=new_ns;
				}
			}
			else
			{
				mp[pref]=(*sit);
			}
			++sit;
		}
		CHECKP(node);
		if ((GETPARENTPOINTER(node))==XNULL)
			break;
		node=removeIndirection(GETPARENTPOINTER(node));
		CHECKP(node);
		schema_node* scm=GETSCHEMENODEX(node);
	}
	//1. ns childs
	
	it=mp.begin();
	while (it!=mp.end())
	{
		result.push_back(it->second);
		++it;
	}
	result.push_back(xml_ns::init("http://www.w3.org/XML/1998/namespace","xml",false));
}
void get_in_scope_namespaces_broad(xptr node,std::vector<xml_ns*> &result)
{
}
void get_namespaces_for_inherit(xptr node,std::vector<xml_ns*> &result)
{
}
xml_ns* generate_pref(int ctr,const char* uri,dynamic_context *cxt)
{
	char x[12] = "XXX";
	if (ctr!=0)
		sprintf(x+3, "%d",ctr );
	return cxt->st_cxt->get_ns_pair(x,uri);
}