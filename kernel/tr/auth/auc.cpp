/*
 * File:  auc.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/auth/auc.h"
#include "common/errdbg/d_printf.h"
#include "tr/executor/por2qep/por2qep.h"
#include "tr/locks/locks.h"
#include "tr/tr_globals.h"
#include "tr/structures/metadata.h"
#include "common/u/uprocess.h"


using namespace std;

static auth_map amap;
static bool security_metadata_updating ;


bool operator < (counted_ptr<db_entity> de1, counted_ptr<db_entity> de2)
{
	if (de1->type < de2->type) return true;
	if (de1->type > de2->type) return false;
	if (strcmp(de1->name, de2->name) < 0) return true;
	else return false;
}

void getSednaAuthMetadataPath(char* path)
{
#ifdef AUTH_SWITCH
    char path_buf[U_MAX_PATH + 32];
#ifdef _WIN32
	string pstring = uGetImageProcPath(path_buf, __sys_call_error) + string("/../share/") + string(INITIAL_SECURITY_METADATA_FILE_NAME);
	strcpy(path, pstring.c_str());
	int i=0;
	while(i<=pstring.length())
	{
		if (path[i] == '\\') path[i] = '/';
		i++;
	}
#else
	string sedna_auth_metadata_file_path = uGetImageProcPath(path_buf, __sys_call_error) + string("/../share/") + string(INITIAL_SECURITY_METADATA_FILE_NAME);
	if(uIsFileExist(sedna_auth_metadata_file_path.c_str(), __sys_call_error))
		strcpy(path, sedna_auth_metadata_file_path.c_str());
	else
	{
		sedna_auth_metadata_file_path = string("/usr/share/sedna-") + SEDNA_VERSION + "." + SEDNA_BUILD +string("/sedna_auth_md.xml");
		strcpy(path, sedna_auth_metadata_file_path.c_str());
	}
#endif
#endif
}
void auth_for_query(counted_ptr<db_entity> dbe)
{
#ifdef AUTH_SWITCH
    bool is_qep_opened = false, is_qep_built = false;
	typedef pair <counted_ptr<db_entity>, struct dbe_properties> authPair;
	auth_map::iterator mapIter;
	qep_subtree *aqtree = NULL;
	if ( auth == BLOCK_AUTH_CHECK ) return;
	
	mapIter = amap.find(dbe);
	if ( mapIter != amap.end() )            // there is the dbe in authmap -> no need to query metadata
	{
		mapIter -> second.current_statement = true; //mark the dbe that it is refered in this statement
	}
	else
	{
		try{
    		//query metadata for new dbe
			string type_obj;
			(dbe -> type == dbe_document) ? (type_obj = "document") : (type_obj = "collection");
            string security_metadata_document = string(SECURITY_METADATA_DOCUMENT);	

            string authorization_query_in_por = "(1 (PPLet (0) (1 (PPDDO (1 (PPReturn (1) (1 (PPAbsPath (document \"" + security_metadata_document +"\") (((PPAxisChild qname (\"\" \"" + security_metadata_document +"\" \"\"))) ((PPAxisChild qname (\"\" \"users\" \"\"))) ((PPAxisChild qname (\"\" \"user\" \"\")))))) (1 (PPIf (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"user_name\" \"\") (1 (PPVariable 1)))))) (1 (PPConst  \""+string(login)+"\" !xs!string)))) (1 (PPVariable 1)) (1 (PPNil)))) -1)))) (1 (PPLet (2) (1 (PPDDO (1 (PPReturn (3) (1 (PPAbsPath (document \"" + security_metadata_document + "\") (((PPAxisChild qname (\"\" \"" + security_metadata_document + "\" \"\"))) ((PPAxisChild qname (\"\" \"roles\" \"\"))) ((PPAxisChild qname (\"\" \"role\" \"\")))))) (1 (PPIf (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"role_name\" \"\") (1 (PPVariable 3)))))) (1 (PPDDO (1 (PPAxisAttribute qname (\"\" \"role_name\" \"\") (1 (PPDDO (1 (PPAxisChild qname (\"\" \"role\" \"\") (1 (PPVariable 0)))))))))))) (1 (PPVariable 3)) (1 (PPNil)))) -1)))) (1 (PPLet (4) (1 (PPConst  \""+string(dbe -> name)+"\" !xs!string)) (1 (PPLet (5) (1 (PPConst \""+ type_obj + "\" !xs!string)) (1 (PPIf (1 (PPDDO (1 (PPReturn (18) (1 (PPAxisChild qname (\"\" \"role\" \"\") (1 (PPVariable 0)))) (1 (PPIf (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisAttribute qname (\"\" \"role_name\" \"\") (1 (PPVariable 18)))))) (1 (PPConst  \"DBA\" !xs!string)))) (1 (PPVariable 18)) (1 (PPNil)))) -1)))) (1 (PPConst 15 !xs!integer))"+"(1 (PPIf (1 (PPDDO (1 (PPReturn (16) (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPDDO (1 (PPReturn (17) (1 (PPAxisChild qname (\"\" \"privilege\" \"\") (1 (PPDDO (1 (PPAxisChild qname (\"\" \"privileges\" \"\") (1 (PPSequence (1 (PPVariable 0)) (1 (PPVariable 2)))))))))) (1 (PPIf (1 (PPCalculate (BinaryOpAnd (BinaryOpOr (LeafEffectBoolOp 0) (LeafEffectBoolOp 1)) "+"(LeafEffectBoolOp 2)) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"pr_name\" \"\") (1 (PPVariable 17)))))) (1 (PPConst \"OWNER\" !xs!string)))) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"pr_name\" \"\") (1 (PPVariable 17)))))) (1 (PPConst \"ALL\" !xs!string)))) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPVariable 17)))))) (1 (PPVariable 4)))))) (1 (PPVariable 17)) (1 (PPNil)))) -1)))))) (1 (PPIf (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisAttribute qname (\"\" \"type_obj\" \"\") (1 (PPVariable 16)))))) (1 (PPVariable 5)))) (1 (PPVariable 16)) (1 (PPNil)))) -1)))) (1 (PPConst 15 !xs!integer)) (1 (PPIf (1 (PPDDO (1 (PPReturn (14) (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPDDO (1 (PPReturn (15) (1 (PPAxisChild qname (\"\" \"privilege\" \"\") (1 (PPDDO (1 (PPAxisChild qname (\"\" \"privileges\" \"\") (1 (PPSequence (1 (PPVariable 0)) (1 (PPVariable 2)))))))))) (1 (PPIf (1 (PPCalculate (BinaryOpAnd (LeafEffectBoolOp 0) (LeafEffectBoolOp 1)) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"pr_name\" \"\") (1 (PPVariable 15)))))) (1 (PPConst \"QUERY\" !xs!string)))) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPVariable 15)))))) (1 (PPVariable 4)))))) (1 (PPVariable 15)) (1 (PPNil)))) -1)))))) (1 (PPIf (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisAttribute qname (\"\" \"type_obj\" \"\") (1 (PPVariable 14)))))) (1 (PPVariable 5)))) (1 (PPVariable 14)) (1 (PPNil)))) -1)))) (1 (PPCalculate (BinaryOpAdd (BinaryOpAdd (BinaryOpAdd (LeafAtomOp 0) (LeafAtomOp 1)) (LeafAtomOp 2)) (LeafAtomOp 3)) (1 (PPIf (1 (PPDDO (1 (PPReturn (6) (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPDDO (1 (PPReturn (7) (1 (PPAxisChild qname (\"\" \"privilege\" \"\") (1 (PPDDO (1 (PPAxisChild qname (\"\" \"privileges\" \"\") (1 (PPSequence (1 (PPVariable 0)) (1 (PPVariable 2)))))))))) (1 (PPIf (1 (PPCalculate (BinaryOpAnd (LeafEffectBoolOp 0) (LeafEffectBoolOp 1)) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"pr_name\" \"\") (1 (PPVariable 7)))))) (1 (PPConst \"INSERT\" !xs!string))))"+" (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPVariable 7)))))) (1 (PPVariable 4)))))) (1 (PPVariable 7)) (1 (PPNil)))) -1)))))) (1 (PPIf (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisAttribute qname (\"\" \"type_obj\" \"\") (1 (PPVariable 6)))))) (1 (PPVariable 5)))) (1 (PPVariable 6)) (1 (PPNil)))) -1)))) (1 (PPConst 1 !xs!integer)) (1 (PPConst 0 !xs!integer)))) (1 (PPIf (1 (PPDDO (1 (PPReturn (8) (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPDDO (1 (PPReturn (9) (1 (PPAxisChild qname (\"\" \"privilege\" \"\") (1 (PPDDO (1 (PPAxisChild qname (\"\" \"privileges\" \"\") (1 (PPSequence (1 (PPVariable 0)) (1 (PPVariable 2)))))))))) "+"(1 (PPIf (1 (PPCalculate (BinaryOpAnd (LeafEffectBoolOp 0) (LeafEffectBoolOp 1)) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"pr_name\" \"\") (1 (PPVariable 9)))))) (1 (PPConst  \"DELETE\" !xs!string)))) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPVariable 9)))))) (1 (PPVariable 4)))))) (1 (PPVariable 9)) (1 (PPNil)))) -1)))))) (1 (PPIf (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisAttribute qname (\"\" \"type_obj\" \"\") (1 (PPVariable 8)))))) (1 (PPVariable 5)))) (1 (PPVariable 8)) (1 (PPNil)))) -1)))) (1 (PPConst 2 !xs!integer)) (1 (PPConst 0 !xs!integer)))) (1 (PPIf (1 (PPDDO (1 (PPReturn (10) "+"(1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPDDO (1 (PPReturn (11) "+"(1 (PPAxisChild qname (\"\" \"privilege\" \"\") (1 (PPDDO (1 (PPAxisChild qname (\"\" \"privileges\" \"\") (1 (PPSequence (1 (PPVariable 0)) (1 (PPVariable 2)))))))))) (1 (PPIf (1 (PPCalculate (BinaryOpAnd (LeafEffectBoolOp 0) (LeafEffectBoolOp 1)) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"pr_name\" \"\") (1 (PPVariable 11)))))) (1 (PPConst \"RENAME\" !xs!string)))) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPVariable 11)))))) (1 (PPVariable 4)))))) (1 (PPVariable 11)) (1 (PPNil)))) -1)))))) (1 (PPIf (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisAttribute qname (\"\" \"type_obj\" \"\") (1 (PPVariable 10)))))) (1 (PPVariable 5)))) (1 (PPVariable 10)) (1 (PPNil)))) -1)))) (1 (PPConst 4 !xs!integer)) (1 (PPConst 0 !xs!integer)))) (1 (PPIf (1 (PPDDO (1 (PPReturn (12) (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPDDO (1 (PPReturn (13) (1 (PPAxisChild qname (\"\" \"privilege\" \"\") (1 (PPDDO (1 (PPAxisChild qname (\"\" \"privileges\" \"\") (1 (PPSequence (1 (PPVariable 0)) (1 (PPVariable 2)))))))))) (1 (PPIf (1 (PPCalculate (BinaryOpAnd (LeafEffectBoolOp 0) (LeafEffectBoolOp 1)) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"pr_name\" \"\") (1 (PPVariable 13)))))) (1 (PPConst \"REPLACE\" !xs!string)))) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"database_obj\" \"\") (1 (PPVariable 13)))))) (1 (PPVariable 4)))))) (1 (PPVariable 13)) (1 (PPNil)))) -1)))))) (1 (PPIf (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisAttribute qname (\"\" \"type_obj\" \"\") (1 (PPVariable 12)))))) (1 (PPVariable 5)))) (1 (PPVariable 12)) (1 (PPNil)))) -1)))) (1 (PPConst 8 !xs!integer)) (1 (PPConst 0 !xs!integer)))))) (1 (PPFnError (1 (PPFnQName (1 (PPConst \"http://www.modis.ispras.ru/sedna\" !xs!string)) (1 (PPConst \"SE3065\" !xs!string)))) (1 (PPConst \"User does not have QUERY privilege on database object\" !xs!string))))))))))))))))))";
            //dynamic_context::__static_cxt()->add_to_context(NULL, "http://www.w3.org/XML/1998/namespace");
			auth = BLOCK_AUTH_CHECK;
            			
			aqtree = build_qep(authorization_query_in_por.c_str(), 19);
            is_qep_built = true; 
			aqtree->tree.op->open();
            is_qep_opened = true;
            tuple t = tuple(1);
			aqtree->tree.op->next(t);
            //dynamic_context::__static_cxt()->remove_from_context((const char*)NULL);
			if ( !t.cells[0].is_light_atomic() ) throw USER_EXCEPTION2(SE1003, "Failed while authorization checking");
			if ( t.cells[0].get_atomic_type() != xs_integer ) throw USER_EXCEPTION2(SE1003, "Failed while authorization checking"); 
			int update_privileges = t.cells[0].get_xs_integer();
			//d_printf2("Update_privileges = %d\n", update_privileges);
			aqtree->tree.op->next(t);
			if(!t.is_eos()) throw USER_EXCEPTION2(SE1003, "Failed while authorization checking"); 
			aqtree->tree.op->close();
            is_qep_opened = false;
			delete_qep(aqtree);
            is_qep_built = false;
			
			auth = 1;
// 		    release_resource(SECURITY_METADATA_DOCUMENT, LM_DOCUMENT);


			//put the new dbe into the authmap
			//d_printf1("put the new dbe into the authmap\n");
    	    dbe_properties dbe_p;          
        	dbe_p.update_privileges = update_privileges;
	        dbe_p.current_statement = true;
	        dbe_p.was_updated = false;
    	    
        	pair< auth_map::iterator, bool > pr;
    	    pr = amap.insert( authPair( dbe, dbe_p ) );
		}
		catch(SednaUserException &e){
            if(is_qep_opened)
                aqtree->tree.op->close();
            if(is_qep_built)
                delete_qep(aqtree);
            
			throw;
		}	
    }
#endif    
}

void auth_for_load_module(const char* module_name)
{
#ifdef AUTH_SWITCH
    PPQueryEssence* qep_tree = NULL;
    se_nullostream null_os;
    bool is_qep_opened = false, is_qep_built = false;
    
    string update_for_load_module_in_por = "(query (query-prolog) (PPInsertTo 0 (1 (PPElement (\"\" \"privilege\") (1 (PPSequence (1 (PPElement (\"\" \"pr_name\") (1 (PPConst \"OWNER\" !xs!string)) #f #f)) (1 (PPElement (\"\" \"database_obj\") (1 (PPSequence (1 (PPAttribute (\"\" \"type_obj\") (1 (PPConst \"module\" !xs!string)) #f)) (1 (PPConst \"" + string(module_name) +"\" !xs!string)))) #f #f)) (1 (PPElement (\"\" \"grantor\") (1 (PPConst \"" + string(login) +"\" !xs!string)) #f #f)))) #t #f)) 3 (1 (PPAxisChild qname (\"\" \"privileges\" \"\") (1 (PPReturn (0) (1 (PPAbsPath (document \"db_security_data\") (((PPAxisChild qname (\"\" \"db_security_data\" \"\"))) ((PPAxisChild qname (\"\" \"users\" \"\")))))) (1 (PPPred1 (1) (1 (PPAxisChild qname (\"\" \"user\" \"\") (1 (PPVariable 0)))) () (1 (PPGeneralCompEQ (1 (PPAxisChild qname (\"\" \"user_name\" \"\") (1 (PPVariable 1)))) (1 (PPConst \""+ string(login) +"\" !xs!string)))) 0)) -1))))))";
    try
    {
        auth = BLOCK_AUTH_CHECK;			
        qep_tree = build_qep(update_for_load_module_in_por.c_str(), null_os, xml); 
        is_qep_built = true;
        qep_tree->open();
        is_qep_opened = true;
        qep_tree->execute();
        qep_tree->close();
        is_qep_opened = false;
        delete_qep(qep_tree);
        is_qep_built = false;
        auth = 1;
    }
    catch(SednaUserException &e) {
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete_qep(qep_tree);
        throw e;}
#endif    
}

void clear_current_statement_authmap()
{
#ifdef AUTH_SWITCH
	auth_map::iterator Iter;
    Iter = amap.begin();
	while ( Iter != amap.end() )
	{
		Iter -> second.current_statement = false;
		Iter++;
	}
#endif 
}

//erases all the elements of a auth_map
void clear_authmap()
{
#ifdef AUTH_SWITCH
	amap.clear();
#endif
}

void security_metadata_upd_controll()
{
#ifdef AUTH_SWITCH
	if( security_metadata_updating )
	{
		clear_authmap();
		security_metadata_updating = false;
	}
#endif
}

bool is_auth_check_needed(int update_privilege)
{
#ifdef AUTH_SWITCH
	if(amap.empty()) return false;
    
	bool all_true = true;
	bool all_false = true;
	bool empty_for_current_statement = true;
	security_metadata_updating = false;

	auth_map::iterator mIter;

    mIter = amap.begin();
	while ( mIter != amap.end() )
	{
		if( mIter -> second.current_statement )
		{
			// pometa dlia petky
			mIter -> second.was_updated = true;
			// pometa dlia petky
		    empty_for_current_statement = false;
		    if( strcmp( mIter -> first -> name, SECURITY_METADATA_DOCUMENT ) == 0 ) security_metadata_updating = true;
			
			if( mIter -> second.update_privileges & update_privilege )  all_false = false; //there is needed privilege
			else all_true = false;                               //no needed privilege
			if(!(all_true || all_false)) 
            {
              return true;
            }       // need to check every node 
		}
		mIter++;
	}
	
	if( empty_for_current_statement || all_true ) {
	                 security_metadata_upd_controll();
	                 return false; 
	                 } //no need to check every node;
	if( all_false ) {
            		security_metadata_upd_controll();
	                throw USER_EXCEPTION2(SE3065, "Failed while authorization checking"); 
	                }
	return true;
#else
	return false;
#endif 
}


void auth_for_update(xptr_sequence* seq, int update_privilege, bool direct)
{
#ifdef AUTH_SWITCH
	auth_map::iterator mIter;
	xptr_sequence::iterator it=(*seq).begin();
	xptr node;
	
	try
	{
		while (it!=(*seq).end())
		{
			if(!direct)
			{
				node= removeIndirection(*it);
			}
			else
			{
				node = *it;
			}
			
			CHECKP(node);			
			schema_node* sn = ((node_blk_hdr*)GETBLOCKBYNODE(node)) -> snode -> root;
			schema_node* dbe_root = NULL;
			
			mIter = amap.begin();
			while( mIter != amap.end() )
			{
				if( mIter -> second.current_statement )
				{
					switch (mIter -> first ->type)
					{
						case dbe_document   : dbe_root = find_document(mIter -> first -> name);
										    	if( sn == dbe_root )
												if( !(mIter -> second.update_privileges & update_privilege) )	throw  USER_EXCEPTION(SE3065); //Failed to process authorization
												else
												{
													// pometa dlia petky
													mIter -> second.was_updated = true;
													// pometa dlia petky
													break;
												}
						case dbe_collection	: dbe_root = find_collection(mIter -> first -> name);
										    	if( sn == dbe_root )
												if( !(mIter -> second.update_privileges & update_privilege) )	throw  USER_EXCEPTION(SE3065); //Failed to process authorization
												else
												{
													// pometa dlia petky
													mIter -> second.was_updated = true;
													// pometa dlia petky
													break;
												}
					}
				}
				mIter++;
			}
			if( mIter == amap.end() ) //no doc was found in amap for the node!
			{
				throw  USER_EXCEPTION2(SE3066, "Auth map does not contain a document for a node subject to update");
			}
			it++;
		}
	}//end of try
	catch(SednaUserException &e)
	{
		security_metadata_upd_controll();
		throw;
	}
	
	security_metadata_upd_controll();
	return;
#endif   
}
