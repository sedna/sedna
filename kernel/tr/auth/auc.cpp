/*
* File:  auc.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <string>

#include "common/sedna.h"
#include "common/u/uprocess.h"
#include "common/errdbg/d_printf.h"

#include "tr/auth/auc.h"
#include "tr/xqp/XQuerytoLR.h"
#include "tr/locks/locks.h"
#include "tr/tr_globals.h"
#include "tr/structures/metadata.h"
#include "tr/crmutils/exec_output.h"

using namespace std;

typedef map<counted_ptr<db_entity>, struct dbe_properties> auth_map;

static auth_map amap;
static bool security_metadata_updating;

static bool
operator < (counted_ptr<db_entity> de1, counted_ptr<db_entity> de2)
{
    if (de1->type < de2->type) return true;
    if (de1->type > de2->type) return false;
    if (strcmp(de1->name, de2->name) < 0) return true;
    else return false;
}

static void find_replace_str(std::string *str, const char *find, const char *repl)
{
    size_t pos = 0;
    size_t end_pos = str->size() - 1;
    size_t find_len = strlen(find), repl_len = strlen(repl);

    if (!str->size()) return;

    while (pos <= end_pos)
    {
        pos = str->find(find, pos);

        if (pos == std::string::npos) break;

        if (pos <= end_pos)
        {
            str->replace(pos, find_len, repl);
            pos += repl_len;
            end_pos += (repl_len - find_len);
        }
    }
}

void getSednaAuthMetadataPath(char* path)
{
    char path_buf[U_MAX_PATH + 32];
#ifdef _WIN32
    string pstring = uGetImageProcPath(path_buf, __sys_call_error) + string("/../share/") + string(INITIAL_SECURITY_METADATA_FILE_NAME);
    strcpy(path, pstring.c_str());

    size_t i=0;
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
}

void auth_for_query(counted_ptr<db_entity> dbe)
{
    if (!tr_globals::authorization) return; //if authorization if off

    bool is_qep_opened = false, is_qep_built = false;
    typedef pair <counted_ptr<db_entity>, struct dbe_properties> authPair;
    auth_map::iterator mapIter;
    PPSubQuery *aqtree = NULL;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    mapIter = amap.find(dbe);
    if (mapIter != amap.end())            // there is the dbe in authmap -> no need to query metadata
    {
        mapIter -> second.current_statement = true; //mark the dbe that it is refered in this statement
    }
    else
    {
        try
        {
            //query metadata for new dbe
            string type_obj;
            switch(dbe->type)
            {
                case dbe_document: type_obj = "document"; break;
                case dbe_collection: type_obj = "collection"; break;
                case dbe_module: type_obj = "module"; break;
            }
            string security_metadata_document = string(SECURITY_METADATA_DOCUMENT);

            // get maximum available priveleges
            std::string auth_xquery = "\
                declare ordering unordered; \
                \
                let $security := doc('%db_sec_doc%')/db_security_data,\
                    $u := $security/users/user[user_name='%user%'],\
                    $r := $security/roles/role[role_name=$u/role/@role_name],\
                    $pr := ($u, $r)/privileges/privilege[database_obj/@type_obj='%type_obj%' and database_obj='%name_obj%'],\
                    $pr_query := $pr[pr_name=('QUERY','OWNER','ALL')],\
                    $pr_all := $pr_query[pr_name=('ALL', 'OWNER')],\
                    $pr_db := ($u, $r)/privileges/privilege[(pr_name = 'ALL' or pr_name = 'QUERY') and empty(database_obj)],\
                    $pr_all_db := $pr_db[pr_name = 'ALL'],\
                    $pr_update := $pr[pr_name=('INSERT','REPLACE', 'DELETE', 'RENAME')]\
                return\
                    if ($u/role/@role_name='DBA' or not(empty($pr_query)) or not(empty($pr_db)))\
                    then \
                        if (not(empty($pr_all)) or not(empty($pr_all_db)) or $u/role/@role_name='DBA')\
                        then \
                            15 \
                        else\
                            sum(for $i in $pr_update/pr_name\
                                return\
                                    if      ($i = 'INSERT')  then 1\
                                    else if ($i = 'REPLACE') then 8\
                                    else if ($i = 'DELETE')  then 2\
                                    else if ($i = 'RENAME')  then 4\
                                    else 0)\
                    else \
                        error(QName('http://www.modis.ispras.ru/sedna','SE3065'), 'User does not have QUERY privilege on the database object')";

            // substitute dynamic parameters
            find_replace_str(&auth_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
            find_replace_str(&auth_xquery, "%user%", tr_globals::login);
            find_replace_str(&auth_xquery, "%type_obj%", (dbe->type == dbe_document) ? "document" : "collection");
            find_replace_str(&auth_xquery, "%name_obj%", dbe->name);

            tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;

            aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(auth_xquery.c_str(), TL_XQuery));
            is_qep_built = true;

            aqtree->open();
            is_qep_opened = true;

            tuple t = tuple(1);
            aqtree->next(t);

            if (!t.cells[0].is_light_atomic())
                throw USER_EXCEPTION2(SE1003, "Failed while authorization checking");

            if (t.cells[0].get_atomic_type() != xs_integer)
                throw USER_EXCEPTION2(SE1003, "Failed while authorization checking");

            int update_privileges = t.cells[0].get_xs_integer();

            aqtree->next(t);
            if(!t.is_eos())
                throw USER_EXCEPTION2(SE1003, "Failed while authorization checking");

            aqtree->close();
            is_qep_opened = false;

            delete aqtree;
            is_qep_built = false;

            tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

            //put the new dbe into the authmap
            dbe_properties dbe_p;
            dbe_p.update_privileges = update_privileges;
            dbe_p.current_statement = true;

            pair<auth_map::iterator, bool> pr;
            pr = amap.insert(authPair(dbe, dbe_p ));
        }
        catch(SednaUserException)
        {
            if(is_qep_opened)
                aqtree->close();
            if(is_qep_built)
                delete aqtree;

            tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

            throw;
        }
    }
}

void auth_for_load_module(const char* module_name)
{
    if (!tr_globals::authorization) return; //if authorization if off
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;


    std::string rc_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr_db := ($u, $r)/privileges/privilege[(pr_name = 'LOAD-MODULE' or pr_name = 'ALL') and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3072'), 'User does not have LOAD-MODULE privilege on database')";


    std::string update_load_module_xquery =
        "declare ordering unordered;\
         \
         update insert \
             if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/privileges/privilege[database_obj = '%name_obj%']) \
             then \
                 () \
             else \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='module'>{'%name_obj%'}</database_obj>\
                    <grantor>{'%user%'}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/privileges";

    // substitute dynamic parameters
    find_replace_str(&update_load_module_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&update_load_module_xquery, "%user%", tr_globals::login);
    find_replace_str(&update_load_module_xquery, "%name_obj%", module_name);

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        find_replace_str(&rc_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&rc_xquery, "%user%", tr_globals::login);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(rc_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update db_security_data for the new module name
        qep_tree = build_subquery_qep(update_load_module_xquery.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_drop_module(const char* mod_name)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPSubQuery *aqtree        = NULL;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string md_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr := ($u, $r)/privileges/privilege[database_obj/@type_obj='module' and database_obj='%name_obj%'],\
            $pr_ren := $pr[pr_name=('DROP','OWNER','ALL')],\
            $pr_db := ($u, $r)/privileges/privilege[pr_name = 'ALL' and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_ren)) or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3065'), 'User does not have DROP privilege on the module')";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has a privilege to rename this collection (privilege DROP is needed)
        find_replace_str(&md_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&md_xquery, "%user%", tr_globals::login);
        find_replace_str(&md_xquery, "%name_obj%", mod_name);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(md_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_rename_collection(const char* old_name, const char* new_name)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string rc_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr := ($u, $r)/privileges/privilege[database_obj/@type_obj='collection' and database_obj='%old_name_obj%'],\
            $pr_ren := $pr[pr_name=('DROP','OWNER','ALL')],\
            $pr_db := ($u, $r)/privileges/privilege[pr_name = 'ALL' and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_ren)) or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3065'), 'User does not have DROP privilege on the collection')";


    std::string update_rc_xquery =
        "UPDATE REPLACE $col_name in\
             doc('%db_sec_doc%')//privilege/database_obj[@type_obj='collection' and text()='%old_name_obj%']\
         with\
             <database_obj type_obj='collection'>%new_name_obj%</database_obj>";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has a privilege to rename this collection (privilege DROP is needed)
        find_replace_str(&rc_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&rc_xquery, "%user%", tr_globals::login);
        find_replace_str(&rc_xquery, "%old_name_obj%", old_name);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(rc_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update db_security_data for the new collection name
        // substitute dynamic parameters
        find_replace_str(&update_rc_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&update_rc_xquery, "%old_name_obj%", old_name);
        find_replace_str(&update_rc_xquery, "%new_name_obj%", new_name);

        qep_tree = build_subquery_qep(update_rc_xquery.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void clear_current_statement_authmap()
{
    if (!tr_globals::authorization) return; //if authorization if off

    auth_map::iterator Iter;
    Iter = amap.begin();
    while ( Iter != amap.end() )
    {
        Iter -> second.current_statement = false;
        Iter++;
    }
}

//erases all the elements of a auth_map
void clear_authmap()
{
    if (!tr_globals::authorization) return; //if authorization if off
    amap.clear();
}

void security_metadata_upd_controll()
{
    if( security_metadata_updating )
    {
        clear_authmap();
        security_metadata_updating = false;
    }
}

bool is_auth_check_needed(int update_privilege)
{
    if (!tr_globals::authorization) return false;

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
}

void auth_for_update(xptr_sequence* seq, int update_privilege, bool direct)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    auth_map::iterator mIter;
    xptr_sequence::iterator it=(*seq).begin();
    xptr node;

    try
    {
        while (it!=(*seq).end())
        {
            node = *it;
            if (!direct) { node = indirectionDereferenceCP(node); }

            doc_schema_node_cptr sn = getSchemaNode(node)->root;
            doc_schema_node_xptr dbe_root  = XNULL;

            mIter = amap.begin();
            while( mIter != amap.end() )
            {
                if( mIter -> second.current_statement )
                {
                    switch ( mIter->first->type )
                    {
                    case dbe_document :
                        dbe_root = find_document   (mIter->first->name);
                        break;
                    case dbe_collection :
                        dbe_root = find_collection (mIter->first->name);
                        break;
                    default:
                        throw USER_EXCEPTION2(SE1003, "Impossible database entity type in the authorization map");
                    }

                    if( sn == dbe_root )
                    {
                        if (mIter -> second.update_privileges & update_privilege)
                            /* User has been authorized to update this node */
                            break;
                        else
                        {
                            /* Failed to process authorization */
                            throw USER_EXCEPTION2(SE3065, ((mIter->first->type == dbe_document ?
                                                                   string("document '") :
                                                                   string("collection '")) +
                                                            mIter->first->name + "'").c_str());
                        }
                    }
                }
                mIter++;
            }
            if( mIter == amap.end() )
            {
                /* No doc was found in amap for the node! */
                throw USER_EXCEPTION2(SE3066, "Authorization map does not contain a document or collection for a given node");
            }
            it++;
        }
    }
    catch(SednaUserException)
    {
        security_metadata_upd_controll();
        throw;
    }

    security_metadata_upd_controll();
}

void auth_for_create_document(const char* doc_name)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string rc_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr_db := ($u, $r)/privileges/privilege[(pr_name = 'CREATE-DOCUMENT' or pr_name = 'ALL') and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3072'), 'User does not have CREATE-DOCUMENT privilege')";


    std::string update_xquery =
        "update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='document'>{'%name_obj%'}</database_obj>\
                    <grantor>{'%user%'}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        find_replace_str(&rc_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&rc_xquery, "%user%", tr_globals::login);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(rc_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update db_security_data for the new document
        // substitute dynamic parameters
        find_replace_str(&update_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&update_xquery, "%user%", tr_globals::login);
        find_replace_str(&update_xquery, "%name_obj%", doc_name);

        qep_tree = build_subquery_qep(update_xquery.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_load_document(const char* doc_name)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string rc_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr_db := ($u, $r)/privileges/privilege[(pr_name = 'LOAD' or pr_name = 'ALL') and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3072'), 'User does not have LOAD privilege on database')";


    std::string update_xquery =
        "update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='document'>{'%name_obj%'}</database_obj>\
                    <grantor>{'%user%'}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        find_replace_str(&rc_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&rc_xquery, "%user%", tr_globals::login);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(rc_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update db_security_data for the new document
        // substitute dynamic parameters
        find_replace_str(&update_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&update_xquery, "%user%", tr_globals::login);
        find_replace_str(&update_xquery, "%name_obj%", doc_name);

        qep_tree = build_subquery_qep(update_xquery.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_create_collection(const char* coll_name)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string rc_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr_db := ($u, $r)/privileges/privilege[(pr_name = 'CREATE-COLLECTION' or pr_name = 'ALL') and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3072'), 'User does not have CREATE-COLLECTION privilege')";


    std::string update_xquery =
        "update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='collection'>{'%name_obj%'}</database_obj>\
                    <grantor>{'%user%'}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        find_replace_str(&rc_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&rc_xquery, "%user%", tr_globals::login);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(rc_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update db_security_data for the new collection
        // substitute dynamic parameters
        find_replace_str(&update_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&update_xquery, "%user%", tr_globals::login);
        find_replace_str(&update_xquery, "%name_obj%", coll_name);

        qep_tree = build_subquery_qep(update_xquery.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_create_document_collection(const char* doc_name, const char *coll_name)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPSubQuery *aqtree        = NULL;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string md_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr := ($u, $r)/privileges/privilege[database_obj/@type_obj='collection' and database_obj='%name_obj%'],\
            $pr_ren := $pr[pr_name=('CREATE-DOCUMENT','OWNER','ALL')],\
            $pr_db := ($u, $r)/privileges/privilege[pr_name = 'ALL' and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_ren)) or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3065'), 'User does not have CREATE-DOCUMENT privilege on the collection')";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has a privilege to rename this collection (privilege DROP is needed)
        find_replace_str(&md_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&md_xquery, "%user%", tr_globals::login);
        find_replace_str(&md_xquery, "%name_obj%", coll_name);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(md_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_load_document_collection(const char* doc_name, const char *coll_name)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPSubQuery *aqtree        = NULL;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string md_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr := ($u, $r)/privileges/privilege[database_obj/@type_obj='collection' and database_obj='%name_obj%'],\
            $pr_ren := $pr[pr_name=('LOAD','OWNER','ALL')],\
            $pr_db := ($u, $r)/privileges/privilege[pr_name = 'ALL' and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_ren)) or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3065'), 'User does not have LOAD privilege on the collection')";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has a privilege to rename this collection (privilege DROP is needed)
        find_replace_str(&md_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&md_xquery, "%user%", tr_globals::login);
        find_replace_str(&md_xquery, "%name_obj%", coll_name);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(md_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_create_index(const char* ind_name, const char *obj_name, bool is_collection)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string rc_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr := ($u, $r)/privileges/privilege[database_obj/@type_obj='%type_obj%' and database_obj='%name_obj%'],\
            $pr_ren := $pr[pr_name=('CREATE-INDEX','OWNER','ALL')],\
            $pr_db := ($u, $r)/privileges/privilege[pr_name = 'ALL' and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_ren)) or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3065'), 'User does not have CREATE-INDEX privilege for the object')";


    std::string update_xquery =
        "update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='index'>{'%name_obj%'}</database_obj>\
                    <grantor>{'%user%'}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        find_replace_str(&rc_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&rc_xquery, "%user%", tr_globals::login);
        find_replace_str(&rc_xquery, "%type_obj%", (is_collection) ? "collection" : "document");
        find_replace_str(&rc_xquery, "%name_obj%", obj_name);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(rc_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update db_security_data for the new index
        // substitute dynamic parameters
        find_replace_str(&update_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&update_xquery, "%user%", tr_globals::login);
        find_replace_str(&update_xquery, "%name_obj%", ind_name);

        qep_tree = build_subquery_qep(update_xquery.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

#ifdef SE_ENABLE_FTSEARCH
void auth_for_create_ftindex(const char* ind_name, const char *obj_name, bool is_collection)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string rc_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr := ($u, $r)/privileges/privilege[database_obj/@type_obj='%type_obj%' and database_obj='%name_obj%'],\
            $pr_ren := $pr[pr_name=('CREATE-FT-INDEX','OWNER','ALL')],\
            $pr_db := ($u, $r)/privileges/privilege[pr_name = 'ALL' and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_ren)) or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3065'), 'User does not have CREATE-FT-INDEX privilege for the object')";


    std::string update_xquery =
        "update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='ft-index'>{'%name_obj%'}</database_obj>\
                    <grantor>{'%user%'}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        find_replace_str(&rc_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&rc_xquery, "%user%", tr_globals::login);
        find_replace_str(&rc_xquery, "%type_obj%", (is_collection) ? "collection" : "document");
        find_replace_str(&rc_xquery, "%name_obj%", obj_name);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(rc_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update db_security_data for the new index
        // substitute dynamic parameters
        find_replace_str(&update_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&update_xquery, "%user%", tr_globals::login);
        find_replace_str(&update_xquery, "%name_obj%", ind_name);

        qep_tree = build_subquery_qep(update_xquery.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}
#endif /* SE_ENABLE_FTSEARCH */

void auth_for_create_trigger(const char *trg_name)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string rc_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr_db := ($u, $r)/privileges/privilege[(pr_name = 'CREATE-TRIGGER' pr_name = 'ALL') and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3072'), 'User does not have CREATE-TRIGGER privilege')";


    std::string update_xquery =
        "update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='trigger'>{'%name_obj%'}</database_obj>\
                    <grantor>{'%user%'}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        find_replace_str(&rc_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&rc_xquery, "%user%", tr_globals::login);
        find_replace_str(&rc_xquery, "%name_obj%", trg_name);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(rc_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update db_security_data for the new trigger
        // substitute dynamic parameters
        find_replace_str(&update_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&update_xquery, "%user%", tr_globals::login);
        find_replace_str(&update_xquery, "%name_obj%", trg_name);

        qep_tree = build_subquery_qep(update_xquery.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_drop_object(const char* obj_name, const char *obj_type, bool just_check)
{
    if (!tr_globals::authorization) return;
    if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree         = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    std::string rc_xquery = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr := ($u, $r)/privileges/privilege[database_obj/@type_obj='%type_obj%' and database_obj='%name_obj%'],\
            $pr_ren := $pr[pr_name=('DROP','OWNER','ALL')],\
            $pr_db := ($u, $r)/privileges/privilege[pr_name = 'ALL' and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_ren)) or not(empty($pr_db)))\
            then \
                () \
            else \
                error(QName('http://www.modis.ispras.ru/sedna','SE3065'), 'User does not have DROP privilege for the object')";


    std::string update_xquery =
        "update delete doc('%db_sec_doc%')/db_security_data//privilege[database_obj = '%name_obj%' and database_obj/@type_obj = '%type_obj%']";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        find_replace_str(&rc_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&rc_xquery, "%user%", tr_globals::login);
        find_replace_str(&rc_xquery, "%type_obj%", obj_type);
        find_replace_str(&rc_xquery, "%name_obj%", obj_name);

        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(rc_xquery.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        if (!just_check)
        {
            // update db_security_data for the new index
            // substitute dynamic parameters
            find_replace_str(&update_xquery, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
            find_replace_str(&update_xquery, "%type_obj%", obj_type);
            find_replace_str(&update_xquery, "%name_obj%", obj_name);

            qep_tree = build_subquery_qep(update_xquery.c_str(), TL_XQuery);
            is_qep_built = true;

            qep_tree->open();
            is_qep_opened = true;

            qep_tree->execute();

            qep_tree->close();
            is_qep_opened = false;

            delete qep_tree;
            is_qep_built = false;
        }

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_create_user(const char* name, const char* passwd)
{
    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    // checks if we have privilege
    std::string q1 = "\
        declare ordering unordered; \
        \
        let $security := doc('%db_sec_doc%')/db_security_data,\
            $u := $security/users/user[user_name='%user%'],\
            $r := $security/roles/role[role_name=$u/role/@role_name],\
            $pr_db := ($u, $r)/privileges/privilege[(pr_name = 'CREATE-USER' or pr_name = 'ALL') and empty(database_obj)]\
        return\
            if ($u/role/@role_name='DBA' or not(empty($pr_db)))\
            then \
                () \
            else \
                fn:error(QName('http://www.modis.ispras.ru/sedna','SE3072'), 'User does not have CREATE-USER privilege')";

    // checks if user already exists
    std::string q2 = "\
            if (doc('%db_sec_doc%')/db_security_data/users/user[user_name='%user%'])\
            then\
                fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3079'), 'User with this name already exists')\
            else\
                ()";

    // updates security
    std::string q3 = "\
               update insert\
                   <user>\
                       <user_name>%new_user%</user_name>\
                       <user_psw>%passwd%</user_psw>\
                       <creator>%user%</creator>\
                       <privileges/>\
                       <role role_name='PUBLIC' grantor='%user%'/>\
                   </user>\
                into\
                    doc('%db_sec_doc%')/db_security_data/users";

    find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q1, "%user%", tr_globals::login);

    find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q2, "%user%", name);

    find_replace_str(&q3, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q3, "%user%", tr_globals::login);
    find_replace_str(&q3, "%new_user%", name);
    find_replace_str(&q3, "%passwd%", passwd);

    try
    {
        tuple t = tuple(1);

        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // first subquery
        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(q1.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // second subquery
        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(q2.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update query
        qep_tree = build_subquery_qep(q3.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_drop_user(const char* name)
{
    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree         = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    // checks if we have privilege
    std::string q1 = "\
            if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/role[@role_name = 'DBA'] or\
                doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%drop_user%' and creator = '%user%'])\
            then\
                fn:true()\
            else\
                fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3078'), 'Not allowed to drop this user')";

    std::string q2 = "update delete doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%drop_user%']";

    find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q1, "%user%", tr_globals::login);
    find_replace_str(&q1, "%drop_user%", name);

    find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q2, "%drop_user%", name);

    try
    {
        tuple t = tuple(1);

        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // first subquery
        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(q1.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update query
        qep_tree = build_subquery_qep(q2.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_alter_user(const char* name, const char* passwd)
{
    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    // checks if we have privilege
    std::string q1 = "\
            if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/role[@role_name = 'DBA'] or\
                doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%alter_user%' and creator = '%user%'] or\
                '%user%' = '%alter_user%')\
            then\
                fn:true()\
            else\
                fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3077'), 'Not allowed to alter this user')";

    // updates security
    std::string q2 = "update replace $n in doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%alter_user%']/user_psw with <user_psw>%new_psw%</user_psw>";


    find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q1, "%user%", tr_globals::login);
    find_replace_str(&q1, "%alter_user%", name);

    find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q2, "%alter_user%", name);
    find_replace_str(&q2, "%new_psw%", passwd);

    try
    {
        tuple t = tuple(1);

        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // first subquery
        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(q1.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update query
        qep_tree = build_subquery_qep(q2.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_create_role(const char* name)
{
    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    // checks if we have privilege
    std::string q1 = "\
            if (doc('%db_sec_doc%')/db_security_data/roles/role[role_name='%role%'] or '%role%' = 'DBA' or '%role%' = 'PUBLIC')\
            then\
                fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3080'), 'Role with this name already exists')\
            else\
                fn:true()";

    std::string q2 = "\
               update insert\
                   <role>\
                       <role_name>%role%</role_name>\
                       <creator>%user%</creator>\
                       <privileges/>\
                   </role>\
                into\
                    doc('%db_sec_doc%')/db_security_data/roles";

    find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q1, "%role%", name);

    find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q2, "%user%", tr_globals::login);
    find_replace_str(&q2, "%role%", name);

    try
    {
        tuple t = tuple(1);

        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // first subquery
        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(q1.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update query
        qep_tree = build_subquery_qep(q2.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_drop_role(const char* name)
{
    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree         = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    // checks if we have privilege
    std::string q1 = "\
            if ((doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/role[@role_name = 'DBA'] or\
                doc('%db_sec_doc%')/db_security_data/roles/role[role_name = '%role%' and creator = '%user%']) and\
                '%role%' != 'DBA' and '%role%' != 'PUBLIC')\
            then\
                fn:true()\
            else\
                fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3077'), 'Not allowed to drop this role')";

    std::string q2 = "update delete doc('%db_sec_doc%')/db_security_data//role[role_name = '%role%']";

    find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q1, "%user%", tr_globals::login);
    find_replace_str(&q1, "%role%", name);

    find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q2, "%role%", name);

    try
    {
        tuple t = tuple(1);

        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // first subquery
        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(q1.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update query
        qep_tree = build_subquery_qep(q2.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_grant_role(const char* name, const char *grantee)
{
    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    // checks if we have privilege
    std::string q1 = "\
            if (doc('%db_sec_doc%')/db_security_data/roles/role[role_name = '%role%'])\
            then\
                if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/role[@role_name = 'DBA'])\
                then\
                    fn:true()\
                else if (\
                    every $p in (doc('%db_sec_doc%')/db_security_data/roles/role[role_name = '%role%']/privileges/privilege) satisfies\
                        (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/privileges/privilege\
                            [pr_name = 'OWNER' and fn:deep-equal(database_obj, $p/database_obj)]))\
                    then\
                        fn:true()\
                    else\
                        fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3074'), 'Not allowed to grant this role')\
            else\
                fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3074'), 'Role does not exist')";

    // update for grantee-user
    std::string q2 = "update insert\
                           <role role_name='%role%' grantor='%user%'/> into\
                           doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%grantee%']";

    // update for grantee-role
    std::string q3 = "update insert\
                               for $p in (doc('%db_sec_doc%')/db_security_data/roles/role[role_name = '%role%']/privileges/privilege)\
                               return <privilege>{$p/pr_name, $p/database_obj, <grantor>%user%</grantor>}</privilege>\
                      into\
                               doc('%db_sec_doc%')/db_security_data/roles/role[role_name = '%grantee%']/privileges";

    find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q1, "%user%", tr_globals::login);
    find_replace_str(&q1, "%role%", name);

    find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q2, "%user%", tr_globals::login);
    find_replace_str(&q2, "%grantee%", grantee);
    find_replace_str(&q2, "%role%", name);

    find_replace_str(&q3, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q3, "%user%", tr_globals::login);
    find_replace_str(&q3, "%grantee%", grantee);
    find_replace_str(&q3, "%role%", name);

    try
    {
        tuple t = tuple(1);

        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // first subquery
        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(q1.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update query
        qep_tree = build_subquery_qep(q2.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        // second update query
        qep_tree = build_subquery_qep(q3.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_grant_privilege(const char* name, const char *obj_name, const char *obj_type, const char *grantee)
{
    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;
    std::string q1, q2;

    if (!obj_type)
    {
        q1 = "if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/role[@role_name = 'DBA'])\
              then\
                 fn:true()\
              else\
                 fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3073'), 'Only DBA is allowed to grant database privileges')";

        q2 =
            "update insert \
                     <privilege>\
                        <pr_name>%name%</pr_name>\
                        <grantor>%user%</grantor>\
                     </privilege>\
             into\
                 (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%grantee%']/privileges,\
                  doc('%db_sec_doc%')/db_security_data/roles/role[role_name = '%grantee%']/privileges)";

        find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&q1, "%user%", tr_globals::login);

        find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&q2, "%name%", name);
        find_replace_str(&q2, "%user%", tr_globals::login);
        find_replace_str(&q2, "%grantee%", grantee);
    }
    else
    {
        q1 =
           "if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/role[@role_name = 'DBA'])\
            then\
                fn:true()\
            else if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/privileges/privilege\
                        [pr_name = 'OWNER' and fn:deep-equal(database_obj, <database_obj type_obj='%obj_type%'>%obj_name%</database_obj>)])\
                then\
                    fn:true()\
                else\
                    fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3074'), 'Not allowed to grant this privilege')";

        q2 =
            "update insert \
                     <privilege>\
                        <pr_name>%name%</pr_name>\
                        <database_obj type_obj='%obj_type%'>%obj_name%</database_obj>\
                        <grantor>%user%</grantor>\
                     </privilege>\
             into\
                 (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%grantee%']/privileges,\
                  doc('%db_sec_doc%')/db_security_data/roles/role[role_name = '%grantee%']/privileges)";

        find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&q1, "%user%", tr_globals::login);
        find_replace_str(&q1, "%obj_type%", obj_type);
        find_replace_str(&q1, "%obj_name%", obj_name);

        find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&q2, "%name%", name);
        find_replace_str(&q2, "%user%", tr_globals::login);
        find_replace_str(&q2, "%grantee%", grantee);
        find_replace_str(&q2, "%obj_type%", obj_type);
        find_replace_str(&q2, "%obj_name%", obj_name);
    }

    try
    {
        tuple t = tuple(1);

        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // first subquery
        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(q1.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update query
        qep_tree = build_subquery_qep(q2.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_revoke_privilege(const char* name, const char *obj_name, const char *obj_type, const char *grantee)
{
    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;
    std::string q1, q2;

    if (!obj_type)
    {
        q1 = "if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/role[@role_name = 'DBA'])\
              then\
                 fn:true()\
              else\
                 fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3075'), 'Only DBA is allowed to revoke database privileges')";

        q2 =
            "update delete \
                 (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%grantee%']/privileges/privilege[pr_name='%name%'],\
                  doc('%db_sec_doc%')/db_security_data/roles/role[role_name = '%grantee%']/privileges/privilege[pr_name='%name%'])";

        find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&q1, "%user%", tr_globals::login);

        find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&q2, "%name%", name);
        find_replace_str(&q2, "%grantee%", grantee);
    }
    else
    {
        q1 =
           "let $p1 := doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%grantee%']/privileges/privilege\
                        [pr_name = '%name%' and fn:deep-equal(database_obj, <database_obj type_obj='%obj_type%'>%obj_name%</database_obj>)],\
                $p2 := doc('%db_sec_doc%')/db_security_data/roles/role[role_name = '%grantee%']/privileges/privilege\
                        [pr_name = '%name%' and fn:deep-equal(database_obj, <database_obj type_obj='%obj_type%'>%obj_name%</database_obj>)]\
            return\
                if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/role[@role_name = 'DBA'])\
                then\
                    fn:true()\
                else if ((empty($p1) or $p1/grantor='%user%') and (empty($p2) or $p2/grantor='%user%'))\
                    then\
                        fn:true()\
                    else\
                        fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3075'), 'Not allowed to revoke this privilege')";

        q2 =
            "update delete\
                 (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%grantee%']/privileges/privilege\
                    [pr_name = '%name%' and fn:deep-equal(database_obj, <database_obj type_obj='%obj_type%'>%obj_name%</database_obj>)],\
                  doc('%db_sec_doc%')/db_security_data/roles/role[role_name = '%grantee%']/privileges/privilege\
                    [pr_name = '%name%' and fn:deep-equal(database_obj, <database_obj type_obj='%obj_type%'>%obj_name%</database_obj>)])";

        find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&q1, "%user%", tr_globals::login);
        find_replace_str(&q1, "%name%", name);
        find_replace_str(&q1, "%grantee%", grantee);
        find_replace_str(&q1, "%obj_type%", obj_type);
        find_replace_str(&q1, "%obj_name%", obj_name);

        find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
        find_replace_str(&q2, "%name%", name);
        find_replace_str(&q2, "%grantee%", grantee);
        find_replace_str(&q2, "%obj_type%", obj_type);
        find_replace_str(&q2, "%obj_name%", obj_name);
    }

    try
    {
        tuple t = tuple(1);

        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // first subquery
        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(q1.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update query
        qep_tree = build_subquery_qep(q2.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}

void auth_for_revoke_role(const char* name, const char *grantee)
{
    PPQueryEssence* qep_tree   = NULL;
    PPSubQuery *aqtree        = NULL;
    bool is_qep_opened         = false;
    bool is_qep_built          = false;
    bool is_qepsubtree_opened  = false;
    bool is_qepsubtree_built   = false;
    bool output_enabled        = false;

    // checks if we have privilege
    std::string q1 =
       "if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%']/role[@role_name = 'DBA'])\
        then\
            fn:true()\
        else if ((doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%grantee%']/role\
                    [@role_name = '%role%' and @grantor='%user%']))\
            then\
                fn:true()\
            else\
                fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3076'), 'Not allowed to revoke this role')";

    // update for grantee-user
    std::string q2 = "update delete\
                           doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%grantee%']/role[@role_name='%role%']";


    find_replace_str(&q1, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q1, "%user%", tr_globals::login);
    find_replace_str(&q1, "%grantee%", grantee);
    find_replace_str(&q1, "%role%", name);

    find_replace_str(&q2, "%db_sec_doc%", SECURITY_METADATA_DOCUMENT);
    find_replace_str(&q2, "%grantee%", grantee);
    find_replace_str(&q2, "%role%", name);

    try
    {
        tuple t = tuple(1);

        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // first subquery
        aqtree = dynamic_cast<PPSubQuery *>(build_subquery_qep(q1.c_str(), TL_XQuery));
        is_qepsubtree_built = true;

        aqtree->open();
        is_qepsubtree_opened = true;

        aqtree->next(t);

        aqtree->close();
        is_qepsubtree_opened = false;

        delete aqtree;
        is_qepsubtree_built = false;

        // update query
        qep_tree = build_subquery_qep(q2.c_str(), TL_XQuery);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete qep_tree;
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException)
    {
        if(is_qepsubtree_opened)
            aqtree->close();
        if(is_qepsubtree_built)
            delete aqtree;
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete qep_tree;
        if(output_enabled)
            tr_globals::client->enable_output();

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        throw;
    }
}
