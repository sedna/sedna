/*
* File:  auc.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <string>

#include "common/sedna.h"
#include "common/u/uprocess.h"
#include "common/errdbg/d_printf.h"

#include "tr/auth/auc.h"
#include "tr/executor/por2qep/por2qep.h"
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

void getSednaAuthMetadataPath(char* path)
{
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
}

void auth_for_query(counted_ptr<db_entity> dbe)
{
    if (!tr_globals::authorization) return; //if authorization if off

    bool is_qep_opened = false, is_qep_built = false;
    typedef pair <counted_ptr<db_entity>, struct dbe_properties> authPair;
    auth_map::iterator mapIter;
    qep_subtree *aqtree = NULL;
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
            type_obj = (dbe->type == dbe_document) ? "document" : "collection";
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
            auth_xquery.replace(auth_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
            auth_xquery.replace(auth_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
            auth_xquery.replace(auth_xquery.find("%type_obj%", 0), strlen("%type_obj%"), (dbe->type == dbe_document) ? "document" : "collection");
            auth_xquery.replace(auth_xquery.find("%name_obj%", 0), strlen("%name_obj%"), dbe->name);

            tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;

            aqtree = build_subqep(auth_xquery.c_str(), false);
            is_qep_built = true;

            aqtree->tree.op->open();
            is_qep_opened = true;

            tuple t = tuple(1);
            aqtree->tree.op->next(t);

            if (!t.cells[0].is_light_atomic())
                throw USER_EXCEPTION2(SE1003, "Failed while authorization checking");

            if (t.cells[0].get_atomic_type() != xs_integer)
                throw USER_EXCEPTION2(SE1003, "Failed while authorization checking");

            int update_privileges = t.cells[0].get_xs_integer();

            aqtree->tree.op->next(t);
            if(!t.is_eos())
                throw USER_EXCEPTION2(SE1003, "Failed while authorization checking");

            aqtree->tree.op->close();
            is_qep_opened = false;

            delete_qep(aqtree);
            is_qep_built = false;

            tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

            //put the new dbe into the authmap
            dbe_properties dbe_p;
            dbe_p.update_privileges = update_privileges;
            dbe_p.current_statement = true;

            pair<auth_map::iterator, bool> pr;
            pr = amap.insert(authPair(dbe, dbe_p ));
        }
        catch(SednaUserException &e)
        {
            if(is_qep_opened)
                aqtree->tree.op->close();
            if(is_qep_built)
                delete_qep(aqtree);

            throw;
        }
    }
}

void auth_for_load_module(const char* module_name)
{
    if (!tr_globals::authorization) return; //if authorization if off

    PPQueryEssence* qep_tree = NULL;
    bool is_qep_opened  = false;
    bool is_qep_built   = false;
    bool output_enabled = false;

    std::string update_load_module_xquery =
        "declare ordering unordered;\
         declare variable $db_sec_doc := '%db_sec_doc%';\
         declare variable $user := '%user%';\
         delcare variable $name_obj := '%name_obj%';\
         \
         update insert \
             if (doc($db_sec_doc)/db_security_data/users/user[user_name = $user]/privileges/privilege[database_obj = $name_obj]) \
             then \
                 () \
             else \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='module'>{$name_obj}</database_obj>\
                    <grantor>{$user}</grantor>\
                 </privilege>\
         into\
             doc($db_sec_doc)/db_security_data/users/user[user_name = $user]/privileges";

    // substitute dynamic parameters
    update_load_module_xquery.replace(update_load_module_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
    update_load_module_xquery.replace(update_load_module_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
    update_load_module_xquery.replace(update_load_module_xquery.find("%name_obj%", 0), strlen("%name_obj%"), module_name);

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        qep_tree = build_qep(update_load_module_xquery.c_str(), false);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete_qep(qep_tree);
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete_qep(qep_tree);
        if(output_enabled)
            tr_globals::client->enable_output();
        throw;
    }
}

void auth_for_drop_module(const char* mod_name)
{
    if (!tr_globals::authorization) return;

    qep_subtree *aqtree        = NULL;
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
        md_xquery.replace(md_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        md_xquery.replace(md_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        md_xquery.replace(md_xquery.find("%name_obj%", 0), strlen("%name_obj%"), mod_name);

        aqtree = build_subqep(md_xquery.c_str(), false);
        is_qepsubtree_built = true;

        aqtree->tree.op->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->tree.op->next(t);

        aqtree->tree.op->close();
        is_qepsubtree_opened = false;

        delete_qep(aqtree);
        is_qepsubtree_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qepsubtree_opened)
            aqtree->tree.op->close();
        if(is_qepsubtree_built)
            delete_qep(aqtree);
        if(output_enabled)
            tr_globals::client->enable_output();
        throw;
    }
}

void auth_for_rename_collection(const char* old_name, const char* new_name)
{
    if (!tr_globals::authorization) return;

    PPQueryEssence* qep_tree   = NULL;
    qep_subtree *aqtree        = NULL;
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
        rc_xquery.replace(rc_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        rc_xquery.replace(rc_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        rc_xquery.replace(rc_xquery.find("%old_name_obj%", 0), strlen("%old_name_obj%"), old_name);

        aqtree = build_subqep(rc_xquery.c_str(), false);
        is_qepsubtree_built = true;

        aqtree->tree.op->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->tree.op->next(t);

        aqtree->tree.op->close();
        is_qepsubtree_opened = false;

        delete_qep(aqtree);
        is_qepsubtree_built = false;

        // update db_security_data for the new collection name
        // substitute dynamic parameters
        update_rc_xquery.replace(update_rc_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        update_rc_xquery.replace(update_rc_xquery.find("%old_name_obj%", 0), strlen("%old_name_obj%"), old_name);
        update_rc_xquery.replace(update_rc_xquery.find("%new_name_obj%", 0), strlen("%new_name_obj%"), new_name);

        qep_tree = build_qep(update_rc_xquery.c_str(), false);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete_qep(qep_tree);
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qepsubtree_opened)
            aqtree->tree.op->close();
        if(is_qepsubtree_built)
            delete_qep(aqtree);
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete_qep(qep_tree);
        if(output_enabled)
            tr_globals::client->enable_output();
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

    auth_map::iterator mIter;
    xptr_sequence::iterator it=(*seq).begin();
    xptr node;

    try
    {
        while (it!=(*seq).end())
        {
            if(!direct)
                node= removeIndirection(*it);
            else
                node = *it;

            CHECKP(node);
            schema_node_xptr sn        = ((node_blk_hdr*)GETBLOCKBYNODE(node)) -> snode -> root;
            schema_node_xptr dbe_root  = XNULL;

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
    catch(SednaUserException &e)
    {
        security_metadata_upd_controll();
        throw;
    }

    security_metadata_upd_controll();
}

void auth_for_create_document(const char* doc_name)
{
    if (!tr_globals::authorization) return;

    PPQueryEssence* qep_tree   = NULL;
    qep_subtree *aqtree        = NULL;
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
        "declare variable $user := '%user%';\
         update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='document'>{%name_obj%}</database_obj>\
                    <grantor>{$user}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = $user]/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        rc_xquery.replace(rc_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        rc_xquery.replace(rc_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);

        aqtree = build_subqep(rc_xquery.c_str(), false);
        is_qepsubtree_built = true;

        aqtree->tree.op->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->tree.op->next(t);

        aqtree->tree.op->close();
        is_qepsubtree_opened = false;

        delete_qep(aqtree);
        is_qepsubtree_built = false;

        // update db_security_data for the new document
        // substitute dynamic parameters
        update_xquery.replace(update_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        update_xquery.replace(update_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        update_xquery.replace(update_xquery.find("%name_obj%", 0), strlen("%name_obj%"), doc_name);

        qep_tree = build_qep(update_xquery.c_str(), false);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete_qep(qep_tree);
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qepsubtree_opened)
            aqtree->tree.op->close();
        if(is_qepsubtree_built)
            delete_qep(aqtree);
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete_qep(qep_tree);
        if(output_enabled)
            tr_globals::client->enable_output();
        throw;
    }
}

void auth_for_load_document(const char* doc_name)
{
    if (!tr_globals::authorization) return;

    PPQueryEssence* qep_tree   = NULL;
    qep_subtree *aqtree        = NULL;
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
                error(QName('http://www.modis.ispras.ru/sedna','SE3072'), 'User does not have CREATE-DOCUMENT privilege')";


    std::string update_xquery =
        "declare variable $user := '%user%';\
         update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='document'>{%name_obj%}</database_obj>\
                    <grantor>{$user}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = $user]/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        rc_xquery.replace(rc_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        rc_xquery.replace(rc_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);

        aqtree = build_subqep(rc_xquery.c_str(), false);
        is_qepsubtree_built = true;

        aqtree->tree.op->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->tree.op->next(t);

        aqtree->tree.op->close();
        is_qepsubtree_opened = false;

        delete_qep(aqtree);
        is_qepsubtree_built = false;

        // update db_security_data for the new document
        // substitute dynamic parameters
        update_xquery.replace(update_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        update_xquery.replace(update_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        update_xquery.replace(update_xquery.find("%name_obj%", 0), strlen("%name_obj%"), doc_name);

        qep_tree = build_qep(update_xquery.c_str(), false);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete_qep(qep_tree);
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qepsubtree_opened)
            aqtree->tree.op->close();
        if(is_qepsubtree_built)
            delete_qep(aqtree);
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete_qep(qep_tree);
        if(output_enabled)
            tr_globals::client->enable_output();
        throw;
    }
}

void auth_for_create_collection(const char* coll_name)
{
    if (!tr_globals::authorization) return;

    PPQueryEssence* qep_tree   = NULL;
    qep_subtree *aqtree        = NULL;
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
        "declare variable $user := '%user%';\
         update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='collection'>{%name_obj%}</database_obj>\
                    <grantor>{$user}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = $user]/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        rc_xquery.replace(rc_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        rc_xquery.replace(rc_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);

        aqtree = build_subqep(rc_xquery.c_str(), false);
        is_qepsubtree_built = true;

        aqtree->tree.op->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->tree.op->next(t);

        aqtree->tree.op->close();
        is_qepsubtree_opened = false;

        delete_qep(aqtree);
        is_qepsubtree_built = false;

        // update db_security_data for the new collection
        // substitute dynamic parameters
        update_xquery.replace(update_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        update_xquery.replace(update_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        update_xquery.replace(update_xquery.find("%name_obj%", 0), strlen("%name_obj%"), coll_name);

        qep_tree = build_qep(update_xquery.c_str(), false);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete_qep(qep_tree);
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qepsubtree_opened)
            aqtree->tree.op->close();
        if(is_qepsubtree_built)
            delete_qep(aqtree);
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete_qep(qep_tree);
        if(output_enabled)
            tr_globals::client->enable_output();
        throw;
    }
}

void auth_for_create_document_collection(const char* doc_name, const char *coll_name)
{
    if (!tr_globals::authorization) return;

    qep_subtree *aqtree        = NULL;
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
        md_xquery.replace(md_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        md_xquery.replace(md_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        md_xquery.replace(md_xquery.find("%name_obj%", 0), strlen("%name_obj%"), coll_name);

        aqtree = build_subqep(md_xquery.c_str(), false);
        is_qepsubtree_built = true;

        aqtree->tree.op->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->tree.op->next(t);

        aqtree->tree.op->close();
        is_qepsubtree_opened = false;

        delete_qep(aqtree);
        is_qepsubtree_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qepsubtree_opened)
            aqtree->tree.op->close();
        if(is_qepsubtree_built)
            delete_qep(aqtree);
        if(output_enabled)
            tr_globals::client->enable_output();
        throw;
    }
}

void auth_for_load_document_collection(const char* doc_name, const char *coll_name)
{
    if (!tr_globals::authorization) return;

    qep_subtree *aqtree        = NULL;
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
                error(QName('http://www.modis.ispras.ru/sedna','SE3065'), 'User does not have CREATE-DOCUMENT privilege on the collection')";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has a privilege to rename this collection (privilege DROP is needed)
        md_xquery.replace(md_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        md_xquery.replace(md_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        md_xquery.replace(md_xquery.find("%name_obj%", 0), strlen("%name_obj%"), coll_name);

        aqtree = build_subqep(md_xquery.c_str(), false);
        is_qepsubtree_built = true;

        aqtree->tree.op->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->tree.op->next(t);

        aqtree->tree.op->close();
        is_qepsubtree_opened = false;

        delete_qep(aqtree);
        is_qepsubtree_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;

        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qepsubtree_opened)
            aqtree->tree.op->close();
        if(is_qepsubtree_built)
            delete_qep(aqtree);
        if(output_enabled)
            tr_globals::client->enable_output();
        throw;
    }
}

void auth_for_create_index(const char* ind_name, const char *obj_name, bool is_collection)
{
    if (!tr_globals::authorization) return;

    PPQueryEssence* qep_tree   = NULL;
    qep_subtree *aqtree        = NULL;
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
        "declare variable $user := '%user%';\
         update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='index'>{%name_obj%}</database_obj>\
                    <grantor>{$user}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = $user]/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        rc_xquery.replace(rc_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        rc_xquery.replace(rc_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        rc_xquery.replace(rc_xquery.find("%type_obj%", 0), strlen("%type_obj%"), (is_collection) ? "collection" : "document");
        rc_xquery.replace(rc_xquery.find("%name_obj%", 0), strlen("%name_obj%"), obj_name);

        aqtree = build_subqep(rc_xquery.c_str(), false);
        is_qepsubtree_built = true;

        aqtree->tree.op->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->tree.op->next(t);

        aqtree->tree.op->close();
        is_qepsubtree_opened = false;

        delete_qep(aqtree);
        is_qepsubtree_built = false;

        // update db_security_data for the new index
        // substitute dynamic parameters
        update_xquery.replace(update_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        update_xquery.replace(update_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        update_xquery.replace(update_xquery.find("%name_obj%", 0), strlen("%name_obj%"), ind_name);

        qep_tree = build_qep(update_xquery.c_str(), false);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete_qep(qep_tree);
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qepsubtree_opened)
            aqtree->tree.op->close();
        if(is_qepsubtree_built)
            delete_qep(aqtree);
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete_qep(qep_tree);
        if(output_enabled)
            tr_globals::client->enable_output();
        throw;
    }
}

void auth_for_create_trigger(const char *trg_name)
{
    if (!tr_globals::authorization) return;

    PPQueryEssence* qep_tree   = NULL;
    qep_subtree *aqtree        = NULL;
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
        "declare variable $user := '%user%';\
         update insert \
                 <privilege>\
                    <pr_name>OWNER</pr_name>\
                    <database_obj type_obj='trigger'>{%name_obj%}</database_obj>\
                    <grantor>{$user}</grantor>\
                 </privilege>\
         into\
             doc('%db_sec_doc%')/db_security_data/users/user[user_name = $user]/privileges";

    try
    {
        tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = tr_globals::client->disable_output();

        // query if user has the privilege
        rc_xquery.replace(rc_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        rc_xquery.replace(rc_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        rc_xquery.replace(rc_xquery.find("%name_obj%", 0), strlen("%name_obj%"), trg_name);

        aqtree = build_subqep(rc_xquery.c_str(), false);
        is_qepsubtree_built = true;

        aqtree->tree.op->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->tree.op->next(t);

        aqtree->tree.op->close();
        is_qepsubtree_opened = false;

        delete_qep(aqtree);
        is_qepsubtree_built = false;

        // update db_security_data for the new trigger
        // substitute dynamic parameters
        update_xquery.replace(update_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        update_xquery.replace(update_xquery.find("%user%", 0), strlen("%user%"), tr_globals::login);
        update_xquery.replace(update_xquery.find("%name_obj%", 0), strlen("%name_obj%"), trg_name);

        qep_tree = build_qep(update_xquery.c_str(), false);
        is_qep_built = true;

        qep_tree->open();
        is_qep_opened = true;

        qep_tree->execute();

        qep_tree->close();
        is_qep_opened = false;

        delete_qep(qep_tree);
        is_qep_built = false;

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qepsubtree_opened)
            aqtree->tree.op->close();
        if(is_qepsubtree_built)
            delete_qep(aqtree);
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete_qep(qep_tree);
        if(output_enabled)
            tr_globals::client->enable_output();
        throw;
    }
}

void auth_for_drop_object(const char* obj_name, const char *obj_type, bool just_check)
{
    if (!tr_globals::authorization) return;

    PPQueryEssence* qep_tree   = NULL;
    qep_subtree *aqtree        = NULL;
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
        rc_xquery.replace(rc_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
        rc_xquery.replace(rc_xquery.find("%type_obj%", 0), strlen("%type_obj%"), obj_type);
        rc_xquery.replace(rc_xquery.find("%name_obj%", 0), strlen("%name_obj%"), obj_name);

        aqtree = build_subqep(rc_xquery.c_str(), false);
        is_qepsubtree_built = true;

        aqtree->tree.op->open();
        is_qepsubtree_opened = true;

        tuple t = tuple(1);
        aqtree->tree.op->next(t);

        aqtree->tree.op->close();
        is_qepsubtree_opened = false;

        delete_qep(aqtree);
        is_qepsubtree_built = false;

        if (!just_check)
        {
            // update db_security_data for the new index
            // substitute dynamic parameters
            update_xquery.replace(update_xquery.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
            update_xquery.replace(update_xquery.find("%type_obj%", 0), strlen("%type_obj%"), obj_type);
            update_xquery.replace(update_xquery.find("%name_obj%", 0), strlen("%name_obj%"), obj_name);

            qep_tree = build_qep(update_xquery.c_str(), false);
            is_qep_built = true;

            qep_tree->open();
            is_qep_opened = true;

            qep_tree->execute();

            qep_tree->close();
            is_qep_opened = false;

            delete_qep(qep_tree);
            is_qep_built = false;
        }

        tr_globals::internal_auth_switch = DEPLOY_AUTH_CHECK;
        if (output_enabled)
            tr_globals::client->enable_output();
    }
    catch(SednaUserException &e)
    {
        if(is_qepsubtree_opened)
            aqtree->tree.op->close();
        if(is_qepsubtree_built)
            delete_qep(aqtree);
        if(is_qep_opened)
            qep_tree->close();
        if(is_qep_built)
            delete_qep(qep_tree);
        if(output_enabled)
            tr_globals::client->enable_output();
        throw;
    }
}
