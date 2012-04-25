#include "DataSources.h"

#include <sstream>

#include "tr/executor/base/xs_uri.h"
#include "tr/structures/metadata.h"

DataRoot::DataRoot(const data_root_type_t _type, const char* _name)
: type(_type), name(NULL)
{
    name = new std::string(_name);
}

DataRoot::DataRoot(const scheme_list* lst) : type(drt_null), name(NULL)
{
    if (lst->size() != 2 || lst->at(0).type != SCM_SYMBOL || lst->at(1).type != SCM_STRING) {
        throw USER_EXCEPTION2(SE1004, "Invalid root LR string");
    }
    
    const char * _type = lst->at(0).internal.symb;
    
    switch (_type[0]) {
        case 'd' : if (strncmp("doc", _type, 3) == 0) {
            type = drt_document;
            break;
        };
        case 'c' : if (strncmp("coll", _type, 4) == 0) {
            type = drt_collection;
            break;
        };
        default :
            throw USER_EXCEPTION2(SE1004, "Invalid root type");
    };
    
    name = new std::string(lst->at(1).internal.str);
}

std::string DataRoot::toLRString() const
{
    std::stringstream stream;

    stream << "(";

    switch(type) {
        case drt_document :
            stream << " doc ";
            stream << "\"" << *name << "\"";
            break;
        case drt_collection :
            stream << " collection ";
            stream << "\"" << *name << "\"";
            break;
        case drt_null :
            stream << " null ";
            break;
        case drt_external :
            stream << " ext ";
            stream << "\"" << *name << "\"";
            break;
    };

    stream << ")";

    return stream.str();
}

schema_node_cptr DataRoot::getSchemaNode() const
{
    schema_node_xptr root = XNULL;
    const char * a_type;
    int error;

    bool valid;
    Uri::check_constraints(name->c_str(), &valid, NULL);

    switch(type) {
        case drt_document :
            if (!valid) {
                throw XQUERY_EXCEPTION2(FODC0005, (std::string("Invalid document URI '") + name.get() + "'").c_str());
            };
            break;
        case drt_collection :
            error = FODC0002;
            a_type = "collection";
            break;
        case drt_external :
            error = FODC0005;
            a_type = "external document";
            break;
        default:
        case drt_null :
            U_ASSERT(false);
    };

    document_or_collection_exists();
    
    if(!valid)
    {
        if (db_ent->type == dbe_document)
        else
            throw XQUERY_EXCEPTION2(FODC0002, (std::string("Invalid collection URI '") + db_ent->name + "'").c_str());
    }
    
    if (!document_or_collection_exists(db_ent->name))
    {
        if (db_ent->type == dbe_document)
            throw XQUERY_EXCEPTION2(FODC0002, (std::string("Document '") + db_ent->name + "'").c_str());
        else
            throw XQUERY_EXCEPTION2(FODC0004, (std::string("Collection '") + db_ent->name + "'").c_str());
    }
    
    switch (db_ent->type)
    {
        case dbe_document   : local_lock_mrg->put_lock_on_document  (db_ent->name); break;
        case dbe_collection : local_lock_mrg->put_lock_on_collection(db_ent->name); break;
        default             : throw USER_EXCEPTION2(SE1003, err_details);
    }
    
    // at this point doc/coll could have become deleted or replaced due to recovery
    // so we must check its existence again and obtain proper root pointer
    switch (db_ent->type)
    {
        case dbe_document   : root = find_document  (db_ent->name); break;
        case dbe_collection : root = find_collection(db_ent->name); break;
        default             : throw USER_EXCEPTION2(SE1003, err_details);
    }
    
    #ifdef SE_ENABLE_TRIGGERS
    nested_updates_tracking(local_lock_mrg->get_cur_lock_mode(), root, db_ent->name);
    #endif
    
    if (root == XNULL)
    {
        if (db_ent->type == dbe_document)
            throw XQUERY_EXCEPTION2(FODC0002, (std::string("Document '") + db_ent->name + "'").c_str());
        else
            throw XQUERY_EXCEPTION2(FODC0004, (std::string("Collection '") + db_ent->name + "'").c_str());
    }
    // here we can be sure that doc/coll exists and is properly locked
    
    lock_mode cur_lock = local_lock_mrg->get_cur_lock_mode();
    local_lock_mrg->lock(lm_s);
    
    auth_for_query(db_ent);
    
    local_lock_mrg->lock(cur_lock);
    
    return root;
    //
}
