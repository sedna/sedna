#include "DataSources.h"

#include <sstream>

#include "tr/executor/base/xs_uri.h"
#include "tr/structures/metadata.h"
#include "tr/locks/locks.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers.h"
#endif /* SE_ENABLE_TRIGGERS */

DataRoot::DataRoot(const data_root_type_t _type, const char* _name)
: type(_type), name(NULL), snode(XNULL)
{
    name = new std::string(_name);
}

DataRoot::DataRoot(const scheme_list* lst) : type(drt_null), name(NULL), snode(XNULL)
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

schema_node_xptr DataRoot::_getSchemaNode() const
{
    schema_node_xptr result = XNULL;
    const char * a_type;
    std::string errorMessage;
    bool validURI;
    bool entityExists;

    Uri::check_constraints(name->c_str(), &validURI, NULL);

    if (validURI) {
        entityExists = document_or_collection_exists(name->c_str());
    };

    switch(type) {
        case drt_document :
            if (!validURI) {
                errorMessage = "Invalid document URI '" + *name + "'";
                throw USER_EXCEPTION2(FODC0005, errorMessage.c_str());
            };

            if (!entityExists) {
                errorMessage = "Document '" + *name + "'";
                throw USER_EXCEPTION2(FODC0002, errorMessage.c_str());
            };

            local_lock_mrg->put_lock_on_document(name->c_str());
            result = find_document(name->c_str());
            
            break;
        case drt_collection :
            if (!validURI) {
                errorMessage = "Invalid collection URI '" + *name + "'";
                throw USER_EXCEPTION2(FODC0004, errorMessage.c_str());
            };
            
            if (!entityExists) {
                errorMessage = "Collection '" + *name + "'";
                throw USER_EXCEPTION2(FODC0002, errorMessage.c_str());
            };

            local_lock_mrg->put_lock_on_collection(name->c_str());
            result = find_collection(name->c_str());

            break;
        case drt_external :
            U_ASSERT(false);
/*          
            if (!validURI) {
                throw XQUERY_EXCEPTION2(FODC0005, (std::string("Invalid document URI '") + name.get() + "'").c_str());
            };
*/

            break;
        default:
        case drt_null :
            U_ASSERT(false);
    };

    #ifdef SE_ENABLE_TRIGGERS
    nested_updates_tracking(local_lock_mrg->get_cur_lock_mode(), result, name->c_str());
    #endif
    
    return result;
}
