/*
 * File:  static_context.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _STATIC_CONTEXT_H
#define _STATIC_CONTEXT_H

#include <vector>
#include <set>
#include <map>

#include "tr/strings/utf8.h"
#include "tr/structures/xmlns.h"
#include "tr/crmutils/global_options.h"
#include "tr/crmutils/str_matcher.h"
#include "tr/executor/base/namespaces.h"

/// query prolog enumerations
enum xq_boundary_space {xq_boundary_space_strip, xq_boundary_space_preserve};
enum xq_ordering_mode  {xq_ordering_mode_ordered, xq_ordering_mode_unordered};
enum xq_empty_order    {xq_empty_order_greatest, xq_empty_order_least};

/*******************************************************************************
 * Define error codes for collations resolving.
 * Since F&O and XQuery specs use different error codes
 * we need to transorm these error codes in place
 * (say in PPOrderBy)
 ******************************************************************************/

#define COLLATION_INVALID_URI        ((int) 0x1)
#define COLLATION_MISS               ((int) 0x2)
#define COLLATION_RESOLVE_ERR        ((int) 0x4)

class static_context
{
public:
    enum static_context_fields_flags
    {
        SC_BOUNDARY_SPACE         = 0x1,
        SC_DEFAULT_COLLATION_URI  = 0x2,
        SC_BASE_URI               = 0x4,
        SC_CONSTRUCTION           = 0x8,
        SC_ORDERING_MODE          = 0x10,
        SC_EMPTY_ORDER            = 0x20,
        SC_NAMESPACE_PRESERVE     = 0x40,
        SC_NAMESPACE_INHERIT      = 0x80,
        SC_NAMESPACE              = 0x100,
        SC_DEFAULT_NAMESPACE      = 0x200,
        SC_OUTPUT_INDENT          = 0x400
    };

private:
    /// Prolog Declarations
    uint32_t prolog_set_fields;
    /// Boundary-space Declaration
    xq_boundary_space boundary_space;
    /// Default Collation Declaration
    char* default_collation_uri;
    /// Base URI Declaration
    char* base_uri;
    /// Construction Declaration ('preserve'=true, 'strip'=false)
    bool preserve_type;
    /// Ordering Mode Declaration
    xq_ordering_mode ordering_mode;
    /// Empty Order Declaration
    xq_empty_order empty_order;

    /// Copy-Namespaces Declaration
    bool cn_preserve;
    bool cn_inherit;

    /// stores pointer to default collation handler already resolved using default collation uri and base uri
    CollationHandler *default_collation_handler;

    // it seems that collation_manager should be static since it doesn't contatin any dynamic info
    static CollationManager collation_manager;

    StaticallyKnownNamespaces staticallyKnownNamespaces;

    std::vector<xmlns_ptr> prologueNamespaces; /* It is here only for PPExplain. WHYY?? */

    typedef std::map<std::string, std::string> OptionMap;
    OptionMap localOptions;

    // serialization parameters: indent, method, etc.
    GlobalSerializationOptions serializationOptions;

    inline void set_field_flag(static_context_fields_flags flag)
    {
        prolog_set_fields |= flag;
    }

public:
    static_context();
    ~static_context();

    inline void setLocalOption(const std::string &opt, const std::string &val) {
        localOptions.insert(OptionMap::value_type(opt, val));
    };

    inline
    std::string getLocalOption(const std::string &opt) const {
        OptionMap::const_iterator x = localOptions.find(opt);
        if (x != localOptions.end()) {
            return x->second;
        } else {
            return "";
        }
    }

    inline StaticallyKnownNamespaces * getStaticallyKnownNamespaces()
    {
        return &staticallyKnownNamespaces;
    }

    inline GlobalSerializationOptions * get_serialization_options()
    {
        return &serializationOptions;
    };

    inline const std::vector<xmlns_ptr> * getPrologueNamespaces() const { return &prologueNamespaces; };
    inline void addPrologueNamespace(const xmlns_ptr ns) { prologueNamespaces.push_back(ns); };

    bool is_field_set_in_prolog(static_context_fields_flags flag)
    {
        return (prolog_set_fields & flag) != 0;
    }

    static inline std::string get_error_description(int err_code)
    {
        switch(err_code)
        {
            case COLLATION_INVALID_URI : return std::string("Invalid lexical representation of the give collation URI");
            case COLLATION_MISS        : return std::string("Collation is not statically known");
            case COLLATION_RESOLVE_ERR : return std::string("Collation URI could not be properly resolved");
            default: throw USER_EXCEPTION2(SE1003, "Impossible case in dynamic_context::get_error_description()");
        }
    }

    // 1. Resolves uri and returns collation handler through "handler".
    // 2. If "uri" is NULL, returns default collation handler.
    // 3. Returns 0 if success, else return one of the collation error
    // codes defined above.
    int get_collation(const char *uri, /* out */ CollationHandler** handler);
    CollationHandler* get_default_collation()
    {
        return default_collation_handler;
    }

    ///Getters and setters for different prolog options
    inline void set_output_method(se_output_method om)
    {
        serializationOptions.xquery_output_method = om;
    }

    inline xq_boundary_space get_boundary_space()
    {
        return boundary_space;
    }
    inline void set_boundary_space(xq_boundary_space bs)
    {
        boundary_space = bs; set_field_flag(SC_BOUNDARY_SPACE);
    }

    inline const char* get_default_collation_uri()
    {
        return default_collation_uri;
    }
    void set_default_collation_uri(const char* _default_collation_uri_);

    inline const char* get_base_uri()
    {
        return base_uri;
    }
    void set_base_uri(const char* _base_uri_);

    inline bool get_construction_mode()
    {
        return preserve_type;
    }

    inline void set_construction_mode(bool cm)
    {
        preserve_type = cm;
        set_field_flag(SC_CONSTRUCTION);
    }

    inline xq_ordering_mode get_ordering_mode()
    {
        return ordering_mode;
    }
    inline void set_ordering_mode(xq_ordering_mode om)
    {
        ordering_mode = om;
        set_field_flag(SC_ORDERING_MODE);
    }

    inline xq_empty_order get_empty_order()
    {
        return empty_order;
    }
    inline void set_empty_order(xq_empty_order eo)
    {
        empty_order = eo;
        set_field_flag(SC_EMPTY_ORDER);
    }

    inline bool is_namespace_preserve()
    {
        return cn_preserve;
    }

    inline void set_namespace_preserve(bool np)
    {
        cn_preserve = np;
        set_field_flag(SC_NAMESPACE_PRESERVE);
    }

    inline bool is_namespace_inherit()
    {
        return cn_inherit;
    }

    inline void set_namespace_inherit(bool ni)
    {
        cn_inherit = ni;
        set_field_flag(SC_NAMESPACE_INHERIT);
    }

    inline void set_output_indent(bool do_indent)
    {
        serializationOptions.indent = do_indent;
        set_field_flag(SC_OUTPUT_INDENT);
    }

    inline void set_nsp(xmlns_ptr ns)
    {
        set_field_flag(SC_NAMESPACE);
    }
};

#endif /* _STATIC_CONTEXT_H */
