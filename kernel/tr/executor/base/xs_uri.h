/*
 * File:  xs_uri.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _XS_URI_H
#define _XS_URI_H

#include "common/sedna.h"
#include "tr/strings/strings.h"
#include "tr/executor/base/tuple.h"

struct Uri
{
private:
    str_counted_ptr scheme;     
    str_counted_ptr authority;  
    str_counted_ptr path;       
    str_counted_ptr query;      
    str_counted_ptr fragment;   

    bool scheme_defined;
    bool authority_defined;
    bool query_defined;
    bool fragment_defined;
    
    ////////////////////////////////////////////////////////////////////
    /// Use parse method to create an instance of this struct.
    Uri() : scheme_defined   (false),
            authority_defined(false),
            query_defined    (false),   
            fragment_defined (false)
    {}
    ////////////////////////////////////////////////////////////////////
    
public:
    enum UriType
    {
        UT_RELATIVE,
        UT_ABSOLUTE
    };
    
    struct Information
    {
        UriType type;     /// one of the possible URI types - relative or absolute;
        bool normalized;  /// 'true' if given URI is normalized, i.e. it doesn't contain leading and trailing whitespaces.
    };

    ////////////////////////////////////////////////////////////////////
    /// Accessors to parsed URI components as defined in RFC 3986.
    /// Each of the components can be NULL. It means that component is
    /// not defined or empty. Use 'defined' accessors to check
    /// if component defined or not.
    ////////////////////////////////////////////////////////////////////
    char* get_scheme()    const { return scheme.get();    }  
    char* get_authority() const { return authority.get(); } 
    char* get_path()      const { return path.get();      }
    char* get_query()     const { return query.get();     } 
    char* get_fragment()  const { return fragment.get();  } 
    
    ////////////////////////////////////////////////////////////////////
    /// Accessors to chech if component is defined or not.
    /// RFC 3986: A component is undefined if its preceding separator 
    /// does not appear in the URI reference; the path component is 
    /// never undefined, though it may be empty.
    ////////////////////////////////////////////////////////////////////
    bool is_scheme_defined()    const { return scheme_defined;    }
    bool is_authority_defined() const { return authority_defined; }
    bool is_path_defined()      const { return true;              }
    bool is_query_defined()     const { return query_defined;     }
    bool is_fragment_defined()  const { return fragment_defined;  }
    
    ////////////////////////////////////////////////////////////////////
    /// Check constraints for URI type as described in RFC 3986 and
    /// in XML Schema Part 2. If URI is valid 'nfo' will contain 
    /// information described this URI, else 'nfo' is unchanged.
    ////////////////////////////////////////////////////////////////////
    static void check_constraints(const tuple_cell *in_tc, bool *valid, Information *nfo);
    static void check_constraints(const char* s, bool *valid, Information *nfo);
    
    ////////////////////////////////////////////////////////////////////
    /// Resolves URI as described in RFC 3986.
    /// If $base is relative URI then throws FORG0009.
    /// Else $dest will be initialized with target URI.
    ////////////////////////////////////////////////////////////////////
    static bool resolve(const char* relative, const char* base, stmt_str_buf &dest);

    ////////////////////////////////////////////////////////////////////
    /// Component recomposition algorithm implementation (RFC 3986). 
    ////////////////////////////////////////////////////////////////////
    void recompose(stmt_str_buf &dest) const;

    ////////////////////////////////////////////////////////////////////
    /// Parses given string into uri components.
    /// Throws SE1003 if string contains invalid URI.
    ////////////////////////////////////////////////////////////////////
    static Uri parse(const char* u); 

    ////////////////////////////////////////////////////////////////////
    /// If it is 100% known that URI is valid the following helper 
    /// represents fast solution to check is it relative or not.
    /// It also fills Uri::Information struct as side effect.
    ////////////////////////////////////////////////////////////////////
    static bool is_relative(const char *s, Uri::Information *nfo);
    static bool is_relative(const tuple_cell *in_tc, Uri::Information *nfo);

};

#endif /* _XS_URI_H */
