/*
 * File:  xs_uri.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _XS_URI_H
#define _XS_URI_H

#include "sedna.h"
#include "PPBase.h"

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
    /// Check constraints for URI type as described in RFC 3986.
    /// If URI is valid returns normalized value, else returns 
    /// copy of the same tuple cell.
    ////////////////////////////////////////////////////////////////////
    static tuple_cell chech_constraints_for_xs_anyURI(const tuple_cell *in_tc, bool *valid);
    static tuple_cell chech_constraints_for_xs_anyURI(char* s, bool *valid);
    
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
};

#endif
