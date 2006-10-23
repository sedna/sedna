/*
 * File:  uri.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _URI_H
#define _URI_H

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
    char* const get_scheme()    { return scheme.get();    }  
    char* const get_authority() { return authority.get(); } 
    char* const get_path()      { return path.get();      }
    char* const get_query()     { return query.get();     } 
    char* const get_fragment()  { return fragment.get();  } 
    
    ////////////////////////////////////////////////////////////////////
    /// Accessors to chech if component is defined or not.
    /// RFC 3986: A component is undefined if its preceding separator 
    /// does not appear in the URI reference; the path component is 
    /// never undefined, though it may be empty.
    ////////////////////////////////////////////////////////////////////
    bool is_scheme_defined()    { return scheme_defined;    }
    bool is_authority_defined() { return authority_defined; }
    bool is_path_defined()      { return true;              }
    bool is_query_defined()     { return query_defined;     }
    bool is_fragment_defined()  { return fragment_defined;  }
    
    ////////////////////////////////////////////////////////////////////
    /// Check constraints for URI type as described in RFC 3986.
    ////////////////////////////////////////////////////////////////////
    static bool chech_constraints_for_xs_anyURI   (const tuple_cell *tc);
    
    ////////////////////////////////////////////////////////////////////
    /// Resolves URI as described in RFC 3986.
    /// If $base is relative URI then throws FORG0009.
    /// Else $dest will be initialized with target URI.
    ////////////////////////////////////////////////////////////////////
    static bool resolve(const char* relative, const char* base, t_str_buf &dest);

    ////////////////////////////////////////////////////////////////////
    /// Component recomposition algorithm implementation (RFC 3986). 
    ////////////////////////////////////////////////////////////////////
    void recompose(t_str_buf &dest);

    ////////////////////////////////////////////////////////////////////
    /// Parses given string into uri components.
    /// Throws SE1003 if string contains invalid URI.
    ////////////////////////////////////////////////////////////////////
    static Uri parse(const char* u);
};

#endif
