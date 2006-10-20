/*
 * File:  uri.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _URI_H
#define _URI_H

#include "sedna.h"
#include "PPBase.h"
#include "strings.h"
#include "counted_ptr.h"

struct Uri
{
private:
    str_counted_ptr scheme;     
    str_counted_ptr authority;  
    str_counted_ptr path;       
    str_counted_ptr query;      
    str_counted_ptr fragment;   
    
    ////////////////////////////////////////////////////////////////////
    /// Use parse method to create an instance of this struct.
    Uri() {}
    ////////////////////////////////////////////////////////////////////

public:
    ////////////////////////////////////////////////////////////////////
    /// Accessors to parsed URI components as defined in RFC 2396
    /// Each of the component can be NULL. It means that component is
    /// not defined (empty for the 'path' component).
    ////////////////////////////////////////////////////////////////////
    char* const get_scheme()    { return scheme.get();    }  
    char* const get_authority() { return authority.get(); } 
    char* const get_path()      { return path.get();      }
    char* const get_query()     { return query.get();     } 
    char* const get_fragment()  { return fragment.get();  } 
    
    ////////////////////////////////////////////////////////////////////
    /// Check constraints for xs:anyURI type as described in RFC 2396.
    ////////////////////////////////////////////////////////////////////
    static bool chech_constraints_for_xs_anyURI   (const tuple_cell *tc);
    static bool check_constraints_for_absolute_URI(const tuple_cell *tc);
    static bool check_constraints_for_relative_URI(const tuple_cell *tc);
    
    ////////////////////////////////////////////////////////////////////
    /// Resolves URI as described in RFC 2396.
    /// If $base is relative URI then throws FORG0009.
    /// Else $dest will be initialized with target URI.
    ////////////////////////////////////////////////////////////////////
    static void resolve(const char* relative, const char* base, t_str_buf &dest);

    ////////////////////////////////////////////////////////////////////
    /// Component recomposition algorithm implementation (RFC 2396). 
    ////////////////////////////////////////////////////////////////////
    void recompose(t_str_buf &dest);

    ////////////////////////////////////////////////////////////////////
    /// Parses given string into uri components.
    /// Throws SE1003 if string contains invalid URI.
    ////////////////////////////////////////////////////////////////////
    static Uri parse(const char* u);
};

#endif
