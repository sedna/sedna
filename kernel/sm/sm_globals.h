/*
 * File:  sm_globals.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SM_GLOBALS_H
#define _SM_GLOBALS_H

#include "common/sedna.h"
#include "u/ugnames.h"

struct DatabaseOptions;
extern DatabaseOptions * databaseOptions;

struct SessionIdString
{
    char s_id_str[16];

    SessionIdString(session_id s_id)
    {
        sprintf(s_id_str, "%d", s_id);
        set();
    };

    void set()
    {
        uSetGlobalNameInstanceId(GN_SESSION, s_id_str);
    };

    ~SessionIdString() {
        /* Unset session id from global names */
        uSetGlobalNameInstanceId(GN_SESSION, "x");
    }
};



#endif /* _SM_GLOBALS_H */
