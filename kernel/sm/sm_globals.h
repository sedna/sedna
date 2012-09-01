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

struct SetSessionIdString
{
    char s_id_str[16];

    SetSessionIdString(session_id s_id)
    {
        sprintf(s_id_str, "%d", s_id);
        set();
    };

    void set()
    {
        uSetGlobalNameInstanceId(GN_SESSION, s_id_str);
    };

    ~SetSessionIdString() {
        /* Unset session id from global names */
        uSetGlobalNameInstanceId(GN_SESSION, "x");
    }
};


inline
int64_t MBS2PAGES(int64_t s) {
    return (s * 0x100000ULL / PAGE_SIZE);
}

inline
int64_t PAGES2MBS(int64_t s) {
    return (s * PAGE_SIZE / 0x100000ULL);
}

#endif /* _SM_GLOBALS_H */
