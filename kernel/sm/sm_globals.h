/*
 * File:  sm_globals.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SM_GLOBALS_H
#define _SM_GLOBALS_H

#include "u/u.h"

class MessageExchanger;
struct DatabaseOptions;

extern DatabaseOptions * databaseOptions;
extern MessageExchanger * govMessager;
extern global_name smMessageServerName;

/*
void         register_sm_on_gov         (MessageExchanger* communicator);
void         unregister_sm_on_gov       (MessageExchanger* communicator);
void         register_cdb_on_gov        (MessageExchanger* communicator);
*/

#endif /* _SM_GLOBALS_H */
