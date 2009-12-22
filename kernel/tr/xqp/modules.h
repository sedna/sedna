/*
 * File:  modules.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __MODULES_H
#define __MODULES_H

#include <string>

#include "common/sedna.h"

#include "tr/tr_base.h"
#include "tr/xqp/XQuerytoLR.h"
#include "tr/client_core.h"

std::string prepare_modules(const std::vector<client_file> &cf_vec, std::string *module_name);
char *get_module(const char *module_uri);

#endif
