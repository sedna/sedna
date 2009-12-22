/*
 * File:  XQuerytoLR.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _XQUERYTOLR_H
#define _XQUERYTOLR_H

#include <string>
#include <vector>

#include "common/sedna.h"

#include "tr/tr_base.h"
#include "tr/executor/por2qep/scheme_tree.h"
#include "tr/xqp/XQueryDriver.h"

struct script_struct
{
  scheme_list* stmnt;
};

typedef std::vector<std::string> StringVector;

class StmntsArray
{
public:
  scheme_list* root;
  std::vector<script_struct> stmnts;

  void operator= (StmntsArray& st)
  {
    root = st.root;

    stmnts.clear();

    std::vector<script_struct>::const_iterator it;

    for (it = st.stmnts.begin(); it != st.stmnts.end(); it++)
       stmnts.push_back(*it);

  };
};

void parse_batch(sedna::XQueryDriver *drv, QueryType type, const char *batch, std::string *module_name);
void parse_batch(sedna::XQueryDriver *drv, QueryType type, StringVector batch, std::string *module_name);
void parse_batch_triggers(sedna::XQueryDriver *drv, const char *query, static_context *sx, dynamic_context *dx);

#endif /* _XQUERYTOLR_H */
