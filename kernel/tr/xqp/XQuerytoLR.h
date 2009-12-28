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
#include "tr/executor/base/dynamic_context.h"
#include "XQueryDriver.h"

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

struct qep_subtree
{
    dynamic_context *cxt;
    PPOpIn tree;
};

void parse_batch(sedna::XQueryDriver *drv, QueryType type, const char *batch, std::string *module_name);
StringVector parse_xq_to_ast(const char *batch);
void parse_batch(sedna::XQueryDriver *drv, QueryType type, StringVector batch, std::string *module_name);
void parse_batch_context(sedna::XQueryDriver *drv, const char *query, QueryType type, static_context *sx);

PPQueryEssence *build_qep(const char*  por, bool is_ast);
qep_subtree    *build_subqep(const char*  por, bool is_ast);

void delete_qep(PPQueryEssence *qep);
void delete_qep_unmanaged(PPQueryEssence *qep);
void delete_qep(qep_subtree *qep);

#endif /* _XQUERYTOLR_H */
