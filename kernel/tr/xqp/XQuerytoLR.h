/*
 * File:  XQuerytoLR.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef XQuerytoLR_h
#define XQuerytoLR_h

#include <string>
#include <vector>
#include "common/base.h"
#include "tr/executor/por2qep/scheme_tree.h"

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

StringVector parse_batch(QueryType type, const char *batch);

#endif
