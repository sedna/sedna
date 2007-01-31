/*
 * File:  XQuerytoLR.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef XQuerytoLR_h
#define XQuerytoLR_h

#include <string>
#include <vector>
#include "common/base.h"
#include "tr/executor/por2qep/scheme_tree.h"
#include "tr/xqp/SORAST.h"


struct script_struct
{
  scheme_list* stmnt;  
};


//typedef std::vector<script_struct> StmntsArray;

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



SORAST* XQuerytoIR(const char *);

StringVector parse_batch(QueryType type, const char* batch);

StmntsArray *transform_stmnt2pr (std::string stmnt,
                                 QueryType type,
                                 StmntsArray* (*transformer) (std::string, QueryType));

//StmntsArray operator+ (StmntsArray v1, StmntsArray v2);
#endif

