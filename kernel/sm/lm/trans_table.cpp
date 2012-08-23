/*
 * File:  trans_table.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <map>
#include <iostream>
#include "common/base.h"
#include "sm/lm/trans_table.h"
#include "sm/lm/TransCB.h"
#include "u/usem.h"
#include "common/errdbg/d_printf.h"

using namespace std;


/******************************************************************************
               tr_lock_head class implementation 
******************************************************************************/

tr_lock_head::tr_lock_head()
{
}


tr_lock_head::tr_lock_head(TransCB *trCB)
{
  tran = trCB;
}

tr_lock_head::~tr_lock_head()
{
  delete tran;
}


void tr_lock_head::print()
{
  tran->print();
}

/******************************************************************************
               trans_table class implementation 
******************************************************************************/


tr_lock_head* trans_table::find_tr_lock_head(transaction_id tr_id)
{
   tr_lock_head* ret;


   Trans_It it = _tr_table_.find(tr_id);

   if ( it == _tr_table_.end())
      ret = NULL;
   else
      ret = it->second;

   return ret;
}


int trans_table::insert_tr_lock_head(tr_pair p)
{
   int ret;

   pair<Trans_It, bool > pr;

   pr = _tr_table_.insert(p);

   if (pr.second == true)
      ret = 0;
   else 
      ret = 1;

   return ret;

}

size_t trans_table::remove_tr(transaction_id tr_id)
{
   return _tr_table_.erase(tr_id);
}

void trans_table::print()
{
  Trans_It it;

  d_printf1("============== Trans Table=================\n");
  for (it = _tr_table_.begin(); it != _tr_table_.end(); it++)
  {
    (*it).second->print();
    d_printf1("\n");
  }

  d_printf1("============================================\n");
}
