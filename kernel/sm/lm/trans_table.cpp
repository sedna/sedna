/*
 * File:  trans_table.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <map>
#include <iostream>
#include "base.h"
#include "trans_table.h"
#include "TransCB.h"
#include "usem.h"
#include "d_printf.h"

using namespace std;


/******************************************************************************
               tr_lock_head class implementation 
******************************************************************************/

tr_lock_head::tr_lock_head()
{
#ifdef LOCK_MGR_ON
#endif
}


tr_lock_head::tr_lock_head(TransCB *trCB)
{
#ifdef LOCK_MGR_ON
  tran = trCB;
#endif
}

tr_lock_head::~tr_lock_head()
{
#ifdef LOCK_MGR_ON
  delete tran;
#endif
}


void tr_lock_head::print()
{
#ifdef LOCK_MGR_ON
  tran->print();
#endif
}

/******************************************************************************
               trans_table class implementation 
******************************************************************************/


tr_lock_head* trans_table::find_tr_lock_head(transaction_id tr_id)
{
#ifdef LOCK_MGR_ON
   int res;
   tr_lock_head* ret;


   Trans_It it = _tr_table_.find(tr_id);

   if ( it == _tr_table_.end())
      ret = NULL;
   else
      ret = it->second;

   return ret;
#else
   return NULL;
#endif
}


int trans_table::insert_tr_lock_head(tr_pair p)
{
#ifdef LOCK_MGR_ON
   int res;
   int ret;

   pair<Trans_It, bool > pr;

   pr = _tr_table_.insert(p);

   if (pr.second == true)
      ret = 0;
   else 
      ret = 1;

   return ret;
#else
   return 0;
#endif

}

int trans_table::remove_tr(transaction_id tr_id)
{
#ifdef LOCK_MGR_ON
   int res;
   int ret;

   ret =_tr_table_.erase(tr_id);

   return ret;
#else
   return 0;
#endif
}

void trans_table::print()
{
#ifdef LOCK_MGR_ON
  Trans_It it;

  d_printf1("============== Trans Table=================\n");
  for (it = _tr_table_.begin(); it != _tr_table_.end(); it++)
  {
    (*it).second->print();
    d_printf1("\n");
  }

  d_printf1("============================================\n");
#endif
}
