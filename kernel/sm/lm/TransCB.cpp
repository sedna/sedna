#include <iostream>
#include "base.h"
#include "TransCB.h"
using namespace std;
/*****************************************************************************
               TransCB class implementation
*****************************************************************************/

TransCB::TransCB(transaction_id _tr_id_)
{
#ifdef LOCK_MGR_ON
  locks = NULL;
  wait = NULL;
  cycle = NULL;
  tr_id = _tr_id_;
#endif
}

void TransCB::print()
{
#ifdef LOCK_MGR_ON
  d_printf2("transaction_id=%d\n", tr_id);
  
  lock_request *request;

  for(request = locks; request != NULL; request = request->tran_next)
  {
    request->print();
    d_printf1("\n");
  }
#endif
}