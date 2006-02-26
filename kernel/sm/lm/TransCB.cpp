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
  cout << "transaction_id=" << tr_id << endl;
  
  lock_request *request;

  for(request = locks; request != NULL; request = request->tran_next)
  {
    request->print();
    cout << endl;
  }
#endif
}