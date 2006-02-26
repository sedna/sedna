#ifndef _TRANS_CB_H_
#define _TRANS_CB_H_

#include "base.h"
#include "lock_table.h"
#include "lm_base.h"

class lock_request;

class TransCB
{
public:
  lock_request* locks; //anchor of transaction lock list
  lock_request* wait; //lock waited for by this transaction (or NULL)  
  TransCB* cycle; //used by deadlock detector
  transaction_id tr_id; // the identifier of transaction
  
  TransCB(transaction_id _tr_id_);
  void print();
  //bool operator== (TransCB &);
  //bool operator!= (TransCB &);
};

#endif
