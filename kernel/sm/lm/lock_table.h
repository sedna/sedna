#ifndef _LOCK_TABLE_H_
#define _LOCK_TABLE_H_


#include <string>
#include <map>
#include "base.h"
#include "lm_base.h"
#include "usem.h"
#include "exceptions.h"
#include "TransCB.h"


bool lock_compat(lock_mode, lock_mode);
lock_mode lock_max(lock_mode, lock_mode);

class lock_head;
class lock_request;
class TransCB;

class lock_head
{
public:
  resource_id r_id; //the id of the locking resource 
  lock_request* queue; // the queue of requests for this lock
  lock_mode granted_mode; // the mode of the granted group
  bool waiting;// flag indicates nonempty wait group

  lock_head(resource_id, lock_mode);
  ~lock_head();
  void print();
  
};


class lock_request
{
public:
  lock_request* queue; //pointer to next request in lock queue
  lock_head* head;//pointer back to head of the queue
  lock_status status; //granted, waiting, converting, denied
  lock_mode mode; //mode requested
  lock_mode convert_mode; // if in convert wait, mode desired
  int count; //count of the number of times lock was locked
  lock_class class_; //class in which lock is held (lock duration)
  USemaphore process_xsem;//sem to up when lock is granted
  TransCB* tran; //pointer to transaction record
  lock_request* tran_prev;// previous lock request in transaction list
  lock_request* tran_next;// next lock request in transaction list

  lock_request(transaction_id, session_id, lock_head*, lock_mode, lock_class);  
  ~lock_request();
  void print();
};



typedef std::pair<std::string, lock_head*> lock_pair;
typedef std::map<std::string, lock_head*>::iterator lock_iter;

class lock_table
{
private:
   USemaphore xsem; //exclusive semaphore protecting hash table
   std::map<std::string, lock_head*> _l_table_;
   // the auxilary functions
   lock_head* find_lock(resource_id&, bool sync = true);

public:
   void init_lock_table();
   void release_lock_table();
   // the main functions
   lock_reply lock(transaction_id,
                   session_id,
                   resource_id,
                   lock_mode,
                   lock_class,
                   long,
                   bool sync = true);

   lock_reply unlock(transaction_id, resource_id, bool sync = true );
   lock_reply release_tr_locks(transaction_id, bool sync = true);
   void print(bool sync = true);

   bool deadlock(bool sync);
   void visit(TransCB* me);

   inline void down_sem(bool sync)
   {
#ifdef LOCK_MGR_ON
      if (sync)
         if ( USemaphoreDown(xsem) != 0 )
            throw USER_EXCEPTION2(SE4015, "SEDNA_LOCK_MANAGER_SEM");
#endif
   }

   inline void up_sem(bool sync)
   {
#ifdef LOCK_MGR_ON
      if (sync)
         if ( USemaphoreUp(xsem) != 0)
            throw USER_EXCEPTION2(SE4014, "SEDNA_LOCK_MANAGER_SEM");
#endif
   }
};

#endif