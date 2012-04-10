#ifndef LISTENER_STATES_H
#define LISTENER_STATES_H

enum listener_progress_t {
      receive_new_message,
      in_progress
};

   
enum sm_state_t {
      awaiting_sm_registering
};
    
enum trn_state_t {
      conscript_reporting,
      waiting_orders,
      yes_commander
};


#endif /* LISTENER_STATES_H */