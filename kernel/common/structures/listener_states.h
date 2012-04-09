#ifndef LISTENER_STATES_H
#define LISTENER_STATES_H

enum listener_progress_t {
      receive_new_message,
      in_progress
};

enum client_state_t {
      client_awaiting_parameters,
      client_awaiting_auth,
      client_awaiting_sm_and_trn,
      client_awaiting_trn_launch
};
    
enum sm_state_t {
      awaiting_sm_registering
};
    
enum trn_state_t {
      conscript_reporting,
      waiting_orders,
      yes_commander
};

enum cdb_state_t {
      cdb_awaiting_parameters,
      cdb_awaiting_auth,
      cdb_awaiting_db_options,
      cdb_awaiting_sm_start
};

#endif /* LISTENER_STATES_H */