/*
 * File:  int_sp.h
 * Copyright (C) 2012 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _INT_SP_H
#define _INT_SP_H

#define MAX_TICKET_SIZE 1024
#define MAX_ERROR_LENGTH 1024
#define MAX_DB_NAME 1024
#define MAX_XML_PARAMS 10000

#include <exception>

namespace proto {
    class InvalidMessage : public std::exception
    {
        int awaiting;
        int received;

    public:
        InvalidMessage(int _awaiting, int _received) : awaiting(_awaiting), received(_received) {};
    };
}

/* Attenttion! You should be very careful:
 * If you want to define new protocol constants for internal Sedna processes 
 * communications you should do it there. If you want to add some constants for
 * communication with clients you should do it in driver/c/sp_defs.h
 * You MUST watch for se_sp_instructions and se_int_sp_instructions do not
 * intersect by numeric values, otherwise you will debug it very-very long
 */

/* internal messages are prefixed se_int_* */

enum se_int_sp_instructions {
/* TODO: sort signals in the order of they appear */
    se_int_Handshake                               = 123,

    se_int_ConnectProcess                          = 180,

//    se_int_ReceiveSocket                           = 164,

    se_int_StartDatabaseInternal                   = 181,
    se_int_CreateDatabaseInternal                  = 183,

    se_int_RegisterSession                         = 184,
    se_int_UnregisterSession                       = 185,

    se_int_SessionParametersInternal               = 186,

    se_int_ProcessReady                            = 187,
    se_int_RegistrationFailed                      = 188,

    se_int_SoftError                               = 192,
    se_int_SuccessefulStop                         = 193,
    
//    se_UnixSocketReady                             = 163,

//    se_SocketReceivedOK                            = 165,
//    se_TrnRegisterFailedNotRunningOrSpecialMode    = 171,
//    se_TrnRegisterFailedMaxSessLimit               = 172
};

#endif /* _INT_SP_H */
