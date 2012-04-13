/*
 * File:  sp_defs.h
 * Copyright (C) 2012 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _INT_SP_H
#define _INT_SP_H


/* Attenttion! You should be very careful:
 * If you want to define new protocol constants for internal Sedna processes 
 * communications you should do it there. If you want to add some constants for
 * communication with clients you should do it in driver/c/sp_defs.h
 * You MUST watch for se_sp_instructions and se_int_sp_instructions do not
 * intersect by numeric values, otherwise you will debug it very-very long
 */

enum se_int_sp_instructions 
{
/* TODO: sort signals in the order of they appear */
  
    se_ConnectProcess                              = 180,
    se_ReceiveSocket                               = 164,
    se_Handshake                                   = 121,
    se_RegisterCDB                                 = 124,
    se_CdbRegisteringOK                            = 183,
    se_CdbRegisteringFailed                        = 184,
/*  
    se_RegisterNewSession                          = 121,
    se_UnRegisterSession                           = 122,
    se_RegisterDB                                  = 123,
    
    se_UnRegisterDB                                = 125,
    se_TrnRegisterOK                               = 161,
    se_TrnRegisterOKFirstTransaction               = 162,
    se_UnixSocketReady                             = 163,
    se_ReceiveSocket                               = 164,
    se_SocketReceivedOK                            = 165,
    se_TrnRegisterFailedNotRunningOrSpecialMode    = 171,
    se_TrnRegisterFailedMaxSessLimit               = 172,
    se_SMRegisteringOK                             = 181,
    se_SMRegisteringFailed                         = 182,
    
    
    */
} se_int_sp_instructions;

#endif /* _INT_SP_H */
