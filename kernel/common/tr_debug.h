/*
 * File:  tr_debug.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TR_DEBUG_H_
#define _TR_DEBUG_H_

void INIT_DEBUG_LOG(const char* db_name);

void INIT_DEBUG_LOG2(const char* db_name);

void CREATE_DEBUG_LOG(const char* db_name);

void RELEASE_DEBUG_LOG();

void RELEASE_DEBUG_LOG2();

void WRITE_DEBUG_LOG(const char* msg);

#endif

