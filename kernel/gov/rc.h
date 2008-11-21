/*
 * File:  rc.h
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __RC_H__
#define __RC_H__

#include <string>
#include <map>

#include "common/sedna.h"

/// Runtime configuration (i.e. (database, opened sessions) pairs)
typedef std::pair <std::string, int> rc_pair;  
typedef std::map  <std::string, int> rc_vector;
typedef std::map  <std::string, int> :: const_iterator rc_const_iterator;
typedef std::map  <std::string, int> :: iterator rc_iterator;


#endif /* __RC_H__ */
