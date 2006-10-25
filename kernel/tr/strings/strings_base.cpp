/*
 * File:  strings_base.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "strings_base.h"
#include "exec_output.h"


void writextext_cb(const char *str, int len, void *p)
{
	((se_ostream*)p)->writextext((char*)str, len);
}
