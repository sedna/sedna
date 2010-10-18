/*
 * File:  strings_base.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/strings/strings_base.h"
#include "tr/crmutils/exec_output.h"


void writextext_cb(const char *str, int len, void *p)
{
	((se_ostream*)p)->write((char*)str, len);
}
