/*
 * File:  gov_functions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _GOV_FUNCTIONS_H_
#define _GOV_FUNCTIONS_H_

void clean_resources(gov_config_struct& cfg, bool background_off_from_background_on);
bool is_first_start_of_gov(int ping_port);
void RenameLastSoftFaultDir(const char* SEDNA_DATA);
#endif

