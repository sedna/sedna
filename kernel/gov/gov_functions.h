/*
 * File:  gov_functions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _GOV_FUNCTIONS_H_
#define _GOV_FUNCTIONS_H_

// void clean_resources(gov_config_struct& cfg, bool background_off_from_background_on);


void check_data_folder_existence();

void log_out_system_information();

void RenameLastSoftFaultDir();

void mergeCommandLineAndConfig();

#endif /*  _GOV_FUNCTIONS_H_ */

