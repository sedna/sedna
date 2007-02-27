/*
 * File:  config_utils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CONFIG_UTILS_H_
#define _CONFIG_UTILS_H_

#include <string>
#include "common/config.h"
#include "../expat/expat.h"

std::string erase_ws(std::string str);

void fulfill_common_system_parameters_from_config_file_and_command_line(gov_config_struct* cfg, const char* gov_proc_buf);

void fullfill_config_parameters(gov_config_struct* cfg);

void fulfill_sm_parameters_from_config_files(gov_config_struct* cfg);

void parse_sm_config_file(gov_db_struct* db_cfg, std::string cfg_text);

void parse_config_file(gov_config_struct* cfg, std::string& cfg_text);

void startElement_gov_cfg(void *cfg, const char *name, const char **atts);

void endElement_gov_cfg(void *cfg, const char *name);

void characterData_gov_cfg(void *cfg, const XML_Char *s, int len);

void startElement_sm_cfg(void *cnt, const char *name, const char **atts);

void endElement_sm_cfg(void *cfg, const char *name);

void characterData_sm_cfg(void *cnt, const XML_Char *s, int len);

#endif
