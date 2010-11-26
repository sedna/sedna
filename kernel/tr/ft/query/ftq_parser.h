/*
 * File:  ftq_parser.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FTQ_PARSER_H
#define _FTQ_PARSER_H

#include "tr/ft/query/ft_query.h"
#include "tr/ft/ft_cache.h"

FtQuery *ft_parse_query(str_cursor *cur, ftc_index_t idx);

#endif