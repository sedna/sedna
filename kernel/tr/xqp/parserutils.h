/*
 * File:  parserutils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <string>

std::string erase_doublequot(char* lex_text);
std::string replace_entity(char* lex_text, std::string find_ent, std::string replc_ent);
std::string escape_quot(std::string text);


