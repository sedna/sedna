/*
 * File:  parserutils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <string>
#include <stdlib.h>

using namespace std;

void erase_doublequot(char* lex_text)
{
  string lex = lex_text;
  if (lex.size() > 3)
  {
    string find_text = string(1, lex[0]) + string(1, lex[0]);
    string repl_text = string("\\") + string(1, lex[0]);
    string text = lex.substr(1, lex.size()-2);//erase first and last characters
    string::size_type pos;
    for(;;)
    {
      pos = text.find(find_text);
      if (pos == string::npos) break;

      text = text.replace(pos, 2, repl_text);
    }

    //replase " with \" for Scheme part
    find_text = "\"";
    repl_text = "\\\"";

    pos = 0;

    for(;;)
    {
      pos = text.find(find_text, pos);
      if (pos == string::npos) break;

      if (pos == 0)
         text = text.replace(pos, 1, repl_text);
      else
      {
         if (text[pos-1] != '\\')
             text = text.replace(pos, 1, repl_text);
      }

    

      pos +=find_text.size() + 1;
    }


    lex = string("\"") + text + "\"";
  }
  else
  {
    lex[0] = '\"';
    if (lex[1] == '\"') 
    {
       lex[1] = '\\';
       lex[2] = '\"';
       lex[3] = '\"';
    }
    else
       lex[lex.size()-1] = '\"';
  }

  strcpy(lex_text, lex.c_str());

//  printf("lext_text=%s\n", lex_text);

}

void replace_entity(char* lex_text, string find_ent, string replc_ent)
{
  string lex = lex_text;
  string::size_type pos;

  for(;;)
  {
    pos = lex.find(find_ent);
    if (pos == string::npos) break;

    lex = lex.replace(pos, find_ent.size(), replc_ent);     
  }

  strcpy(lex_text, lex.c_str());
}


