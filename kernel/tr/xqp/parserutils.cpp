/*
 * File:  parserutils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <string>
#include <stdlib.h>
#include <string.h>
#include "utf8.h"

using namespace std;

bool check_char_wellformedness(int c)
{
  if (c == 0x9 || c == 0xA || (c >= 0x20 && c <= 0xd7ff) || (c >= 0xe000 && c <= 0xfffd) || (c >= 0x1000 && c <= 0x10ffff))
     return true;
  else return false;
}

string erase_doublequot(char* lex_text)
{
  string lex = lex_text;
  if (lex.size() >= 3)
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
    lex[lex.size()-1] = '\"';
  }

  return  lex;

//  printf("lext_text=%s\n", lex_text);

}

string escape_quot(string text)
{
    //replase " with \" for Scheme part
    int pos;
    string find_text = "\"";
    string repl_text = "\\\"";

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

    return text;
}

string replace_entity(char* lex_text, string find_ent, string replc_ent)
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

  return lex;
}


string replace_charref(char* lex_text)
{
   string text = std::string(lex_text);
   string::size_type posb = 0, pose = 0;
   string char_ref;
   int char_code;
   string str_char_code;

   //replace hex char ref
   for (;;)
   {
      posb = text.find("&#x", posb);
      if (posb == string::npos) break;

      pose = text.find(";", posb);
      if (pose == string::npos) break;

      char_ref =  text.substr(posb+3, pose - (posb+3));

      char_code = strtol(char_ref.c_str(), NULL, 16);

      if (!check_char_wellformedness(char_code))
	throw USER_EXCEPTION(XQST0090);

//      printf("char_code=%d\n", char_code);
      str_char_code = string(utf8_encode_char(char_code));

      text = text.replace(posb, (pose-posb)+1, str_char_code.c_str(), 0, str_char_code.size());
   }

   posb=0; 
   pose=0;

   //replace dec char ref
   for (;;)
   {
      posb = text.find("&#", posb);
      if (posb == string::npos) break;

      pose = text.find(";", posb);
      if (pose == string::npos) break;

      char_ref =  text.substr(posb+2, pose - (posb+2));

      char_code = atoi(char_ref.c_str());

      if (!check_char_wellformedness(char_code))
	throw USER_EXCEPTION(XQST0090);

//      printf("char_code=%d\n", char_code);
      str_char_code = string(utf8_encode_char(char_code));


      text = text.replace(posb, (pose-posb)+1, str_char_code.c_str(), 0, str_char_code.size());
   }

   return text;
}

/*
enum pragma_states {START_WS, P_PREFIX, P_LOC_NAME, NEXT_WS, PRCONTENT};

void * extract_pragma_content(char* text, char* qname, char* pr_content)
{
  state = START_WS;
  string loc_name, prefix;

  for (int i=0; i<strlen(text); i++)
  {
     if (state == START_WS)
     {
         if (text[i] == ' ' || text[i] == '\t' || text[i] == '\r' || text[i] == '\n') continue;
         else state = P_PREFIX;
     }

     if (state == P_PREFIX)
     {
        if ((text[i] >= 'a' && text[i] <= 'z') || (text[i] >= 'A' && text[i] <= 'Z')  || (text[i] >= '0' && text[i] <= '9') ||
	     text[i] == '_' || text[i] == '-' || text[i] == '.')
        {
             prefix +=text[i];
             continue;
        }
        else
        if (text[i] == ':')
        {
             state = P_LOC_NAME;
             continue;
        }
        else
        if (text[i] == )
     }
  }
}
*/

