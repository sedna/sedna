/*
 * File:  parserutils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include <stdlib.h>
#include <string.h>
#include "tr/strings/utf8.h"
#include "tr/xqp/parserutils.h"
#include "tr/executor/base/xs_names.h"
#include "tr/executor/base/xs_helper.h"

using namespace std;

string scheme_prepare_string(const char* lex_text)
{
    string lex = lex_text;
    string::size_type pos = 0;
  
    /// I.  Replace back-slashes for Scheme part
    escape_back_slash(lex);
  
    /// II. Further processing
    if (lex.size() >= 3)
    {
        bool double_quotes = (lex[0] == '"');
        const char* find_text = double_quotes ? "\"\"" : "''"; 
        const char* repl_text = double_quotes ? "\\\"" : "\\'";
        string text = lex.substr(1, lex.size()-2);
         
        while(true)
        {
            pos = text.find(find_text, pos);
            if (pos == string::npos) break;
            text.replace(pos, 2, repl_text);
            pos += 2;
        }
         
        /// III. Escape '"' for Scheme part
        if(!double_quotes)
            escape_double_quote(text, true);
        lex = string("\"") + text + "\"";
    }
    else
    {
        lex[0] = '"';
        lex[lex.size()-1] = '"';
    }
  
    return lex;
}

/// Escape double quotes symbol for Scheme part
/// escaped_escape determines if we should escape double
/// quote followed after back slash.
void escape_double_quote(string& text, bool escaped_escape)
{
    string::size_type pos = 0;
    const char* find_text = "\"";
    const char* repl_text = "\\\"";

    while(true)
    {
        pos = text.find(find_text, pos);
        if (pos == string::npos) break;

        if (pos == 0 || text[pos-1] != '\\' || escaped_escape)
        {
           text.replace(pos, 1, repl_text);
        }
        pos += 2;
    }
}

/// Escape back slash symbol for Scheme part
void escape_back_slash(string& text)
{
    string::size_type pos = 0;
    const char* find_text = "\\";
    const char* repl_text = "\\\\";

    while(true)
    {
        pos = text.find(find_text, pos);
        if (pos == string::npos) break;
        text.replace(pos, 1, repl_text);
        pos += 2;
    }
}

void replace_entity(char* lex_text, const char* find_ent, const char* replc_ent)
{
  string lex = lex_text;
  string::size_type pos = 0;
  int find_len = strlen(find_ent);

  while(true)
  {
    pos = lex.find(find_ent, pos);
    if (pos == string::npos) break;

    lex.replace(pos, find_len, replc_ent);
    pos++;     
  }

  strcpy(lex_text, lex.c_str());
}


string replace_charref(const char* lex_text)
{
   string text = string(lex_text);
   string::size_type posb = 0, pose = 0;
   string char_ref;
   int char_code;
   string str_char_code;

   /// I.  Replace hex char ref
   while(true)
   {
      posb = text.find("&#x", posb);
      if (posb == string::npos) break;

      pose = text.find(";", posb);
      if (pose == string::npos) break;

      char_ref =  text.substr(posb+3, pose - (posb+3));
      char_code = strtol(char_ref.c_str(), NULL, 16);

      if (!isXML10Valid(char_code)) throw USER_EXCEPTION(XQST0090);

      str_char_code = string(utf8_encode_char(char_code));

      text = text.replace(posb, (pose-posb)+1, str_char_code.c_str(), 0, str_char_code.size());
   }

   posb=0; 
   pose=0;

   /// II. Replace dec char ref
   while(true)
   {
      posb = text.find("&#", posb);
      if (posb == string::npos) break;

      pose = text.find(";", posb);
      if (pose == string::npos) break;

      char_ref =  text.substr(posb+2, pose - (posb+2));
      char_code = atoi(char_ref.c_str());

      if (!isXML10Valid(char_code)) throw USER_EXCEPTION(XQST0090);

      str_char_code = string(utf8_encode_char(char_code));

      text = text.replace(posb, (pose-posb)+1, str_char_code.c_str(), 0, str_char_code.size());
   }

   return text;
}

void parse_processing_instruction(const string& raw, string& name, string& content, int line)
{
    int raw_len = raw.size();
    name.clear();
    content.clear();
    int i = 0;            ///position in raw content
    unsigned char byte;

    for(i = 0; i < raw_len; i++)
    {
        byte = (unsigned char) raw[i];
        if(!IS_WHITESPACE(byte)) name += byte;
        else break;
    }

    if(!check_constraints_for_xs_NCName(name.c_str()))
        throw USER_EXCEPTION2(XPST0003, (string("Empty or invalid processing instruction name: '") + name + "', line: " + int2string(line)).c_str());

    if (3 == name.size())
    {
        if ( (name[0] == 'x' || name[0] == 'X') &&
	         (name[1] == 'm' || name[1] == 'M') &&
	         (name[2] == 'l' || name[2] == 'L'))
        throw USER_EXCEPTION2(XPST0003, (string("Reserved processing instruction name: '") + name + "', line: " + int2string(line)).c_str());
    }

    while(i < raw_len) 
    { 
        byte = (unsigned char) raw[i]; 
        if(!IS_WHITESPACE(byte)) break;
        i++;
    }

    if(i < raw_len)
    { 
        content += "\"";
        content += raw.substr(i);
        content += "\"";
    }
}

/*
enum pragma_states {START_WS, P_PREFIX, P_LOC_NAME, NEXT_WS, PRCONTENT};

void* extract_pragma_content(char* text, char* qname, char* pr_content)
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

char* encoding_processing(const char *query)
{
    int query_len = strlen(query);
    if (query_len > 2 && 
        (unsigned char)query[0] == 0xef && 
        (unsigned char)query[1] == 0xbb && 
        (unsigned char)query[2] == 0xbf) {
    	query += 3;
    	query_len -= 3;
    }

    if (utf8_valid(query, query_len) >= 0)
        throw USER_EXCEPTION(SE4082);
    
    char* x = (char*)malloc(strlen(query) + 1);
    strcpy(x, query);
    return x;
}

