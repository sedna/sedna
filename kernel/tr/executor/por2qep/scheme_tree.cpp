/*
 * File:  scheme_tree.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <iostream>

#include "common/sedna.h"
#include "tr/executor/por2qep/scheme_tree.h"
#include "common/errdbg/d_printf.h"

#define LEX_EOF			0
#define LEX_LBR			1
#define LEX_RBR			2
#define LEX_SYMBOL		3
#define LEX_STRING		4
#define LEX_CHAR		5
#define LEX_NUMBER		6
#define LEX_BOOL		7

using namespace std;

struct token
{
    token(int _type) { type = _type; }
    token(int _type, string _text) { type = _type; text = _text; }
    int type;
    string text;
};

typedef vector<token> tokens;

class Tokenizer
{
public:
    Tokenizer(const char *_str) { if (_str == NULL) throw USER_EXCEPTION2(SE1005, "Tokenizer bad string error (0)"); str = _str; pos = 0; }
    token get_token();

private:
    const char *str;
    int pos;
    char getch() { if (str[pos] == '\0') return '\0'; else return str[pos++]; } 
    void undoget() { if (pos == 0) return; pos--; }

    token get_bool();
    token get_string();
    token get_number(char c);
    token get_char();
    token get_symbol(char c);
};



token Tokenizer::get_token()
{
    char c = getch();

    switch(c) {
        case '\0' : return token(LEX_EOF);
        case '('  : return token(LEX_LBR);
        case ')'  : return token(LEX_RBR);
        case '#'  : return get_bool();
        case '\"' : return get_string();
        case ' '  : return get_token();
        case '\n' : return get_token();
        case '\r' : return get_token();
        case '\'' : return get_char();
        default:
            if (('0' <= c && c <= '9') || c == '-' || c == '+') return get_number(c);
            else if (isalpha(c) || c == '<' || c == '=' || c == '>' || c == '=' || c == '_' || c == '$' || c == '%' || c == '!') return get_symbol(c);
            else {
                U_ASSERT(false);
                throw USER_EXCEPTION2(SE1005, "Tokenizer bad string error (1)");
            }
    };
}

token Tokenizer::get_bool()
{
    char c = getch();
    if (c == 't') return token(LEX_BOOL, "#t");
    else if (c == 'f') return token (LEX_BOOL, "#f");
    else throw USER_EXCEPTION2(SE1005, "Tokenizer bad string error (2)");
}

token Tokenizer::get_string()
{
    string str;
    char c = getch();
    while (c != '\"' && c != '\0')
    {
        if (c == '\\')
        {
            c = getch();
            if (c == '\\') str += "\\";
            else if (c == '\"') str += "\"";
            else if (c == 'n') str += "\n";
            else if (c == 't') str += "\t";
            else if (c == 'r') str += "\r";
            else { /*d_printf3("symbol !%c!, position %d\n", c, pos);*/ throw USER_EXCEPTION2(SE1005, "Tokenizer bad string error (3)"); }
        } else str += c;
        c = getch();
    }
    if (c == '\0') throw USER_EXCEPTION2(SE1005, "Tokenizer bad string error (4)");
    return token(LEX_STRING, str);
}

token Tokenizer::get_number(char ch)
{
    char c = getch();
    string str;
    str += ch;
    while ('0' <= c && c <= '9')
    {
        str += c;
        c = getch();
    }
    if (c == '.') 
    {
        str += c;
        c = getch();
        while ('0' <= c && c <= '9')
        {
            str += c;
            c = getch();
        }
        undoget();
        return token(LEX_NUMBER, str);
    }
    else 
    {
        undoget();
        return token(LEX_NUMBER, str);
    }
}

token Tokenizer::get_char()
{
    char c = getch();
    if (getch() != '\'') throw USER_EXCEPTION2(SE1005, "Tokenizer bad string error (5)");
    string str;
    str += c;
    return token(LEX_CHAR, str);
}

token Tokenizer::get_symbol(char ch)
{
    char c = getch();
    string str;
    str += ch;
    while (isalpha(c) || isdigit(c) || c == '@' || c == '-' || c == '_' || c == '%' || c == '!' || c == ':')
    {
        str += c;
        c = getch();
    }
    undoget();
    return token(LEX_SYMBOL, str);
}

scheme_list *make_node(Tokenizer &toks)
{
    scheme_list *root = new scheme_list();
    token tok = toks.get_token();
    while (tok.type != LEX_RBR)
    {
        scm_elem elem;
        if (tok.type == LEX_LBR)
        {
            elem.type = SCM_LIST;
            elem.internal.list = make_node(toks);
        }
        else if (tok.type == LEX_SYMBOL)
        {
            elem.type = SCM_SYMBOL;
            char *str = new char[tok.text.size() + 1];
            memset(str, 0, tok.text.size() + 1);
            strcpy(str, tok.text.c_str());
            elem.internal.symb = str;
        }
        else if (tok.type == LEX_STRING)
        {
            elem.type = SCM_STRING;
            char *str = new char[tok.text.size() + 1];
            memset(str, 0, tok.text.size() + 1);
            strcpy(str, tok.text.c_str());
            elem.internal.str = str;
        }
        else if (tok.type == LEX_CHAR)
        {
            elem.type = SCM_CHAR;
            elem.internal.ch = tok.text.c_str()[0];
        }
        else if (tok.type == LEX_NUMBER)
        {
            elem.type = SCM_NUMBER;
            char *str = new char[tok.text.size() + 1];
            memset(str, 0, tok.text.size() + 1);
            strcpy(str, tok.text.c_str());
            elem.internal.num = str;
        }
        else if (tok.type == LEX_BOOL)
        {
            elem.type = SCM_BOOL;
            if (tok.text.compare("#t") == 0) elem.internal.b = true;
            else if (tok.text.compare("#f") == 0) elem.internal.b = false;
            else throw USER_EXCEPTION2(SE1005, "Bad scheme list");
        }
        else throw USER_EXCEPTION2(SE1005, "Bad scheme list");
        root->push_back(elem);
        tok = toks.get_token();
    }
    return root;
}

scheme_list *make_tree_from_scheme_list(const char *str)
{
    Tokenizer toks = Tokenizer(str);

//    token tok = toks.get_token();
//    while (tok.type != LEX_EOF)
//    {
//        cout << tok.type << " " << tok.text << endl;
//        tok = toks.get_token();
//    }

    if (toks.get_token().type != LEX_LBR) throw USER_EXCEPTION2(SE1005, "Bad scheme list");
    return make_node(toks);
}

void delete_scheme_list(scheme_list *node)
{
    if (node == NULL) return;
    for (scheme_list::iterator i = node->begin(); i != node->end(); i++)
    {
        switch (i->type)
        {
            case SCM_BOOL:                                         break;
            case SCM_SYMBOL: delete[] i->internal.symb;            break;
            case SCM_CHAR:                                         break;
            case SCM_NUMBER: delete[] i->internal.num;             break;
            case SCM_STRING: delete[] i->internal.str;             break;
            case SCM_LIST:   delete_scheme_list(i->internal.list); break;
        }
    }
    delete node;
}

void walk_scheme_list(scheme_list *node, string sp)
{
    for (scheme_list::iterator i = node->begin(); i != node->end(); i++)
    {
        switch (i->type)
        {
            case SCM_BOOL:   d_printf2("%s", sp.c_str());
                             if (i->internal.b) d_printf1("true\n"); else d_printf1("false\n");   
                             break;
            case SCM_SYMBOL: d_printf3("%s%s\n", sp.c_str(), i->internal.symb);
                             break;
            case SCM_CHAR:   d_printf3("%s%c\n", sp.c_str(), i->internal.ch);
                             break;
            case SCM_NUMBER: d_printf3("%s%s\n", sp.c_str(), i->internal.num);
                             break;
            case SCM_STRING: d_printf3("%s%s\n", sp.c_str(), i->internal.str);
                             break;
            case SCM_LIST:   string new_sp = sp;
                             new_sp += "  ";
                             walk_scheme_list(i->internal.list, new_sp);
                             break;
        }
    }
}
