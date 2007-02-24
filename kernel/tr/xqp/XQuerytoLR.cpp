/*
 * File:  XQuerytoLR.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/xqp/XQuerytokens.h"
#include "tr/xqp/XQueryDLGLexer.h"
#include "tr/xqp/ANTLRToken.h"
#include "tr/xqp/XQueryParser.h"
#include "tr/xqp/XQueryTreeParser.h"
#include "ATokPtr.h"
#include "ASTBase.h"
#include "tr/xqp/AST.h"
#include "tr/xqp/XQuerytoLR.h"
#include "common/base.h"
#include "common/errdbg/d_printf.h"
#include "tr/tr_utils.h"
#include <iostream>
#include <string>
#include <vector>

using namespace std;

EXTERN_DECLARE_TIME_VARS

XQueryDLGLexer* my_lexer;
bool is_preserve_boundary_space;

SORAST* XQuerytoIR(const char* xquery){

  ASTBase *root = NULL, *_root=NULL;

  SORASTBase *result = NULL;

  string LispRepresent ="";

  int sig;

  is_preserve_boundary_space = false;

  DLGStringInput in(xquery);
  
  XQueryDLGLexer scan(&in);

  ANTLRTokenBuffer pipe(&scan);

  ANTLRTokenPtr aToken = se_new ANTLRToken;

  scan.setToken(mytoken(aToken));

  XQueryParser parser(&pipe);

  XQueryTreeParser  tparser;

  parser.init();

  my_lexer = &scan;
  parser.script(&root, &sig, &scan);
  // parser.query(&root, &sig);
  _root=root;

  tparser.script((SORASTBase **)&_root, &result);

  return (SORAST*)result;
}


StringVector parse_batch(QueryType type, const char* batch1)
{

     char * batch = NULL;  
     u_ftime(&t1_parser);

 try{
     GET_TIME(&t1_parser);

     SORAST* t, *it;
     StringVector array;


    batch = encoding_processing(batch1);
//  getchar();

//     malloc_ast_vector();

     if (type == TL_XQuery)
     {
        t = XQuerytoIR(batch);

        for (it = (SORAST*)(t->down()); it != NULL; it = (SORAST*)(it->right()))
        {
           array.push_back(string(it->getText()));  
//           printf("query: %s\n", it->getText());
        }


     }
     else
     {
        array.push_back(string(batch));
     }

//     free_ast_vector();
     GET_TIME(&t2_parser);

     ADD_TIME(t_total_parser, t1_parser, t2_parser);

     free(batch);
              
     return array;
 } catch(SednaUserException &e) {
//     free_ast_vector();
     GET_TIME(&t2_parser);
     ADD_TIME(t_total_parser, t1_parser, t2_parser);

     if (batch != NULL)
         free(batch);

     throw;
 }
}

/*
StmntsArray operator+ (StmntsArray v1, StmntsArray v2)
{
  StmntsArray res;

  res.reserve(v1.size() + v2.size());

  StmntsArray::const_iterator it;
  for (it = v1.begin(); it != v1.end(); it++) res.push_back(*it);
  for (it = v2.begin(); it != v2.end(); it++) res.push_back(*it);
  
  return res;  
}
*/
/*

StmntsArray* transform_stmnt2pr (string text_stmnt,
                                  QueryType type,
                                  StmntsArray* (*transformer) (string, QueryType))
{
  SORAST *pr;
  StmntsArray *stmnt;

  switch (type)
  {
    case TL_XQuery:
    {
        pr =  XQuerytoIR(text_stmnt.c_str());    
 
        stmnt = se_new StmntsArray();
        stmnt = transformer(((SORAST*)(pr->down()))->getText(), type); 
        
        break;
    }
    case TL_POR:
    {
        stmnt = se_new StmntsArray();
        stmnt = transformer(text_stmnt, type);
        break;
    }
    default : throw PrepareQueryException("#????: Unexpected type of the query");
  }
    
  return stmnt;
}
*/