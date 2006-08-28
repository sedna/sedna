/*
 * File:  XQuerytoLR.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "XQuerytokens.h"
#include "XQueryDLGLexer.h"
#include "ANTLRToken.h"
#include "XQueryParser.h"
#include "XQueryTreeParser.h"
#include "ATokPtr.h"
#include "ASTBase.h"
#include "AST.h"
#include "XQuerytoLR.h"
#include "base.h"
#include "d_printf.h"
#include "tr_utils.h"
#include <iostream>
#include <string>
#include <vector>

using namespace std;

EXTERN_DECLARE_TIME_VARS


SORAST* XQuerytoIR(const char* xquery){

  ASTBase *root = NULL, *_root=NULL;

  SORASTBase *result = NULL;

  string LispRepresent ="";

  int sig;

  DLGStringInput in(xquery);
  
  XQueryDLGLexer scan(&in);

  ANTLRTokenBuffer pipe(&scan);

  ANTLRTokenPtr aToken = new ANTLRToken;

  scan.setToken(mytoken(aToken));

  XQueryParser parser(&pipe);

  XQueryTreeParser  tparser;

  parser.init();


  parser.script(&root, &sig, &scan);
  // parser.query(&root, &sig);
  _root=root;

  tparser.script((SORASTBase **)&_root, &result);

  return (SORAST*)result;
}


StringVector parse_batch(QueryType type, const char* batch)
{

  
     u_ftime(&t1_parser);

 try{
     GET_TIME(&t1_parser);

     SORAST* t, *it;
     StringVector array;


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


     return array;
 } catch(SednaUserException &e) {
//     free_ast_vector();
     GET_TIME(&t2_parser);
     ADD_TIME(t_total_parser, t1_parser, t2_parser);

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
 
        stmnt = new StmntsArray();
        stmnt = transformer(((SORAST*)(pr->down()))->getText(), type); 
        
        break;
    }
    case TL_POR:
    {
        stmnt = new StmntsArray();
        stmnt = transformer(text_stmnt, type);
        break;
    }
    default : throw PrepareQueryException("#????: Unexpected type of the query");
  }
    
  return stmnt;
}
*/