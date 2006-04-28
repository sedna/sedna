/*
 * File:  AST.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef AST_h
#define AST_h

#include "sedna.h"
#include <string>
#include "XQuerytokens.h"
#include "ATokPtr.h"
#include "AToken.h"
#include "ANTLRToken.h"
#include "ASTBase.h"
#include "ASTNodeTypes.h"



//void malloc_ast_vector();

//void free_ast_vector();

//void realloc_ast_vector();

class AST : public ASTBase {

protected:
  //string text;
  //ANTLRTokenType type_;
  ANTLRToken* token;
public:

// called when #[string, tokentype] is seen in an action
 AST(char *, ANTLRTokenType);

// called when #[string, ASTNodeType] is seen in an action
 AST(char *, ASTNodeType);

// called when #[string, ASTNodeType] is seen in an action
 AST(std::string, ASTNodeType);
// called when #[ASTNodeType] is seen in action
 AST(ASTNodeType);

// constructor called by parser for token references in grammar
// AST(ANTLRTokenPtr);

// caled when #[char *] is sen in an action
 AST(const char *);

// caled when #[string]  is seen in an action
 AST(std::string);

// empty constructor
 AST();

// copy constructor
 AST(const AST&);

 ~AST();

// void* operator new (size_t size);
// void operator delete (void *p);

// shallow copy (used by sorcerer in transform mode)
 virtual PCCTS_AST *shallowCopy();

// return type's value
 int type ();

// retrn AST text value
 char *getText();

  // set new text of the AST node
 void setText(std::string);

// define what happens at a node when preorder() is called
 virtual void preorder_action(void* = NULL);

// define what happens at parent node during tree traversal
 virtual void preorder_before_action(void* = NULL); 

// define .what happens at children nodes during tree traversal
 virtual void preorder_after_action(void* = NULL); 

};

#endif


