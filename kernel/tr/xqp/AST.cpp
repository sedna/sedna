/*
 * File:  AST.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <stdlib.h>
#include "AST.h"
#include "types.h"
#include "d_printf.h"

using namespace std;

//int count = 0;

void* ast_massive;
int ast_massive_pos = 0;
int ast_massive_cells = 1024;

void malloc_ast_vector()
{
  //d_printf1("malloc vector\n");
  ast_massive = malloc(ast_massive_cells*sizeof(AST*));
}


void realloc_ast_vector()
{

  if (ast_massive_cells == 1024) ast_massive_cells = ast_massive_cells*10;
  else if (ast_massive_cells == 1024*10) ast_massive_cells = ast_massive_cells*10;
  else if (ast_massive_cells == 1024*100) ast_massive_cells = ast_massive_cells*10;
  else if (ast_massive_cells == 1024*1000) ast_massive_cells = ast_massive_cells*10;
  else if (ast_massive_cells == 1024*10000) ast_massive_cells = ast_massive_cells*10;
  else  ast_massive_cells = ast_massive_cells + 1024*10000;


  //d_printf2("realloc vector num=%d\n", ast_massive_cells);
  ast_massive = realloc(ast_massive, sizeof(AST*) * ast_massive_cells);

}


void free_ast_vector()
{
  int i=0;
  AST* pc;

  //d_printf1("free ast vector\n");
  
  for (i=0; i< ast_massive_pos; i++)
  {
    pc = (AST*)(*((unsigned int*)((char*)ast_massive + i*sizeof(AST*))));
    //d_printf2("pointer=%x freed\n", (unsigned int)pc);
    delete pc;
    free((void*)pc);
  }
      
  free(ast_massive);
  ast_massive_pos = 0;
  ast_massive_cells = 1024;

}

// called when #[string, tokentype] is seen in an action
AST::AST(char *s, ANTLRTokenType tok)
{
  //printf("constructor\n"); 
  token = new ANTLRToken(tok,s); 
}

// called when #[string, ASTNodeType] is seen in an action
AST::AST(char *s, ASTNodeType type) {
  token = new ANTLRToken();
  token->setText(s);
  token->setType(ANTLRTokenType(type));
  //printf("constructor text=%s\n\n", s);
}

// called when #[string, ASTNodeType] is seen in an action
AST::AST(string str, ASTNodeType type){
  token = new ANTLRToken();
  token->setText(str.c_str());
  token->setType(ANTLRTokenType(type));

  //printf("constructor\n");
}

// constructor called by parser for token references in grammar
//AST::AST(ANTLRTokenPtr t) { token = t; }

// caled when #[char *] is seen in an action
AST::AST(const char *s) {
  token = new ANTLRToken();
  token->setText(s);
  //printf("constructor for char*=%s\n",s);
}

//caled when #[string] is seen in an action
AST::AST(string str){
  token = new ANTLRToken();
  token->setText(str.c_str());
  //printf("constructor string=%s\n", str.c_str());
}

// caled when #[ASTNodeType] is seen in an action
AST::AST(ASTNodeType type) {
  token = new ANTLRToken();
  token->setType((ANTLRTokenType)type);
  //printf("constructor\n");
}


// empty constructor
AST::AST() {
  token = new ANTLRToken();
  token->setType((ANTLRTokenType)-1);

  //printf("constructor\n");
}

// copy constructor
AST::AST (const AST &t) {
  token= new ANTLRToken();
  token->setText((t.token)->getText());
  token->setType((t.token)->getType());
//  *token = *(t.token);
  setDown(NULL);
  setRight(NULL);
  //printf("copy constructor\n");
}

AST::~AST()
{
  //printf("destructor for AST called\n");
  delete token;
  //printf("destructor finished\n");
}

void* AST::operator new (size_t  size) {
  //printf("caled begin AST::new size=%d\n", size);
  void * p = malloc(size);

  if (ast_massive_pos < ast_massive_cells)
     *((unsigned int *)((char*)ast_massive + ast_massive_pos*sizeof(AST*))) = (unsigned int)p;
  else
  {
     realloc_ast_vector();
     *((unsigned int *)((char*)ast_massive + ast_massive_pos*sizeof(AST*))) = (unsigned int)p;
  }

  ast_massive_pos++;
  //d_printf2("pointer=%x alloced\n", (unsigned int)p);
  return p; 
};

void AST::operator delete (void *p){;};


PCCTS_AST*
AST::shallowCopy()
{
  //printf("shallow copy\n");
  return new AST(*this);
}

//PCCTS_AST*
//AST::shallowCopy() {return this;}


// return AST type value
int
AST::type () {return (int) token->getType(); }

// return AST text value
char*
AST::getText() { return token->getText(); }

void
AST::setText(string text)
{
  token->setText(text.c_str());
}

// define what happens at a node when preorder() is called
void
AST::preorder_action(void* p) {

//  printf("[");
//  print_ast_type(token->getType());
//  if(strcmp(token->getText(),"")!=0) printf(" %s] ",token->getText());
//  else printf("] ");

//  printf("%s",token->getText());

  ((string *)p)->append(token->getText());
  ((string *)p)->append(" ");

}

void
AST::preorder_before_action(void* p){


  ((string *)p)->append("(");
//  printf("( ");
}

void
AST::preorder_after_action(void* p){


  ((string *)p)->append(")");
//  printf(") ");
}
