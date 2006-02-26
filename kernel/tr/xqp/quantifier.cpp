/*
 * File:  quantifier.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "PCCTSAST.h"
#include "AST.h"
#include "ASTBase.h"
#include "ASTNodeTypes.h"


PCCTS_AST*
make_nested_quantifier(PCCTS_AST* q, PCCTS_AST* v_decls, PCCTS_AST* bv){

 if(v_decls==NULL)   //exit from recursion
   return bv;  

 else{
   
     PCCTS_AST *r=v_decls->right();
     v_decls->setRight(NULL);

    
     return ASTBase::tmake(new AST(*((AST *)q)), v_decls, make_nested_quantifier(q, r, bv), NULL);
    
 }

}
