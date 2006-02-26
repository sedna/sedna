/*
 * File:  flwr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "PCCTSAST.h"
#include "AST.h"
#include "ASTBase.h"
#include "ASTNodeTypes.h"


PCCTS_AST*
make_nested_flwr(PCCTS_AST* flcs, PCCTS_AST* r_cl, PCCTS_AST* w_cl, PCCTS_AST* o_cl){

 if(flcs==NULL)
 {
   if(w_cl==NULL)
      return r_cl;  
   else
      return ASTBase::tmake(new AST(AST_IF), w_cl, r_cl, ASTBase::tmake(new AST(AST_RELATIVE_PATH), ASTBase::tmake(new AST(AST_FILTER_PATH_STEP),  ASTBase::tmake(new AST(AST_SEQUENCE), NULL), ASTBase::tmake(new AST(AST_PREDICATES), NULL), NULL), NULL), NULL); 
 } //exit from recursion

 else{
   if(o_cl==NULL){
     PCCTS_AST *r=flcs->right(), *tmp;
     flcs->setRight(NULL);
    
     tmp = ASTBase::tmake(new AST(AST_BOUND), flcs,
                           make_nested_flwr(r, r_cl, w_cl, NULL), NULL);

     return tmp;
   }
   else
     return ASTBase::tmake(new AST(AST_ORDER_BY), o_cl,
                           make_nested_flwr(flcs, r_cl, w_cl, NULL), NULL);  
    
 }

}

/*
vars_decl(PCCTS_AST* flcs){
  if(flcs!=NULL){

    PCCTS_AST *var_name=flcs->down();

    PCCTS_AST *var_type;

    if(flcs->down()->down()->right()->right()!=NULL)
       var_type=flcs->down()->down()->right()->right();
    else
       var_type=ASTBase::tmake(new AST("xs:anyType"));
    
    if(flcs->right()!=NULL)
       return ASTBase::tmake(var_type, var_name, NULL)->append(var_decl(flcs->right()));
    else
       return ASTBase::tmake(var_type, var_name, NULL);
  }
  else //flcs==NULL
    return NULL;
}
*/