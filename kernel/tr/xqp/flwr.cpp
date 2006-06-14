/*
 * File:  flwr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PCCTSAST.h"
#include "AST.h"
#include "ASTBase.h"
#include "ASTNodeTypes.h"


PCCTS_AST*
make_nested_flwr(PCCTS_AST* flcs, PCCTS_AST* r_cl, PCCTS_AST* w_cl, PCCTS_AST* var_decls, bool order_by){

 if(flcs==NULL)
 {
   if(w_cl==NULL)
   {
      if (order_by)
         return ASTBase::tmake(new AST(AST_UNIO), var_decls); 
      else r_cl;
   }
   else
   {
      if (order_by)
         return ASTBase::tmake(new AST(AST_IF), w_cl, ASTBase::tmake(new AST(AST_UNIO), var_decls), ASTBase::tmake(new AST(AST_RELATIVE_PATH), ASTBase::tmake(new AST(AST_FILTER_PATH_STEP),  ASTBase::tmake(new AST(AST_SEQUENCE), NULL), ASTBase::tmake(new AST(AST_PREDICATES), NULL), NULL), NULL));
      else
         return ASTBase::tmake(new AST(AST_IF), w_cl, r_cl, ASTBase::tmake(new AST(AST_RELATIVE_PATH), ASTBase::tmake(new AST(AST_FILTER_PATH_STEP),  ASTBase::tmake(new AST(AST_SEQUENCE), NULL), ASTBase::tmake(new AST(AST_PREDICATES), NULL), NULL), NULL), NULL); 
   }
 } //exit from recursion

 else{
     PCCTS_AST *r=flcs->right(), *tmp;
     flcs->setRight(NULL);
    
     tmp = ASTBase::tmake(new AST(AST_BOUND), flcs,
                           make_nested_flwr(r, r_cl, w_cl, var_decls, order_by), NULL);

     return tmp;
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