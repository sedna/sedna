/************************************************************
************* XQuery flwr syntactic analizer rules **********
*************************************************************/

class XQueryParser{

flworExpr!:
	<<AST *flcs=NULL;>>

	( fc:forClause <<if(flcs==NULL) flcs=#fc; else flcs->append(#fc);>>
	| lc:letClause <<if(flcs==NULL) flcs=#lc; else flcs->append(#lc);>>)+
	{wc:whereClause}
	{obc:orderByClause}
	RETURN e:exprSingle
	<<ASTBase* it;
      ASTBase* copy_var_decls1 = NULL, *copy_var_decls2 = NULL;

      for(it = flcs; it != NULL; it =(ASTBase*)(it->right()))
      {

        if (copy_var_decls1 == NULL)
        {

           copy_var_decls1=ASTBase::tmake(new AST(AST_VAR_DECL), ((ASTBase*)(it->down()->down()))->dup(), NULL);
           copy_var_decls2=ASTBase::tmake(new AST(AST_VAR_DECL), ((ASTBase*)(it->down()->down()))->dup(), NULL);
        }
        else
        {
           copy_var_decls1->append(ASTBase::tmake(new AST(AST_VAR_DECL), ((ASTBase*)(it->down()->down()))->dup(), NULL));
           copy_var_decls2->append(ASTBase::tmake(new AST(AST_VAR_DECL), ((ASTBase*)(it->down()->down()))->dup(), NULL));
        }
      }
      if (#obc != NULL)
	    #0=ASTBase::tmake(new AST(AST_FLWR_ORDER_BY), (ASTBase*)make_nested_flwr(flcs, #e, #wc, copy_var_decls1, true), copy_var_decls2, #obc, #e, NULL);
	  else
	    #0=(ASTBase*)make_nested_flwr(flcs, #e, #wc, copy_var_decls1, false);
	>>
;

forClause!:
	FOR
	v1:varRef {td1:typeDeclaration} {pv1:positionalVar} IN_ e1:exprSingle

	<<#0=#(#[AST_FOR], #(#[AST_VAR_DECL], #v1, #e1, #td1), #pv1);>>

	(COMMA v2:varRef {td2:typeDeclaration} {pv2:positionalVar} IN_ e2:exprSingle
	<<#0->append(#(#[AST_FOR], #(#[AST_VAR_DECL], #v2, #e2, #td2), #pv2));>>	
	)*
;

positionalVar!:
	AT_ v:varRef
	<<#0=#v;>>
;

letClause!:
	LET
	v1:varRef {td1:typeDeclaration} COLONEQUALS e1:exprSingle

	<<#0=#(#[AST_LET], #(#[AST_VAR_DECL], #v1, #e1, #td1));>>	

	(COMMA v2:varRef {td2:typeDeclaration}  COLONEQUALS e2:exprSingle
	<<#0->append(#(#[AST_LET], #(#[AST_VAR_DECL], #v2, #e2, #td2)));>>
	)*
;

whereClause!:
	WHERE e:expr
	<<#0=#e;>>
;

orderByClause!:
	(  ORDER BY ocl1:orderSpecList 
	   <<#0=#(#[AST_ORDER_BY], #ocl1);>>

	 | STABLE ORDER BY ocl2:orderSpecList 
	   <<#0=#(#[AST_STABLE_ORDER_BY], #ocl2);>>
	)

;

orderSpecList!:
	os1:orderSpec <<#0=#os1;>>

	(COMMA os2:orderSpec <<#0->append(#os2);>>)*
;

orderSpec!:
	e:exprSingle om:orderModifier
	<<#0=#(#[AST_ORDER_SPEC], #e, #om);>>
;

orderModifier!:
	<<ASTBase *asc_desc=NULL, *empt_gr_lst=NULL, *col=NULL;>>
	{ ASCENDING <<asc_desc=#["\"asc\"", AST_ORDER_PROPERTY];>> 
	| DESCENDING <<asc_desc=#["\"desc\"", AST_ORDER_PROPERTY];>>
	}
	{ (EMPTY GREATEST) <<empt_gr_lst=#["\"empty-greatest\"", AST_ORDER_PROPERTY];>>
	| (EMPTY LEAST)    <<empt_gr_lst=#["\"empty-least\"", AST_ORDER_PROPERTY];>>
	}
	{COLLATION s:STRINGLITERAL <<col=#[$s->getText(), AST_ORDER_PROPERTY];>>}

	<<#0=#(#[AST_ORDER], 
	         #(#[AST_ASC_DESC], asc_desc),
	         #(#[AST_EMPT_GR_LST], empt_gr_lst),
	         #(#[AST_COLLATION], col));
	>>
;

}
