/************************************************************
****** XQuery arithmetical syntactic analizer rules *********
*************************************************************/

class XQueryParser {

orExpr!:
	left:andExpr <<#0=#left;>>
	(OR right:andExpr <<#0=#(#["or", AST_B_OP], #0, #right);>> )*
;


andExpr!:
	left:instanceOfExpr <<#0=#left;>>
	(AND right:instanceOfExpr <<#0=#(#["and", AST_B_OP], #0, #right);>>)*
;

instanceOfExpr!:
	te:treatExpr  <<#0=#te;>>
	{INSTANCE OF st:sequenceType 
	 <<#0=#(#[AST_INSTANCE_OF], #0, #st);>>
	}
;

treatExpr!:
	ce:castableExpr  <<#0=#ce;>>
	{TREAT AS st:sequenceType
	 <<#0=#(#[AST_TREAT], #0, #st);>>
	}
;

castableExpr!:
	ce:castExpr  <<#0=#ce;>>
	{CASTABLE AS st:singleType
	 <<#0=#(#[AST_CASTABLE], #0, #st);>>
	}
;

castExpr!:
	ce:comparisonExpr  <<#0=#ce;>>
	{CAST AS st:singleType
	 <<#0=#(#[AST_CAST], #0, #st);>>
	}
;

comparisonExpr!:
	<<ASTBase* op=NULL;>>
	re1:rangeExpr       <<#0=#re1;>>
	{(  vc:valueComp    <<op=#vc;>>
	  | gc:generalComp  <<op=#gc;>>
	  | nc:nodeComp     <<op=#nc;>>
	 ) re2:rangeExpr    <<#0=#(op, #0, #re2);>>
	}
;

valueComp!:
	  EQ  <<#0=#["eq", AST_B_OP];>>
	| NE  <<#0=#["ne", AST_B_OP];>>
	| LT_ <<#0=#["lt", AST_B_OP];>>
	| LE  <<#0=#["le", AST_B_OP];>>
	| GT  <<#0=#["gt", AST_B_OP];>>
	| GE  <<#0=#["ge", AST_B_OP];>>
;

generalComp!:
	  EQUAL        <<#0=#["=", AST_B_OP];>>
	| NOTEQUAL     <<#0=#["!=", AST_B_OP];>>
	| LESS         <<#0=#["<", AST_B_OP];>>
	| LESSEQUAL    <<#0=#["<=", AST_B_OP];>>
	| GREAT        <<#0=#[">", AST_B_OP];>>
	| GREATEQUAL   <<#0=#[">=", AST_B_OP];>>
;

nodeComp!:
	  IDENT            <<#0=#["is", AST_B_OP];>> 
	| LESS_DOC_ORDER   <<#0=#["<<", AST_B_OP];>>
	| GREAT_DOC_ORDER  <<#0=#[">>", AST_B_OP];>>
;

rangeExpr!:
	ae1:additiveExpr <<#0=#ae1;>>
	{TO ae2:additiveExpr
	 <<#0=#(#["to", AST_B_OP], #0, #ae2);>>
	}
;

additiveExpr!:
	<<ASTBase *op=NULL;>>
	me1:multiplicativeExpr <<#0=#me1;>>
	((  PLUS  <<op=#["+", AST_B_OP];>>
	  | MINUS <<op=#["-", AST_B_OP];>>
	 ) 
	me2:multiplicativeExpr
	<<#0=#(op, #0, #me2);>>
	)*
;

multiplicativeExpr!:
	<<ASTBase *op=NULL;>>
	ue1:unaryExpr <<#0=#ue1;>>
	((  STAR   <<op=#["*", AST_B_OP];>>
	  | DIV    <<op=#["/", AST_B_OP];>>
	  | IDIV   <<op=#["idiv", AST_B_OP];>>
	  | MOD    <<op=#["mod", AST_B_OP];>>
	 )
	 ue2:unaryExpr
	 <<#0=#(op, #0, #ue2);>>
	)*
;

unaryExpr!:
	<<ASTBase *op=NULL;>>
	{  MINUS  <<op=#["unary-", AST_UNARY_OP];>>
	 | PLUS   <<op=#["unary+", AST_UNARY_OP];>>
	}//does not corresponds to the specification
	 ue:unionExpr
	 <<if(op==NULL) #0=#ue;
	   else #0=#(op, #ue);
	 >>
;

unionExpr!:
	<<ASTBase *op=NULL;>>
	iee1:intersectExceptExpr <<#0=#iee1;>>
	((  UNION <<op=#["union", AST_B_OP];>>
	  | ALT   <<op=#["union", AST_B_OP];>>
	 )
	 iee2:intersectExceptExpr
	 <<#0=#(op, #0, #iee2);>>
	)*
;

intersectExceptExpr!:
	<<ASTBase *op=NULL;>>
	ve1:valueExpr <<#0=#ve1;>>
	((  INTERSECT <<op=#["intersect", AST_B_OP];>>
	  | EXCEPT    <<op=#["except", AST_B_OP];>>
	 )
	 ve2:valueExpr
	 <<#0=#(op, #0, #ve2);>>
	)*
;

valueExpr!:
	  pe:pathExpr <<#0=#pe;>>
//	| validateExpr
	| ee:extensionExpr <<#0=#ee;>>
;

validateExpr!:
	VALIDATE {(LAX | STRICT_)} LBRACE expr RBRACE
	<<throw USER_EXCEPTION(XQST0075);>>	
;

typeswitchExpr!:
	<<ASTBase* var;>>
	TYPESWITCH LPAR e1:expr RPAR cc:caseClauses DEFAULT {v:varRef AS} RETURN e2:exprSingle
	<<
	  if (#v == NULL) var=#(#[AST_VAR], #(#[AST_QNAME], #["%v", AST_LOCAL_NAME], #["", AST_PREFIX]));
	  else var = #v;
	  #0=#(#[AST_TYPESWITCH], #e1, #cc, #(#[AST_DEFAULT_TS], var, #e2));
	>>
;

caseClauses!:
	<<ASTBase* var;
	  bool exist_var=false;
	  #0=#(#[AST_CASES]);
	>>
	(CASE_ {v:varRef AS <<exist_var=true;>>} t:sequenceType RETURN e:exprSingle
	  <<
	    if (!exist_var) var=#(#[AST_VAR], #(#[AST_QNAME], #["%v", AST_LOCAL_NAME], #["", AST_PREFIX]));
	    else var = #v;

	    #0->addChild(#(#[AST_CASE_TS], var, #t, #e));
	    exist_var = false;
	  >>
	)+
;

extensionExpr!:
	p:pragmas LBRACE {e:expr} RBRACE
	<<#0=#(#[AST_EXTENSION_EXPR], #p, #e);>>
;

pragmas!:
	<<#0=#(#[AST_PRAGMAS]);>>
	(PR_OPEN  {c:prcontent}  PR_CLOSE 

	 <<#0->addChild(#(#[AST_PRAGMA], #c));>>	
	)+
;

prcontent!:
	p:PR_CONTENT <<#0=#[$p->getText(), AST_STRING_CONST];>>
;

}