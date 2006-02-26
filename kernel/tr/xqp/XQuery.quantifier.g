/************************************************************
******** XQuery quantifiers syntactic analizer rules ********
*************************************************************/

class XQueryParser{

quantifiedExpr!:
	<<ASTBase *vars_decl=NULL;>>
	q:quantifier

	v1:varRef {td1:typeDeclaration}  i:IN_ ie1:exprSingle

	<<vars_decl=#(#[AST_VAR_DECL], #v1, #ie1, #td1);>>

	(COMMA v2:varRef {td2:typeDeclaration} IN_ ie2:exprSingle
	<<vars_decl->append(#(#[AST_VAR_DECL], #v2, #ie2, #td2));>>
	)*
	s:SATISFIES be:exprSingle

	<<#0=(ASTBase *)make_nested_quantifier(#q, vars_decl, #be);>>

;
/*exception[v]
	catch MismatchedToken: 
	<<throw XQueryParserBadQuery(string("quantifiedExpr::syntax error: token \'")+LT(1)->getText()+"\' is not a variable", LT(1)->getLine());>>
exception[i]
	catch MismatchedToken: 
	<<throw XQueryParserBadQuery(string("quantifiedExpr::syntax error: instead token \'")+LT(1)->getText()+"\' must be keyword 'in'", LT(1)->getLine());>>
exception[e]
	catch MismatchedToken: 
	<<throw XQueryParserBadQuery(string("quantifiedExpr::syntax error: unexpected token \'")+LT(1)->getText()+"\' in in-clause", LT(1)->getLine());>>
exception[s]
	catch MismatchedToken: 
	<<throw XQueryParserBadQuery(string("quantifiedExpr::syntax error: instead token \'")+LT(1)->getText()+"\' must be keyword 'satisfies'", LT(1)->getLine());>>
exception[qe]
	catch MismatchedToken: 
	<<throw XQueryParserBadQuery(string("quantifiedExpr::syntax error: unexpected token \'")+LT(1)->getText()+"\' in test-expression", LT(1)->getLine());>>

                       	
exception
	catch MismatchedToken:
	<<throw XQueryParserBadQuery(string("quantifiedExpr::syntax error: unexpected token in quantifier expression ")+LT(1)->getText(), LT(1)->getLine());>>   
*/
quantifier!:
	  SOME <<#0=#[AST_SOME];>>
	| EVERY  <<#0=#[AST_EVERY];>>
;

}