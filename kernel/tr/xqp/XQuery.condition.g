/************************************************************
******* XQuery conditional syntactic analizer rules *********
*************************************************************/

class XQueryParser {

ifExpr!:
	IF LPAR cond:expr RPAR THEN e1:exprSingle ELSE e2:exprSingle
	
	<<#0=#(#[AST_IF], #cond, #e1, #e2);>>

;


}



