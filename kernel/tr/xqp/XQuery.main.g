/* */

class XQueryParser

{



script![XQueryDLGLexer* lexer_] :
	<<ASTBase* last_sib = NULL;>>
	q1:query[$lexer_]

	 <<
	  // $lexer_->mode($lexer_->SCRIPT_LANGUAGE);
	   #0 = #(#[AST_SCRIPT], #q1);
	   last_sib = #q1;
	 >>
	( STMNT_DELIM   
	  q2:query[$lexer_] 
	  <<
	     last_sib->setRight(#q2);
	     last_sib = #q2;
	  >>
	)*
	<<
	  if(LA(1) != eofToken)
	  {
	    if (LA(1) == eofToken) LT(1)->setText("Eof");
		throw USER_EXCEPTION2(XPST0003, (std::string("unexpected token: ")+ "\'" + LT(1)->getText() + "\'" + ", line: " + int2string(LT(1)->getLine())).c_str());
	  }
	>>

;

exception
	catch MismatchedToken:
    <<
	{
	  std::string text = "";
	  if (LA(1) == eofToken) text= "Eof";
	  else text = LT(1)->getText();
	  throw USER_EXCEPTION2(XPST0003, (std::string("unexpected token: ") + "\'" + text + "\'" + ", line: " + int2string(LT(1)->getLine())).c_str());
	}
	>>
	default: 
    <<
	{
	  std::string text = "";
	  if (LA(1) == eofToken) text = "Eof";
	  else if (LA(2) != eofToken) text = LT(2)->getText(); 
	  else text = LT(1)->getText();
	  throw USER_EXCEPTION2(XPST0003, (std::string("syntax error at token: ") + "\'" + text + "\'" + ", line: " +  int2string(LT(1)->getLine())).c_str());
	}
	>>



query![XQueryDLGLexer* lexer_] :
	   qp:queryProlog
	 (
       ce:createExpr[$lexer_] <<#0=#ce;>>
	   <<#0=#(#[AST_CREATE], #(#[AST_PROLOG], #qp), #ce);>>

	 | e:expr
	
	   <<#0=#(#[AST_QUERY], #(#[AST_PROLOG], #qp), #(#[AST_EXPR], #e));>>

	 | ue:updateExpr
	  
	   <<#0=#(#[AST_UPDATE], #(#[AST_PROLOG], #qp), #ue);>>


	 | md:metadataExpr

	   <<#0=#(#[AST_METADATA], #(#[AST_PROLOG], #qp), #md);>>

	 | ROLLBACK <<#0=#[AST_ROLLBACK];>>

	 | COMMIT <<#0=#[AST_COMMIT];>>
     )
;



}