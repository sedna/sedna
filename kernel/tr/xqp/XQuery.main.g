/* */

class XQueryParser

{



script![XQueryDLGLexer* lexer_] :
	<<ASTBase* last_sib = NULL;>>
	q1:module

	 <<
	  // $lexer_->mode($lexer_->SCRIPT_LANGUAGE);
	   #0 = #(#[AST_SCRIPT], #q1);
	   last_sib = #q1;
	 >>
	( STMNT_DELIM   
	  q2:module
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


module!:
	(lm:libraryModule <<#0=#(#[AST_MODULE], #lm);>> | mm:mainModule <<#0=#(#[AST_MODULE], #mm);>>)
	
;

versionDecl!:
	XQUERY VERSION s1:STRINGLITERAL 
	<<#0=#(#[AST_VERSION_DECL], #[$s1->getText(), AST_STRING_CONST]);>>
	{ENCODING s2:STRINGLITERAL <<#0->addChild(#[$s2->getText(), AST_STRING_CONST]);>>}
;

libraryModule!:
	 md:moduleDecl p:queryProlog
	<<
	  #0=#(#[AST_LIB_MODULE], #md, #(#[AST_PROLOG], #p));
	>>
;

moduleDecl!:
	MODULE NAMESPACE nc:ncname EQUAL s:STRINGLITERAL SEMICOLON
	<<#0=#(#[AST_MODULE_DECL], #nc, #[$s->getText(), AST_STRING_CONST]);>>
;

mainModule!:
	q:query <<#0=#(#[AST_MAIN_MODULE], #q);>>
;



query!:
	<<ASTBase* prol=NULL;>>
	  {vd:versionDecl}  qp:queryProlog

	  <<if (#vd==NULL) prol = #qp;
	    else {prol=#vd; prol->append(#qp);}
	  >>
	 
	 (
	   ce:createExpr <<#0=#ce;>>
	   <<#0=#(#[AST_CREATE], #(#[AST_PROLOG], prol), #ce);>>

	 | e:expr
	
	   <<#0=#(#[AST_QUERY], #(#[AST_PROLOG], prol), #(#[AST_EXPR], #e));>>

	 | ue:updateExpr
	  
	   <<#0=#(#[AST_UPDATE], #(#[AST_PROLOG], prol), #ue);>>
           
	 | md:metadataExpr

	   <<#0=#(#[AST_METADATA], #(#[AST_PROLOG], prol), #md);>>

	 | ROLLBACK <<#0=#[AST_ROLLBACK];>>

	 | COMMIT <<#0=#[AST_COMMIT];>>
     )
;



}