/* */

class XQueryParser{

expr!:
	<<ASTBase *es=NULL;>>
	e1:exprSingle 
        <<es=#e1;>> 

	(COMMA e2:exprSingle
	<<es->append(#e2);>>)*

	<<
	  if(es->nsiblings()>1) 
	    #0=#(#[AST_SEQUENCE], es);
	  else
	    #0=es;
	>>
;


exprSingle!:

	  fe:flworExpr      <<#0=#fe;>>
	| te:typeswitchExpr  <<#0=#te;>>
	| ie:ifExpr         <<#0=#ie;>>
	| oe:orExpr         <<#0=#oe;>>
	| qe:quantifiedExpr <<#0=#qe;>>
//	  ie:ifExpr          <<#0=#ie;>>
;

qname!:
     <<bool p1_=false, p2_=false, l1_=false, l2_=false;>>
	( {(p1:prefixPart1 <<p1_=true;>> | p2:prefixPart2 <<p2_=true;>>) COLON} (l1:localPart1 <<l1_ = true;>>| l2:localPart2<<l2_=true;>>)
	  <<
        if (l1_ == NULL)
	    {
	       if (p1_ == NULL)
	   	       #0=#(#[AST_QNAME], #l2, #p2);
	       else
	   	       #0=#(#[AST_QNAME], #l2, #p1);
	    }
	    else
	    {
	       if (p1_ == NULL)
	   	       #0=#(#[AST_QNAME], #l1, #p2);
	       else
	   	       #0=#(#[AST_QNAME], #l1, #p1);

	    }
	  >>
	)
;

prefix!:
	  nn:NCNAME <<#0=#[$nn->getText(), AST_PREFIX];>>
;

localPart1!:
  	  nn:NCNAME <<#0=#[$nn->getText(), AST_LOCAL_NAME];>>
	| DECLARE <<#0=#["declare", AST_LOCAL_NAME];>>
	| FUNCTION <<#0=#["function", AST_LOCAL_NAME];>>
	| EXTERNAL <<#0=#["external", AST_LOCAL_NAME];>>
	| NAMESPACE <<#0=#["namespace", AST_LOCAL_NAME];>>
	| DEFAULT <<#0=#["default", AST_LOCAL_NAME];>>
	| FOR  <<#0=#["for", AST_LOCAL_NAME];>>
	| LET  <<#0=#["let", AST_LOCAL_NAME];>>
	| IN_  <<#0=#["in", AST_LOCAL_NAME];>>
	| WHERE <<#0=#["where", AST_LOCAL_NAME];>>
	| STABLE <<#0=#["stable", AST_LOCAL_NAME];>>
	| ORDER  <<#0=#["order", AST_LOCAL_NAME];>>
	| BY     <<#0=#["by", AST_LOCAL_NAME];>>
	| CBY    <<#0=#["BY", AST_LOCAL_NAME];>>
	| AT_    <<#0=#["at", AST_LOCAL_NAME];>>
	| ASCENDING <<#0=#["ascending", AST_LOCAL_NAME];>>
	| DESCENDING <<#0=#["descending", AST_LOCAL_NAME];>>
	| GREATEST  <<#0=#["greatest", AST_LOCAL_NAME];>>
	| LEAST     <<#0=#["least", AST_LOCAL_NAME];>>
	| COLLATION <<#0=#["collation", AST_LOCAL_NAME];>>
	| RETURN    <<#0=#["return", AST_LOCAL_NAME];>>
	| SOME      <<#0=#["some", AST_LOCAL_NAME];>>
	| EVERY     <<#0=#["every", AST_LOCAL_NAME];>>
	| SATISFIES <<#0=#["satisfies", AST_LOCAL_NAME];>>
	| IF  <<#0=#["if", AST_LOCAL_NAME];>>
	| THEN <<#0=#["then", AST_LOCAL_NAME];>>
	| ELSE <<#0=#["else", AST_LOCAL_NAME];>>
	| ELEMENT <<#0=#["element", AST_LOCAL_NAME];>>
	| OF      <<#0=#["of", AST_LOCAL_NAME];>>
	| TYPE    <<#0=#["type", AST_LOCAL_NAME];>>
	| ATTRIBUTE <<#0=#["attribute", AST_LOCAL_NAME];>>
	| XMLNS <<#0=#["xmlns", AST_LOCAL_NAME];>>
	| DEFINE <<#0=#["define", AST_LOCAL_NAME];>>
	| EMPTY  <<#0=#["empty", AST_LOCAL_NAME];>>
	| ITEM   <<#0=#["item", AST_LOCAL_NAME];>>
	| TEXT   <<#0=#["text", AST_LOCAL_NAME];>>
	| NILLABLE <<#0=#["nillable", AST_LOCAL_NAME];>>
	| PROCESSING_INSTRUCTION <<#0=#["processing-instruction", AST_LOCAL_NAME];>>
	| COMMENT_  <<#0=#["comment", AST_LOCAL_NAME];>>
	| NODE      <<#0=#["node", AST_LOCAL_NAME];>>
	| DOCUMENT  <<#0=#["document-node", AST_LOCAL_NAME];>>
	| CHILD     <<#0=#["child", AST_LOCAL_NAME];>>
	| DESCENDANT  <<#0=#["descendant", AST_LOCAL_NAME];>>
	| SELF       <<#0=#["self", AST_LOCAL_NAME];>>
	| DESCENDANT_OR_SELF <<#0=#["descendant-or-self", AST_LOCAL_NAME];>>
	| FOLLOWING_SIBLING <<#0=#["following-sibling", AST_LOCAL_NAME];>>
	| FOLLOWING   <<#0=#["following", AST_LOCAL_NAME];>>
	| PARENT   <<#0=#["parent", AST_LOCAL_NAME];>>
	| ANCESTOR <<#0=#["ancestor", AST_LOCAL_NAME];>>
	| PRECEDING_SIBLING <<#0=#["preceding-sibling", AST_LOCAL_NAME];>>
	| PRECEDING  <<#0=#["preceding", AST_LOCAL_NAME];>>
	| ANCESTOR_OR_SELF <<#0=#["ancestor-or-self", AST_LOCAL_NAME];>>
	| CASTABLE  <<#0=#["castable", AST_LOCAL_NAME];>>
;

localPart2!:
	  AS  <<#0=#["as", AST_LOCAL_NAME];>>
	| CAS <<#0=#["AS", AST_LOCAL_NAME];>>
	| CAST <<#0=#["cast", AST_LOCAL_NAME];>>
	| TO   <<#0=#["to", AST_LOCAL_NAME];>>
	| INSTANCE <<#0=#["instance", AST_LOCAL_NAME];>>
	| TREAT <<#0=#["treat", AST_LOCAL_NAME];>>
	| UPDATE <<#0=#["UPDATE", AST_LOCAL_NAME];>>
	| INTO <<#0=#["into", AST_LOCAL_NAME];>>
	| WITH  <<#0=#["with", AST_LOCAL_NAME];>>
	| ON    <<#0=#["on", AST_LOCAL_NAME];>>
	| CON   <<#0=#["ON", AST_LOCAL_NAME];>>
	| CREATE <<#0=#["CREATE", AST_LOCAL_NAME];>>
	| DROP  <<#0=#["DROP", AST_LOCAL_NAME];>>
	| GRANT <<#0=#["GRANT", AST_LOCAL_NAME];>>
	| REVOKE <<#0=#["REVOKE", AST_LOCAL_NAME];>>
	| LOAD   <<#0=#["LOAD", AST_LOCAL_NAME];>>
	| ALTER  <<#0=#["ALTER", AST_LOCAL_NAME];>>
	| RETRIEVE <<#0=#["RETRIEVE", AST_LOCAL_NAME];>>
	| OR     <<#0=#["or", AST_LOCAL_NAME];>>
	| AND    <<#0=#["and", AST_LOCAL_NAME];>>
	| DIV    <<#0=#["div", AST_LOCAL_NAME];>>
	| IDIV   <<#0=#["idiv", AST_LOCAL_NAME];>>
	| MOD    <<#0=#["mod", AST_LOCAL_NAME];>>
	| EQ     <<#0=#["eq", AST_LOCAL_NAME];>>
	| NE     <<#0=#["ne", AST_LOCAL_NAME];>>
	| LT_    <<#0=#["lt", AST_LOCAL_NAME];>>
	| LE     <<#0=#["le", AST_LOCAL_NAME];>>
	| GT     <<#0=#["gt", AST_LOCAL_NAME];>>
	| GE     <<#0=#["ge", AST_LOCAL_NAME];>>
	| IDENT  <<#0=#["is", AST_LOCAL_NAME];>>
	| UNION  <<#0=#["union", AST_LOCAL_NAME];>>
	| INTERSECT <<#0=#["intersect", AST_LOCAL_NAME];>>
	| EXCEPT  <<#0=#["except", AST_LOCAL_NAME];>>
	| ROLE    <<#0=#["ROLE", AST_LOCAL_NAME];>>
	| DATABASE <<#0=#["DATABASE", AST_LOCAL_NAME];>>
	| INDEX   <<#0=#["INDEX", AST_LOCAL_NAME];>>
	| FROM    <<#0=#["FROM", AST_LOCAL_NAME];>>
	| CCOLLECTION <<#0=#["COLLECTION", AST_LOCAL_NAME];>>
	| CIN_     <<#0=#["IN", AST_LOCAL_NAME];>>
	| STDIN    <<#0=#["STDIN", AST_LOCAL_NAME];>>
	| USER     <<#0=#["USER", AST_LOCAL_NAME];>>
	| PASSWORD <<#0=#["PASSWORD", AST_LOCAL_NAME];>>
	| METADATA <<#0=#["METADATA", AST_LOCAL_NAME];>>
	| FOR_     <<#0=#["FOR", AST_LOCAL_NAME];>>
	| DOCUMENTS <<#0=#["DOCUMENTS", AST_LOCAL_NAME];>>
	| COLLECTIONS <<#0=#["COLLECTIONS", AST_LOCAL_NAME];>>
	| DESCRIPTIVE <<#0=#["DESCRIPTIVE", AST_LOCAL_NAME];>>
	| SCHEMA      <<#0=#["SCHEMA", AST_LOCAL_NAME];>>
	| STATISTICS  <<#0=#["STATISTICS", AST_LOCAL_NAME];>>
	| INSERT      <<#0=#["insert", AST_LOCAL_NAME];>>
	| DELETE_     <<#0=#["delete", AST_LOCAL_NAME];>>
	| DELETE_UNDEEP <<#0=#["delete_undeep", AST_LOCAL_NAME];>>
	| REPLACE     <<#0=#["replace", AST_LOCAL_NAME];>>
	| RENAME      <<#0=#["rename", AST_LOCAL_NAME];>>
	| MOVE        <<#0=#["move", AST_LOCAL_NAME];>>
	| ALL         <<#0=#["ALL", AST_LOCAL_NAME];>>
	| PUBLIC      <<#0=#["PUBLIC", AST_LOCAL_NAME];>>
	| CTO         <<#0=#["TO", AST_LOCAL_NAME];>>
	| BOUNDARYSPACE <<#0=#["boundary-space", AST_LOCAL_NAME];>>
	| PRESERVE    <<#0=#["preserve", AST_LOCAL_NAME];>>
	| STRIP       <<#0=#["strip", AST_LOCAL_NAME];>>
	| OPTION       <<#0=#["option", AST_LOCAL_NAME];>>
	| LOADFILE       <<#0=#["LOADFILE", AST_LOCAL_NAME];>>
	| CDOCUMENT       <<#0=#["DOCUMENT", AST_LOCAL_NAME];>>
	| TYPESWITCH <<#0=#["typeswitch", AST_LOCAL_NAME];>>
	| CASE <<#0=#["case", AST_LOCAL_NAME];>>
	| LDOCUMENT <<#0=#["document", AST_LOCAL_NAME];>>
;

prefixPart1!:
  	  nn:NCNAME <<#0=#[$nn->getText(), AST_PREFIX];>>
	| DECLARE <<#0=#["declare", AST_PREFIX];>>
	| FUNCTION <<#0=#["function", AST_PREFIX];>>
	| EXTERNAL <<#0=#["external", AST_PREFIX];>>
	| NAMESPACE <<#0=#["namespace", AST_PREFIX];>>
	| DEFAULT <<#0=#["default", AST_PREFIX];>>
	| FOR  <<#0=#["for", AST_PREFIX];>>
	| LET  <<#0=#["let", AST_PREFIX];>>
	| IN_  <<#0=#["in", AST_PREFIX];>>
	| WHERE <<#0=#["where", AST_PREFIX];>>
	| STABLE <<#0=#["stable", AST_PREFIX];>>
	| ORDER  <<#0=#["order", AST_PREFIX];>>
	| BY     <<#0=#["by", AST_PREFIX];>>
	| CBY    <<#0=#["BY", AST_PREFIX];>>
	| AT_    <<#0=#["at", AST_PREFIX];>>
	| ASCENDING <<#0=#["ascending", AST_PREFIX];>>
	| DESCENDING <<#0=#["descending", AST_PREFIX];>>
	| GREATEST  <<#0=#["greatest", AST_PREFIX];>>
	| LEAST     <<#0=#["least", AST_PREFIX];>>
	| COLLATION <<#0=#["collation", AST_PREFIX];>>
	| RETURN    <<#0=#["return", AST_PREFIX];>>
	| SOME      <<#0=#["some", AST_PREFIX];>>
	| EVERY     <<#0=#["every", AST_PREFIX];>>
	| SATISFIES <<#0=#["satisfies", AST_PREFIX];>>
	| IF  <<#0=#["if", AST_PREFIX];>>
	| THEN <<#0=#["then", AST_PREFIX];>>
	| ELSE <<#0=#["else", AST_PREFIX];>>
	| ELEMENT <<#0=#["element", AST_PREFIX];>>
	| OF      <<#0=#["of", AST_PREFIX];>>
	| TYPE    <<#0=#["type", AST_PREFIX];>>
	| ATTRIBUTE <<#0=#["attribute", AST_PREFIX];>>
	| XMLNS    <<#0=#["xmlns", AST_PREFIX];>>
	| DEFINE <<#0=#["define", AST_PREFIX];>>
	| EMPTY  <<#0=#["empty", AST_PREFIX];>>
	| ITEM   <<#0=#["item", AST_PREFIX];>>
	| TEXT   <<#0=#["text", AST_PREFIX];>>
	| NILLABLE <<#0=#["nillable", AST_PREFIX];>>
	| PROCESSING_INSTRUCTION <<#0=#["processing-instruction", AST_PREFIX];>>
	| COMMENT_  <<#0=#["comment", AST_PREFIX];>>
	| NODE      <<#0=#["node", AST_PREFIX];>>
	| DOCUMENT  <<#0=#["document-node", AST_PREFIX];>>
	| CHILD     <<#0=#["child", AST_PREFIX];>>
	| DESCENDANT  <<#0=#["descendant", AST_PREFIX];>>
	| SELF       <<#0=#["self", AST_PREFIX];>>
	| DESCENDANT_OR_SELF <<#0=#["descendant-or-self", AST_PREFIX];>>
	| FOLLOWING_SIBLING <<#0=#["following-sibling", AST_PREFIX];>>
	| FOLLOWING   <<#0=#["following", AST_PREFIX];>>
	| PARENT   <<#0=#["parent", AST_PREFIX];>>
	| ANCESTOR <<#0=#["ancestor", AST_PREFIX];>>
	| PRECEDING_SIBLING <<#0=#["preceding-sibling", AST_PREFIX];>>
	| PRECEDING  <<#0=#["preceding", AST_PREFIX];>>
	| ANCESTOR_OR_SELF <<#0=#["ancestor-or-self", AST_PREFIX];>>
	| CASTABLE  <<#0=#["castable", AST_PREFIX];>>
;

prefixPart2!:
	  AS  <<#0=#["as", AST_PREFIX];>>
	| CAS <<#0=#["AS", AST_PREFIX];>>
	| CAST <<#0=#["cast", AST_PREFIX];>>
	| TO   <<#0=#["to", AST_PREFIX];>>
	| INSTANCE <<#0=#["instance", AST_PREFIX];>>
	| TREAT <<#0=#["treat", AST_PREFIX];>>
	| UPDATE <<#0=#["UPDATE", AST_PREFIX];>>
	| INTO <<#0=#["into", AST_PREFIX];>>
	| WITH  <<#0=#["with", AST_PREFIX];>>
	| ON    <<#0=#["on", AST_PREFIX];>>
	| CON   <<#0=#["ON", AST_PREFIX];>>
	| CREATE <<#0=#["CREATE", AST_PREFIX];>>
	| DROP  <<#0=#["DROP", AST_PREFIX];>>
	| GRANT <<#0=#["GRANT", AST_PREFIX];>>
	| REVOKE <<#0=#["REVOKE", AST_PREFIX];>>
	| LOAD   <<#0=#["LOAD", AST_PREFIX];>>
	| ALTER  <<#0=#["ALTER", AST_PREFIX];>>
	| RETRIEVE <<#0=#["RETRIEVE", AST_PREFIX];>>
	| OR     <<#0=#["or", AST_PREFIX];>>
	| AND    <<#0=#["and", AST_PREFIX];>>
	| DIV    <<#0=#["div", AST_PREFIX];>>
	| IDIV   <<#0=#["idiv", AST_PREFIX];>>
	| MOD    <<#0=#["mod", AST_PREFIX];>>
	| EQ     <<#0=#["eq", AST_PREFIX];>>
	| NE     <<#0=#["ne", AST_PREFIX];>>
	| LT_    <<#0=#["lt", AST_PREFIX];>>
	| LE     <<#0=#["le", AST_PREFIX];>>
	| GT     <<#0=#["gt", AST_PREFIX];>>
	| GE     <<#0=#["ge", AST_PREFIX];>>
	| IDENT  <<#0=#["is", AST_PREFIX];>>
	| UNION  <<#0=#["union", AST_PREFIX];>>
	| INTERSECT <<#0=#["intersect", AST_PREFIX];>>
	| EXCEPT  <<#0=#["except", AST_PREFIX];>>
	| ROLE    <<#0=#["ROLE", AST_PREFIX];>>
	| DATABASE <<#0=#["DATABASE", AST_PREFIX];>>
	| INDEX   <<#0=#["INDEX", AST_PREFIX];>>
	| FROM    <<#0=#["FROM", AST_PREFIX];>>
	| CCOLLECTION <<#0=#["COLLECTION", AST_PREFIX];>>
	| CIN_     <<#0=#["IN", AST_PREFIX];>>
	| STDIN    <<#0=#["STDIN", AST_PREFIX];>>
	| USER     <<#0=#["USER", AST_PREFIX];>>
	| PASSWORD <<#0=#["PASSWORD", AST_PREFIX];>>
	| METADATA <<#0=#["METADATA", AST_PREFIX];>>
	| FOR_     <<#0=#["FOR", AST_PREFIX];>>
	| DOCUMENTS <<#0=#["DOCUMENTS", AST_PREFIX];>>
	| COLLECTIONS <<#0=#["COLLECTIONS", AST_PREFIX];>>
	| DESCRIPTIVE <<#0=#["DESCRIPTIVE", AST_PREFIX];>>
	| SCHEMA      <<#0=#["SCHEMA", AST_PREFIX];>>
	| STATISTICS  <<#0=#["STATISTICS", AST_PREFIX];>>
	| INSERT      <<#0=#["insert", AST_PREFIX];>>
	| DELETE_     <<#0=#["delete", AST_PREFIX];>>
	| DELETE_UNDEEP <<#0=#["delete_undeep", AST_PREFIX];>>
	| REPLACE     <<#0=#["replace", AST_PREFIX];>>
	| RENAME      <<#0=#["rename", AST_PREFIX];>>
	| MOVE        <<#0=#["move", AST_PREFIX];>>
	| ALL         <<#0=#["ALL", AST_PREFIX];>>
	| PUBLIC      <<#0=#["PUBLIC", AST_PREFIX];>>
	| CTO         <<#0=#["TO", AST_PREFIX];>>
	| BOUNDARYSPACE <<#0=#["boundary-space", AST_PREFIX];>>
	| PRESERVE    <<#0=#["preserve", AST_PREFIX];>>
	| STRIP       <<#0=#["strip", AST_PREFIX];>>
	| OPTION       <<#0=#["option", AST_PREFIX];>>
	| LOADFILE       <<#0=#["LOADFILE", AST_PREFIX];>>
	| CDOCUMENT       <<#0=#["DOCUMENT", AST_PREFIX];>>
	| TYPESWITCH <<#0=#["typeswitch", AST_PREFIX];>>
	| CASE <<#0=#["case", AST_PREFIX];>>
	| LDOCUMENT <<#0=#["document", AST_PREFIX];>>
;

ncname!: 
	  l1:localPart1 <<#0=#l1;>>
	| l2:localPart2 <<#0=#l2;>>
;

/*
localPart!:
	(
  	  nn:NCNAME <<#0=#[$nn->getText(), AST_LOCAL_NAME];>>
	| DECLARE
	| FUNCTION
	| EXTERNAL
	| NAMESPACE
	| DEFAULT
	| FOR
	| LET
	| IN_
	| WHERE
	| STABLE
	| ORDER
	| BY
	| CBY
	| AT_
	| ASCENDING
	| DESCENDING
	| GREATEST
	| LEAST
	| COLLATION
	| RETURN
	| SOME
	| EVERY
	| SATISFIES
	| IF  <<#0=#["if", AST_LOCAL_NAME];>>
	| THEN
	| ELSE
	| ELEMENT
	| OF
	| TYPE
	| ATTRIBUTE

//	| XMLNS

	| DEFINE
	| EMPTY
	| ITEM
	| TEXT
	| NILLABLE
	| PROCESSING_INSTRUCTION
	| COMMENT_
	| NODE
	| DOCUMENT
	| CHILD
	| DESCENDANT
	| SELF
	| DESCENDANT_OR_SELF
	| FOLLOWING_SIBLING
	| FOLLOWING
	| PARENT
	| ANCESTOR
	| PRECEDING_SIBLING
	| PRECEDING
	| ANCESTOR_OR_SELF
	| CASTABLE
	| AS
	| CAS
	| CAST
	| TO
	| INSTANCE
	| TREAT
	| UPDATE
	| INTO
	| WITH
	| ON
	| CON
	| CREATE
	| DROP
	| GRANT
	| REVOKE
	| LOAD
	| ALTER
	| RETRIEVE
	| OR
	| AND
	| DIV
	| IDIV
	| MOD
	| EQ
	| NE
	| LT_
	| LE
	| GT
	| GE
	| EQUAL
	| NOTEQUAL
	| LESS
	| LESSEQUAL
	| IDENT
	| UNION
	| INTERSECT
	| EXCEPT
	| ROLE
	| DATABASE
	| INDEX
	| FROM
	| COLLECTION
	| IN__
	| STDIN
	| USER
	| PASSWORD
	| METADATA
	| FOR_
	| DOCUMENTS
	| COLLECTIONS
	| DESCRIPTIVE
	| SCHEMA
	| STATISTICS
	| INSERT
	| DELETE_
	| DELETE_UNDEEP
	| REPLACE
	| RENAME
	| MOVE
    )
;
*/

varRef!:
	DOLLAR qn:qname
/*
	<<char *loc_name=((AST *)(#qn->down()))->getText();
	  #0=#[string("$")+loc_name, AST_VAR];
	>>
*/

	<<#0 = #(#[AST_VAR], #qn);>>
;

enclosedExpr!:
	LBRACE e:expr RBRACE
	<<
	  #0=#e;
//	  #0=#(#[AST_SPACE_SEQUENCE], #e);
	>>
;

parenthesizedExpr!:
	
	LPAR {e:expr} RPAR 
	<<if (#e == NULL)
	  {
	     #0=#[AST_EMPTY_SEQUENCE];
	  }
	  else
	  {
	     #0=#e;
	  }
	>>
;

typeDeclaration!:
	AS st:sequenceType
	<<#0=#st;>>
;

}
