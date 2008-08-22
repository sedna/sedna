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
	| te:typeswitchExpr <<#0=#te;>>
	| ie:ifExpr         <<#0=#ie;>>
	| oe:orExpr         <<#0=#oe;>>
	| qe:quantifiedExpr <<#0=#qe;>>
//	  ie:ifExpr          <<#0=#ie;>>
;

/*
qname!:
	  l1:localPart1 <<#0=#(#[AST_QNAME], #l1);>>
	| l2:localPart2 <<#0=#(#[AST_QNAME], #l2);>>
	| q:FULLQNAME
	<<
	   std::string qn = $q->getText(), prefix, local;
	   int pos = qn.find(':');//always must found colon (:)
	   prefix = qn.substr(0, pos);
	   local = qn.substr(pos+1, qn.size()-(pos+1));
	   #0=#(#[AST_QNAME], #[local.c_str(), AST_LOCAL_NAME], #[prefix.c_str(), AST_PREFIX]);
	>>
;
*/


qname!:
     << ASTBase* pref = NULL, *local= NULL;>>
	( {(p1:prefixPart1 <<pref=#p1;>> | p2:prefixPart2 <<pref=#p2;>>| p3:prefixPart3 <<pref=#p3;>> | p4:prefixPart4 <<pref=#p4;>>) COLON} (l1:localPart1 <<local=#l1;>> | l2:localPart2 <<local=#l2;>> | l3:localPart3 <<local=#l3;>> | l4:localPart4 <<local=#l4;>>)
	)
	  <<
	     int line = pref == NULL ? ((AST*)local)->getLine() : ((AST*)pref)->getLine();
	     #0=#(#[AST_QNAME, line], local, pref);
	  >>

;

prefix!:
	  nn:NCNAME <<#0=#[$nn->getText(), AST_PREFIX, $nn->getLine()];>>
;

localPart1!: <<ANTLRTokenPtr tk = (ANTLRTokenPtr)LT(1);>>
  	  nn:NCNAME <<#0=#[tk->getText(), AST_LOCAL_NAME, tk->getLine()];>>
	| DECLARE <<#0=#["declare", AST_LOCAL_NAME, tk->getLine()];>>
	| FUNCTION <<#0=#["function", AST_LOCAL_NAME, tk->getLine()];>>
	| EXTERNAL <<#0=#["external", AST_LOCAL_NAME, tk->getLine()];>>
	| NAMESPACE <<#0=#["namespace", AST_LOCAL_NAME, tk->getLine()];>>
	| DEFAULT <<#0=#["default", AST_LOCAL_NAME, tk->getLine()];>>
	| FOR  <<#0=#["for", AST_LOCAL_NAME, tk->getLine()];>>
	| LET  <<#0=#["let", AST_LOCAL_NAME, tk->getLine()];>>
	| IN_  <<#0=#["in", AST_LOCAL_NAME, tk->getLine()];>>
	| WHERE <<#0=#["where", AST_LOCAL_NAME, tk->getLine()];>>
	| STABLE <<#0=#["stable", AST_LOCAL_NAME, tk->getLine()];>>
	| ORDER  <<#0=#["order", AST_LOCAL_NAME, tk->getLine()];>>
	| BY     <<#0=#["by", AST_LOCAL_NAME, tk->getLine()];>>
	| CBY    <<#0=#["BY", AST_LOCAL_NAME, tk->getLine()];>>
	| AT_    <<#0=#["at", AST_LOCAL_NAME, tk->getLine()];>>
	| ASCENDING <<#0=#["ascending", AST_LOCAL_NAME, tk->getLine()];>>
	| DESCENDING <<#0=#["descending", AST_LOCAL_NAME, tk->getLine()];>>
	| GREATEST  <<#0=#["greatest", AST_LOCAL_NAME, tk->getLine()];>>
	| LEAST     <<#0=#["least", AST_LOCAL_NAME, tk->getLine()];>>
	| COLLATION <<#0=#["collation", AST_LOCAL_NAME, tk->getLine()];>>
	| RETURN    <<#0=#["return", AST_LOCAL_NAME, tk->getLine()];>>
	| SOME      <<#0=#["some", AST_LOCAL_NAME, tk->getLine()];>>
	| EVERY     <<#0=#["every", AST_LOCAL_NAME, tk->getLine()];>>
	| SATISFIES <<#0=#["satisfies", AST_LOCAL_NAME, tk->getLine()];>>
	| IF  <<#0=#["if", AST_LOCAL_NAME, tk->getLine()];>>
	| THEN <<#0=#["then", AST_LOCAL_NAME, tk->getLine()];>>
	| ELSE <<#0=#["else", AST_LOCAL_NAME, tk->getLine()];>>
	| ELEMENT <<#0=#["element", AST_LOCAL_NAME, tk->getLine()];>>
	| OF      <<#0=#["of", AST_LOCAL_NAME, tk->getLine()];>>
	| TYPE    <<#0=#["type", AST_LOCAL_NAME, tk->getLine()];>>
	| ATTRIBUTE <<#0=#["attribute", AST_LOCAL_NAME, tk->getLine()];>>
	| XMLNS <<#0=#["xmlns", AST_LOCAL_NAME, tk->getLine()];>>
	| DEFINE <<#0=#["define", AST_LOCAL_NAME, tk->getLine()];>>
	| EMPTY  <<#0=#["empty", AST_LOCAL_NAME, tk->getLine()];>>
	| EMPTYSEQ  <<#0=#["empty-sequence", AST_LOCAL_NAME, tk->getLine()];>>
	| ITEM   <<#0=#["item", AST_LOCAL_NAME, tk->getLine()];>>
	| TEXT   <<#0=#["text", AST_LOCAL_NAME, tk->getLine()];>>
	| NILLABLE <<#0=#["nillable", AST_LOCAL_NAME, tk->getLine()];>>
	| PROCESSING_INSTRUCTION <<#0=#["processing-instruction", AST_LOCAL_NAME, tk->getLine()];>>
	| COMMENT_  <<#0=#["comment", AST_LOCAL_NAME, tk->getLine()];>>
	| NODE      <<#0=#["node", AST_LOCAL_NAME, tk->getLine()];>>
	| DOCUMENT  <<#0=#["document-node", AST_LOCAL_NAME, tk->getLine()];>>
	| CHILD     <<#0=#["child", AST_LOCAL_NAME, tk->getLine()];>>
	| DESCENDANT  <<#0=#["descendant", AST_LOCAL_NAME, tk->getLine()];>>
	| SELF       <<#0=#["self", AST_LOCAL_NAME, tk->getLine()];>>
	| DESCENDANT_OR_SELF <<#0=#["descendant-or-self", AST_LOCAL_NAME, tk->getLine()];>>
	| FOLLOWING_SIBLING <<#0=#["following-sibling", AST_LOCAL_NAME, tk->getLine()];>>
	| FOLLOWING   <<#0=#["following", AST_LOCAL_NAME, tk->getLine()];>>
	| PARENT   <<#0=#["parent", AST_LOCAL_NAME, tk->getLine()];>>
	| ANCESTOR <<#0=#["ancestor", AST_LOCAL_NAME, tk->getLine()];>>
	| PRECEDING_SIBLING <<#0=#["preceding-sibling", AST_LOCAL_NAME, tk->getLine()];>>
	| PRECEDING  <<#0=#["preceding", AST_LOCAL_NAME, tk->getLine()];>>
	| ANCESTOR_OR_SELF <<#0=#["ancestor-or-self", AST_LOCAL_NAME, tk->getLine()];>>
	| CASTABLE  <<#0=#["castable", AST_LOCAL_NAME, tk->getLine()];>>
;
      
localPart2!: <<ANTLRTokenPtr tk = (ANTLRTokenPtr)LT(1);>>
	  AS  <<#0=#["as", AST_LOCAL_NAME, tk->getLine()];>>
	| CAS <<#0=#["AS", AST_LOCAL_NAME, tk->getLine()];>>
	| CAST <<#0=#["cast", AST_LOCAL_NAME, tk->getLine()];>>
	| TO   <<#0=#["to", AST_LOCAL_NAME, tk->getLine()];>>
	| INSTANCE <<#0=#["instance", AST_LOCAL_NAME, tk->getLine()];>>
	| TREAT <<#0=#["treat", AST_LOCAL_NAME, tk->getLine()];>>
	| UPDATE <<#0=#["UPDATE", AST_LOCAL_NAME, tk->getLine()];>>
	| INTO <<#0=#["into", AST_LOCAL_NAME, tk->getLine()];>>
	| WITH  <<#0=#["with", AST_LOCAL_NAME, tk->getLine()];>>
	| ON    <<#0=#["on", AST_LOCAL_NAME, tk->getLine()];>>
	| CON   <<#0=#["ON", AST_LOCAL_NAME, tk->getLine()];>>
	| CREATE <<#0=#["CREATE", AST_LOCAL_NAME, tk->getLine()];>>
	| DROP  <<#0=#["DROP", AST_LOCAL_NAME, tk->getLine()];>>
	| GRANT <<#0=#["GRANT", AST_LOCAL_NAME, tk->getLine()];>>
	| REVOKE <<#0=#["REVOKE", AST_LOCAL_NAME, tk->getLine()];>>
	| LOAD   <<#0=#["LOAD", AST_LOCAL_NAME, tk->getLine()];>>
	| ALTER  <<#0=#["ALTER", AST_LOCAL_NAME, tk->getLine()];>>
	| RETRIEVE <<#0=#["RETRIEVE", AST_LOCAL_NAME, tk->getLine()];>>
	| OR     <<#0=#["or", AST_LOCAL_NAME, tk->getLine()];>>
	| AND    <<#0=#["and", AST_LOCAL_NAME, tk->getLine()];>>
	| DIV    <<#0=#["div", AST_LOCAL_NAME, tk->getLine()];>>
	| IDIV   <<#0=#["idiv", AST_LOCAL_NAME, tk->getLine()];>>
	| MOD    <<#0=#["mod", AST_LOCAL_NAME, tk->getLine()];>>
	| EQ     <<#0=#["eq", AST_LOCAL_NAME, tk->getLine()];>>
	| NE     <<#0=#["ne", AST_LOCAL_NAME, tk->getLine()];>>
	| LT_    <<#0=#["lt", AST_LOCAL_NAME, tk->getLine()];>>
	| LE     <<#0=#["le", AST_LOCAL_NAME, tk->getLine()];>>
	| GT     <<#0=#["gt", AST_LOCAL_NAME, tk->getLine()];>>
	| GE     <<#0=#["ge", AST_LOCAL_NAME, tk->getLine()];>>
	| IDENT  <<#0=#["is", AST_LOCAL_NAME, tk->getLine()];>>
	| UNION  <<#0=#["union", AST_LOCAL_NAME, tk->getLine()];>>
	| INTERSECT <<#0=#["intersect", AST_LOCAL_NAME, tk->getLine()];>>
	| EXCEPT  <<#0=#["except", AST_LOCAL_NAME, tk->getLine()];>>
	| ROLE    <<#0=#["ROLE", AST_LOCAL_NAME, tk->getLine()];>>
	| DATABASE <<#0=#["DATABASE", AST_LOCAL_NAME, tk->getLine()];>>
	| INDEX   <<#0=#["INDEX", AST_LOCAL_NAME, tk->getLine()];>>
	| FROM    <<#0=#["FROM", AST_LOCAL_NAME, tk->getLine()];>>
	| CCOLLECTION <<#0=#["COLLECTION", AST_LOCAL_NAME, tk->getLine()];>>
	| CIN_     <<#0=#["IN", AST_LOCAL_NAME, tk->getLine()];>>
	| STDIN    <<#0=#["STDIN", AST_LOCAL_NAME, tk->getLine()];>>
	| USER     <<#0=#["USER", AST_LOCAL_NAME, tk->getLine()];>>
	| PASSWORD <<#0=#["PASSWORD", AST_LOCAL_NAME, tk->getLine()];>>
	| METADATA <<#0=#["METADATA", AST_LOCAL_NAME, tk->getLine()];>>
	| FOR_     <<#0=#["FOR", AST_LOCAL_NAME, tk->getLine()];>>
	| DOCUMENTS <<#0=#["DOCUMENTS", AST_LOCAL_NAME, tk->getLine()];>>
	| COLLECTIONS <<#0=#["COLLECTIONS", AST_LOCAL_NAME, tk->getLine()];>>
	| DESCRIPTIVE <<#0=#["DESCRIPTIVE", AST_LOCAL_NAME, tk->getLine()];>>
	| SCHEMA      <<#0=#["SCHEMA", AST_LOCAL_NAME, tk->getLine()];>>
	| STATISTICS  <<#0=#["STATISTICS", AST_LOCAL_NAME, tk->getLine()];>>
	| INSERT      <<#0=#["insert", AST_LOCAL_NAME, tk->getLine()];>>
	| DELETE_     <<#0=#["delete", AST_LOCAL_NAME, tk->getLine()];>>
	| DELETE_UNDEEP <<#0=#["delete_undeep", AST_LOCAL_NAME, tk->getLine()];>>
	| REPLACE     <<#0=#["replace", AST_LOCAL_NAME, tk->getLine()];>>
	| RENAME      <<#0=#["rename", AST_LOCAL_NAME, tk->getLine()];>>
	| MOVE        <<#0=#["move", AST_LOCAL_NAME, tk->getLine()];>>
	| ALL         <<#0=#["ALL", AST_LOCAL_NAME, tk->getLine()];>>
	| PUBLIC      <<#0=#["PUBLIC", AST_LOCAL_NAME, tk->getLine()];>>
	| CTO         <<#0=#["TO", AST_LOCAL_NAME, tk->getLine()];>>
	| BOUNDARYSPACE <<#0=#["boundary-space", AST_LOCAL_NAME, tk->getLine()];>>
	| PRESERVE    <<#0=#["preserve", AST_LOCAL_NAME, tk->getLine()];>>
	| STRIP       <<#0=#["strip", AST_LOCAL_NAME, tk->getLine()];>>
	| OPTION       <<#0=#["option", AST_LOCAL_NAME, tk->getLine()];>>
	| LOADFILE       <<#0=#["LOADFILE", AST_LOCAL_NAME, tk->getLine()];>>
	| CDOCUMENT       <<#0=#["DOCUMENT", AST_LOCAL_NAME, tk->getLine()];>>
	| CASE_ <<#0=#["case", AST_LOCAL_NAME, tk->getLine()];>>
	| TYPESWITCH <<#0=#["typeswitch", AST_LOCAL_NAME, tk->getLine()];>>
	   
;

localPart3!: <<ANTLRTokenPtr tk = (ANTLRTokenPtr)LT(1);>>
	  LDOCUMENT <<#0=#["document", AST_LOCAL_NAME, tk->getLine()];>>
	| IMPORT <<#0=#["import", AST_LOCAL_NAME, tk->getLine()];>>
	| MODULE <<#0=#["module", AST_LOCAL_NAME, tk->getLine()];>>
	| VALIDATE <<#0=#["validate", AST_LOCAL_NAME, tk->getLine()];>>
	| LAX <<#0=#["lax", AST_LOCAL_NAME, tk->getLine()];>>
	| STRICT_ <<#0=#["strict", AST_LOCAL_NAME, tk->getLine()];>>
	| SSHEMA <<#0=#["schema", AST_LOCAL_NAME, tk->getLine()];>>
	| BASEURI <<#0=#["base-uri", AST_LOCAL_NAME, tk->getLine()];>>
	| CONSTRUCTION <<#0=#["construction", AST_LOCAL_NAME, tk->getLine()];>>
	| ORDERING <<#0=#["ordering", AST_LOCAL_NAME, tk->getLine()];>>
	| ORDERED <<#0=#["ordered", AST_LOCAL_NAME, tk->getLine()];>>
	| UNORDERED <<#0=#["unordered", AST_LOCAL_NAME, tk->getLine()];>>
	| COPYNAMESPACE <<#0=#["copy-namespaces", AST_LOCAL_NAME, tk->getLine()];>>
	| NOPRESERVE <<#0=#["no-preserve", AST_LOCAL_NAME, tk->getLine()];>>
	| INHERIT <<#0=#["inherit", AST_LOCAL_NAME, tk->getLine()];>>
	| NOINHERIT <<#0=#["no-inherit", AST_LOCAL_NAME, tk->getLine()];>>
	| VARIABLE <<#0=#["variable", AST_LOCAL_NAME, tk->getLine()];>>
	| TRIGGER <<#0=#["TRIGGER", AST_LOCAL_NAME, tk->getLine()];>>
	| BEFORE <<#0=#["BEFORE", AST_LOCAL_NAME, tk->getLine()];>>
	| AFTER <<#0=#["AFTER", AST_LOCAL_NAME, tk->getLine()];>>
	| CINSERT <<#0=#["INSERT", AST_LOCAL_NAME, tk->getLine()];>>
	| CDELETE <<#0=#["DELETE", AST_LOCAL_NAME, tk->getLine()];>>
	| CREPLACE <<#0=#["REPLACE", AST_LOCAL_NAME, tk->getLine()];>>
	| EACH <<#0=#["EACH", AST_LOCAL_NAME, tk->getLine()];>>
	| CNODE <<#0=#["NODE", AST_LOCAL_NAME, tk->getLine()];>>
	| STATEMENT <<#0=#["STATEMENT", AST_LOCAL_NAME, tk->getLine()];>>
	| CDO <<#0=#["DO", AST_LOCAL_NAME, tk->getLine()];>>
	| XQUERY <<#0=#["xquery", AST_LOCAL_NAME, tk->getLine()];>>
	| VERSION <<#0=#["version", AST_LOCAL_NAME, tk->getLine()];>>
	| ENCODING <<#0=#["encoding", AST_LOCAL_NAME, tk->getLine()];>>
	| CMODULE <<#0=#["MODULE", AST_LOCAL_NAME, tk->getLine()];>>
	| COR <<#0=#["OR", AST_LOCAL_NAME, tk->getLine()];>>
;	


// sed 's/"$/" dummy/g' |
// awk '{print "\t| "$2, "<<#0=#["$3", AST_LOCAL_NAME]; $l->getLine();>>"}'
localPart4!: <<ANTLRTokenPtr tk = (ANTLRTokenPtr)LT(1);>>
	CREATE_LOWCASE <<#0=#["create", AST_LOCAL_NAME, tk->getLine()];>>
	| DROP_LOWCASE <<#0=#["drop", AST_LOCAL_NAME, tk->getLine()];>>
	| RENAME_LOWCASE <<#0=#["rename", AST_LOCAL_NAME, tk->getLine()];>>
    | INTO_LOWCASE <<#0=#["into", AST_LOCAL_NAME, tk->getLine()];>>
	| GRANT_LOWCASE <<#0=#["grant", AST_LOCAL_NAME, tk->getLine()];>>
	| REVOKE_LOWCASE <<#0=#["revoke", AST_LOCAL_NAME, tk->getLine()];>>
	| LOADFILE_LOWCASE <<#0=#["loadfile", AST_LOCAL_NAME, tk->getLine()];>>
	| LOAD_LOWCASE <<#0=#["load", AST_LOCAL_NAME, tk->getLine()];>>
	| ALTER_LOWCASE <<#0=#["alter", AST_LOCAL_NAME, tk->getLine()];>>
	| ROLE_LOWCASE <<#0=#["role", AST_LOCAL_NAME, tk->getLine()];>>
	| DATABASE_LOWCASE <<#0=#["database", AST_LOCAL_NAME, tk->getLine()];>>
	| INDEX_LOWCASE <<#0=#["index", AST_LOCAL_NAME, tk->getLine()];>>
	| FULLTEXT_LOWCASE <<#0=#["full-text", AST_LOCAL_NAME, tk->getLine()];>>
	| FROM_LOWCASE <<#0=#["from", AST_LOCAL_NAME, tk->getLine()];>>
	| STDIN_LOWCASE <<#0=#["stdin", AST_LOCAL_NAME, tk->getLine()];>>
	| USER_LOWCASE <<#0=#["user", AST_LOCAL_NAME, tk->getLine()];>>
	| PASSWORD_LOWCASE <<#0=#["password", AST_LOCAL_NAME, tk->getLine()];>>
	| ALL_LOWCASE <<#0=#["all", AST_LOCAL_NAME, tk->getLine()];>>
	| PUBLIC_LOWCASE <<#0=#["public", AST_LOCAL_NAME, tk->getLine()];>>
	| TRIGGER_LOWCASE <<#0=#["trigger", AST_LOCAL_NAME, tk->getLine()];>>
	| BEFORE_LOWCASE <<#0=#["before", AST_LOCAL_NAME, tk->getLine()];>>
	| AFTER_LOWCASE <<#0=#["after", AST_LOCAL_NAME, tk->getLine()];>>
	| CINSERT_LOWCASE <<#0=#["insert", AST_LOCAL_NAME, tk->getLine()];>>
	| CDELETE_LOWCASE <<#0=#["delete", AST_LOCAL_NAME, tk->getLine()];>>
	| CREPLACE_LOWCASE <<#0=#["replace", AST_LOCAL_NAME, tk->getLine()];>>
	| EACH_LOWCASE <<#0=#["each", AST_LOCAL_NAME, tk->getLine()];>>
	| STATEMENT_LOWCASE <<#0=#["statement", AST_LOCAL_NAME, tk->getLine()];>> 
	| CDO_LOWCASE <<#0=#["do", AST_LOCAL_NAME, tk->getLine()];>>
	| RETRIEVE_LOWCASE <<#0=#["retrieve", AST_LOCAL_NAME, tk->getLine()];>>
	| METADATA_LOWCASE <<#0=#["metadata", AST_LOCAL_NAME, tk->getLine()];>>
	| DOCUMENTS_LOWCASE <<#0=#["documents", AST_LOCAL_NAME, tk->getLine()];>>
	| COLLECTIONS_LOWCASE <<#0=#["collections", AST_LOCAL_NAME, tk->getLine()];>>
	| CCOLLECTION_LOWCASE <<#0=#["collection", AST_LOCAL_NAME, tk->getLine()];>>
	| DESCRIPTIVE_LOWCASE <<#0=#["descriptive", AST_LOCAL_NAME, tk->getLine()];>>
	| STATISTICS_LOWCASE <<#0=#["statistics", AST_LOCAL_NAME, tk->getLine()];>>
	| UPDATE_LOWCASE <<#0=#["update", AST_LOCAL_NAME, tk->getLine()];>>
;

prefixPart1!: <<ANTLRTokenPtr tk = (ANTLRTokenPtr)LT(1);>>
  	  nn:NCNAME <<#0=#[tk->getText(), AST_PREFIX, tk->getLine()];>>
	| DECLARE <<#0=#["declare", AST_PREFIX, tk->getLine()];>>
	| FUNCTION <<#0=#["function", AST_PREFIX, tk->getLine()];>>
	| EXTERNAL <<#0=#["external", AST_PREFIX, tk->getLine()];>>
	| NAMESPACE <<#0=#["namespace", AST_PREFIX, tk->getLine()];>>
	| DEFAULT <<#0=#["default", AST_PREFIX, tk->getLine()];>>
	| FOR  <<#0=#["for", AST_PREFIX, tk->getLine()];>>
	| LET  <<#0=#["let", AST_PREFIX, tk->getLine()];>>
	| IN_  <<#0=#["in", AST_PREFIX, tk->getLine()];>>
	| WHERE <<#0=#["where", AST_PREFIX, tk->getLine()];>>
	| STABLE <<#0=#["stable", AST_PREFIX, tk->getLine()];>>
	| ORDER  <<#0=#["order", AST_PREFIX, tk->getLine()];>>
	| BY     <<#0=#["by", AST_PREFIX, tk->getLine()];>>
	| CBY    <<#0=#["BY", AST_PREFIX, tk->getLine()];>>
	| AT_    <<#0=#["at", AST_PREFIX, tk->getLine()];>>
	| ASCENDING <<#0=#["ascending", AST_PREFIX, tk->getLine()];>>
	| DESCENDING <<#0=#["descending", AST_PREFIX, tk->getLine()];>>
	| GREATEST  <<#0=#["greatest", AST_PREFIX, tk->getLine()];>>
	| LEAST     <<#0=#["least", AST_PREFIX, tk->getLine()];>>
	| COLLATION <<#0=#["collation", AST_PREFIX, tk->getLine()];>>
	| RETURN    <<#0=#["return", AST_PREFIX, tk->getLine()];>>
	| SOME      <<#0=#["some", AST_PREFIX, tk->getLine()];>>
	| EVERY     <<#0=#["every", AST_PREFIX, tk->getLine()];>>
	| SATISFIES <<#0=#["satisfies", AST_PREFIX, tk->getLine()];>>
	| IF  <<#0=#["if", AST_PREFIX, tk->getLine()];>>
	| THEN <<#0=#["then", AST_PREFIX, tk->getLine()];>>
	| ELSE <<#0=#["else", AST_PREFIX, tk->getLine()];>>
	| ELEMENT <<#0=#["element", AST_PREFIX, tk->getLine()];>>
	| OF      <<#0=#["of", AST_PREFIX, tk->getLine()];>>
	| TYPE    <<#0=#["type", AST_PREFIX, tk->getLine()];>>
	| ATTRIBUTE <<#0=#["attribute", AST_PREFIX, tk->getLine()];>>
	| XMLNS <<#0=#["xmlns", AST_PREFIX, tk->getLine()];>>
	| DEFINE <<#0=#["define", AST_PREFIX, tk->getLine()];>>
	| EMPTYSEQ  <<#0=#["empty-sequence", AST_LOCAL_NAME, tk->getLine()];>>
	| EMPTY  <<#0=#["empty", AST_PREFIX, tk->getLine()];>>
	| ITEM   <<#0=#["item", AST_PREFIX, tk->getLine()];>>
	| TEXT   <<#0=#["text", AST_PREFIX, tk->getLine()];>>
	| NILLABLE <<#0=#["nillable", AST_PREFIX, tk->getLine()];>>
	| PROCESSING_INSTRUCTION <<#0=#["processing-instruction", AST_PREFIX, tk->getLine()];>>
	| COMMENT_  <<#0=#["comment", AST_PREFIX, tk->getLine()];>>
	| NODE      <<#0=#["node", AST_PREFIX, tk->getLine()];>>
	| DOCUMENT  <<#0=#["document-node", AST_PREFIX, tk->getLine()];>>
	| CHILD     <<#0=#["child", AST_PREFIX, tk->getLine()];>>
	| DESCENDANT  <<#0=#["descendant", AST_PREFIX, tk->getLine()];>>
	| SELF       <<#0=#["self", AST_PREFIX, tk->getLine()];>>
	| DESCENDANT_OR_SELF <<#0=#["descendant-or-self", AST_PREFIX, tk->getLine()];>>
	| FOLLOWING_SIBLING <<#0=#["following-sibling", AST_PREFIX, tk->getLine()];>>
	| FOLLOWING   <<#0=#["following", AST_PREFIX, tk->getLine()];>>
	| PARENT   <<#0=#["parent", AST_PREFIX, tk->getLine()];>>
	| ANCESTOR <<#0=#["ancestor", AST_PREFIX, tk->getLine()];>>
	| PRECEDING_SIBLING <<#0=#["preceding-sibling", AST_PREFIX, tk->getLine()];>>
	| PRECEDING  <<#0=#["preceding", AST_PREFIX, tk->getLine()];>>
	| ANCESTOR_OR_SELF <<#0=#["ancestor-or-self", AST_PREFIX, tk->getLine()];>>
	| CASTABLE  <<#0=#["castable", AST_PREFIX, tk->getLine()];>>
;

prefixPart2!: <<ANTLRTokenPtr tk = (ANTLRTokenPtr)LT(1);>>
	  AS  <<#0=#["as", AST_PREFIX, tk->getLine()];>>
	| CAS <<#0=#["AS", AST_PREFIX, tk->getLine()];>>
	| CAST <<#0=#["cast", AST_PREFIX, tk->getLine()];>>
	| TO   <<#0=#["to", AST_PREFIX, tk->getLine()];>>
	| INSTANCE <<#0=#["instance", AST_PREFIX, tk->getLine()];>>
	| TREAT <<#0=#["treat", AST_PREFIX, tk->getLine()];>>
	| UPDATE <<#0=#["UPDATE", AST_PREFIX, tk->getLine()];>>
	| INTO <<#0=#["into", AST_PREFIX, tk->getLine()];>>
	| WITH  <<#0=#["with", AST_PREFIX, tk->getLine()];>>
	| ON    <<#0=#["on", AST_PREFIX, tk->getLine()];>>
	| CON   <<#0=#["ON", AST_PREFIX, tk->getLine()];>>
	| CREATE <<#0=#["CREATE", AST_PREFIX, tk->getLine()];>>
	| DROP  <<#0=#["DROP", AST_PREFIX, tk->getLine()];>>
	| GRANT <<#0=#["GRANT", AST_PREFIX, tk->getLine()];>>
	| REVOKE <<#0=#["REVOKE", AST_PREFIX, tk->getLine()];>>
	| LOAD   <<#0=#["LOAD", AST_PREFIX, tk->getLine()];>>
	| ALTER  <<#0=#["ALTER", AST_PREFIX, tk->getLine()];>>
	| RETRIEVE <<#0=#["RETRIEVE", AST_PREFIX, tk->getLine()];>>
	| OR     <<#0=#["or", AST_PREFIX, tk->getLine()];>>
	| AND    <<#0=#["and", AST_PREFIX, tk->getLine()];>>
	| DIV    <<#0=#["div", AST_PREFIX, tk->getLine()];>>
	| IDIV   <<#0=#["idiv", AST_PREFIX, tk->getLine()];>>
	| MOD    <<#0=#["mod", AST_PREFIX, tk->getLine()];>>
	| EQ     <<#0=#["eq", AST_PREFIX, tk->getLine()];>>
	| NE     <<#0=#["ne", AST_PREFIX, tk->getLine()];>>
	| LT_    <<#0=#["lt", AST_PREFIX, tk->getLine()];>>
	| LE     <<#0=#["le", AST_PREFIX, tk->getLine()];>>
	| GT     <<#0=#["gt", AST_PREFIX, tk->getLine()];>>
	| GE     <<#0=#["ge", AST_PREFIX, tk->getLine()];>>
	| IDENT  <<#0=#["is", AST_PREFIX, tk->getLine()];>>
	| UNION  <<#0=#["union", AST_PREFIX, tk->getLine()];>>
	| INTERSECT <<#0=#["intersect", AST_PREFIX, tk->getLine()];>>
	| EXCEPT  <<#0=#["except", AST_PREFIX, tk->getLine()];>>
	| ROLE    <<#0=#["ROLE", AST_PREFIX, tk->getLine()];>>
	| DATABASE <<#0=#["DATABASE", AST_PREFIX, tk->getLine()];>>
	| INDEX   <<#0=#["INDEX", AST_PREFIX, tk->getLine()];>>
	| FROM    <<#0=#["FROM", AST_PREFIX, tk->getLine()];>>
	| CCOLLECTION <<#0=#["COLLECTION", AST_PREFIX, tk->getLine()];>>
	| CIN_     <<#0=#["IN", AST_PREFIX, tk->getLine()];>>
	| STDIN    <<#0=#["STDIN", AST_PREFIX, tk->getLine()];>>
	| USER     <<#0=#["USER", AST_PREFIX, tk->getLine()];>>
	| PASSWORD <<#0=#["PASSWORD", AST_PREFIX, tk->getLine()];>>
	| METADATA <<#0=#["METADATA", AST_PREFIX, tk->getLine()];>>
	| FOR_     <<#0=#["FOR", AST_PREFIX, tk->getLine()];>>
	| DOCUMENTS <<#0=#["DOCUMENTS", AST_PREFIX, tk->getLine()];>>
	| COLLECTIONS <<#0=#["COLLECTIONS", AST_PREFIX, tk->getLine()];>>
	| DESCRIPTIVE <<#0=#["DESCRIPTIVE", AST_PREFIX, tk->getLine()];>>
	| SCHEMA      <<#0=#["SCHEMA", AST_PREFIX, tk->getLine()];>>
	| STATISTICS  <<#0=#["STATISTICS", AST_PREFIX, tk->getLine()];>>
	| INSERT      <<#0=#["insert", AST_PREFIX, tk->getLine()];>>
	| DELETE_     <<#0=#["delete", AST_PREFIX, tk->getLine()];>>
	| DELETE_UNDEEP <<#0=#["delete_undeep", AST_PREFIX, tk->getLine()];>>
	| REPLACE     <<#0=#["replace", AST_PREFIX, tk->getLine()];>>
	| RENAME      <<#0=#["rename", AST_PREFIX, tk->getLine()];>>
	| MOVE        <<#0=#["move", AST_PREFIX, tk->getLine()];>>
	| ALL         <<#0=#["ALL", AST_PREFIX, tk->getLine()];>>
	| PUBLIC      <<#0=#["PUBLIC", AST_PREFIX, tk->getLine()];>>
	| CTO         <<#0=#["TO", AST_PREFIX, tk->getLine()];>>
	| BOUNDARYSPACE <<#0=#["boundary-space", AST_PREFIX, tk->getLine()];>>
	| PRESERVE    <<#0=#["preserve", AST_PREFIX, tk->getLine()];>>
	| STRIP       <<#0=#["strip", AST_PREFIX, tk->getLine()];>>
	| OPTION       <<#0=#["option", AST_PREFIX, tk->getLine()];>>
	| LOADFILE       <<#0=#["LOADFILE", AST_PREFIX, tk->getLine()];>>
	| CDOCUMENT       <<#0=#["DOCUMENT", AST_PREFIX, tk->getLine()];>>
	| CASE_ <<#0=#["case", AST_PREFIX, tk->getLine()];>>
	| TYPESWITCH <<#0=#["typeswitch", AST_PREFIX, tk->getLine()];>>
;

prefixPart3!: <<ANTLRTokenPtr tk = (ANTLRTokenPtr)LT(1);>>
	  LDOCUMENT <<#0=#["document", AST_PREFIX, tk->getLine()];>>
	| IMPORT <<#0=#["import", AST_PREFIX, tk->getLine()];>>
	| MODULE <<#0=#["module", AST_PREFIX, tk->getLine()];>>
	| VALIDATE <<#0=#["validate", AST_PREFIX, tk->getLine()];>>
	| LAX <<#0=#["lax", AST_PREFIX, tk->getLine()];>>
	| STRICT_ <<#0=#["strict", AST_PREFIX, tk->getLine()];>>
	| SSHEMA <<#0=#["schema", AST_PREFIX, tk->getLine()];>>
	| BASEURI <<#0=#["base-uri", AST_PREFIX, tk->getLine()];>>
	| CONSTRUCTION <<#0=#["construction", AST_PREFIX, tk->getLine()];>>
	| ORDERING <<#0=#["ordering", AST_PREFIX, tk->getLine()];>>
	| ORDERED <<#0=#["ordered", AST_PREFIX, tk->getLine()];>>
	| UNORDERED <<#0=#["unordered", AST_PREFIX, tk->getLine()];>>
	| COPYNAMESPACE <<#0=#["copy-namespaces", AST_PREFIX, tk->getLine()];>>
	| NOPRESERVE <<#0=#["no-preserve", AST_PREFIX, tk->getLine()];>>
	| INHERIT <<#0=#["inherit", AST_PREFIX, tk->getLine()];>>
	| NOINHERIT <<#0=#["no-inherit", AST_PREFIX, tk->getLine()];>>
	| VARIABLE <<#0=#["variable", AST_PREFIX, tk->getLine()];>>
	| TRIGGER <<#0=#["TRIGGER", AST_PREFIX, tk->getLine()];>>
	| BEFORE <<#0=#["BEFORE", AST_PREFIX, tk->getLine()];>>
	| AFTER <<#0=#["AFTER", AST_PREFIX, tk->getLine()];>>
	| CINSERT <<#0=#["INSERT", AST_PREFIX, tk->getLine()];>>
	| CDELETE <<#0=#["DELETE", AST_PREFIX, tk->getLine()];>>
	| CREPLACE <<#0=#["REPLACE", AST_PREFIX, tk->getLine()];>>
	| EACH <<#0=#["EACH", AST_PREFIX, tk->getLine()];>>
	| CNODE <<#0=#["NODE", AST_PREFIX, tk->getLine()];>>
	| STATEMENT <<#0=#["STATEMENT", AST_PREFIX, tk->getLine()];>>
	| CDO <<#0=#["DO", AST_PREFIX, tk->getLine()];>>
	| XQUERY <<#0=#["xquery", AST_PREFIX, tk->getLine()];>>
	| VERSION <<#0=#["version", AST_PREFIX, tk->getLine()];>>
	| ENCODING <<#0=#["encoding", AST_PREFIX, tk->getLine()];>>
	| CMODULE <<#0=#["MODULE", AST_PREFIX, tk->getLine()];>>
	| COR <<#0=#["OR", AST_PREFIX, tk->getLine()];>>
;

prefixPart4!: <<ANTLRTokenPtr tk = (ANTLRTokenPtr)LT(1);>>
	CREATE_LOWCASE <<#0=#["create", AST_LOCAL_NAME, tk->getLine()];>>
	| DROP_LOWCASE <<#0=#["drop", AST_LOCAL_NAME, tk->getLine()];>>
	| RENAME_LOWCASE <<#0=#["rename", AST_LOCAL_NAME, tk->getLine()];>>
    | INTO_LOWCASE <<#0=#["into", AST_LOCAL_NAME, tk->getLine()];>>
	| GRANT_LOWCASE <<#0=#["grant", AST_LOCAL_NAME, tk->getLine()];>>
	| REVOKE_LOWCASE <<#0=#["revoke", AST_LOCAL_NAME, tk->getLine()];>>
	| LOADFILE_LOWCASE <<#0=#["loadfile", AST_LOCAL_NAME, tk->getLine()];>>
	| LOAD_LOWCASE <<#0=#["load", AST_LOCAL_NAME, tk->getLine()];>>
	| ALTER_LOWCASE <<#0=#["alter", AST_LOCAL_NAME, tk->getLine()];>>
	| ROLE_LOWCASE <<#0=#["role", AST_LOCAL_NAME, tk->getLine()];>>
	| DATABASE_LOWCASE <<#0=#["database", AST_LOCAL_NAME, tk->getLine()];>>
	| INDEX_LOWCASE <<#0=#["index", AST_LOCAL_NAME, tk->getLine()];>>
	| FULLTEXT_LOWCASE <<#0=#["full-text", AST_LOCAL_NAME, tk->getLine()];>>
	| FROM_LOWCASE <<#0=#["from", AST_LOCAL_NAME, tk->getLine()];>>
	| STDIN_LOWCASE <<#0=#["stdin", AST_LOCAL_NAME, tk->getLine()];>>
	| USER_LOWCASE <<#0=#["user", AST_LOCAL_NAME, tk->getLine()];>>
	| PASSWORD_LOWCASE <<#0=#["password", AST_LOCAL_NAME, tk->getLine()];>>
	| ALL_LOWCASE <<#0=#["all", AST_LOCAL_NAME, tk->getLine()];>>
	| PUBLIC_LOWCASE <<#0=#["public", AST_LOCAL_NAME, tk->getLine()];>>
	| TRIGGER_LOWCASE <<#0=#["trigger", AST_LOCAL_NAME, tk->getLine()];>>
	| BEFORE_LOWCASE <<#0=#["before", AST_LOCAL_NAME, tk->getLine()];>>
	| AFTER_LOWCASE <<#0=#["after", AST_LOCAL_NAME, tk->getLine()];>>
	| CINSERT_LOWCASE <<#0=#["insert", AST_LOCAL_NAME, tk->getLine()];>>
	| CDELETE_LOWCASE <<#0=#["delete", AST_LOCAL_NAME, tk->getLine()];>>
	| CREPLACE_LOWCASE <<#0=#["replace", AST_LOCAL_NAME, tk->getLine()];>>
	| EACH_LOWCASE <<#0=#["each", AST_LOCAL_NAME, tk->getLine()];>>
	| STATEMENT_LOWCASE <<#0=#["statement", AST_LOCAL_NAME, tk->getLine()];>> 
	| CDO_LOWCASE <<#0=#["do", AST_LOCAL_NAME, tk->getLine()];>>
	| RETRIEVE_LOWCASE <<#0=#["retrieve", AST_LOCAL_NAME, tk->getLine()];>>
	| METADATA_LOWCASE <<#0=#["metadata", AST_LOCAL_NAME, tk->getLine()];>>
	| DOCUMENTS_LOWCASE <<#0=#["documents", AST_LOCAL_NAME, tk->getLine()];>>
	| COLLECTIONS_LOWCASE <<#0=#["collections", AST_LOCAL_NAME, tk->getLine()];>>
	| CCOLLECTION_LOWCASE <<#0=#["collection", AST_LOCAL_NAME, tk->getLine()];>>
	| DESCRIPTIVE_LOWCASE <<#0=#["descriptive", AST_LOCAL_NAME, tk->getLine()];>>
	| STATISTICS_LOWCASE <<#0=#["statistics", AST_LOCAL_NAME, tk->getLine()];>>
	| UPDATE_LOWCASE <<#0=#["update", AST_LOCAL_NAME, tk->getLine()];>>
;

ncname!: 
	  l1:localPart1 <<#0=#l1;>>
	| l2:localPart2 <<#0=#l2;>>
	| l3:localPart3 <<#0=#l3;>>
	| l4:localPart4 <<#0=#l4;>>
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
