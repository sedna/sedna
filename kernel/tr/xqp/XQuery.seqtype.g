/************************************************************
********** XQuery types syntactic analizer rules ************
*************************************************************/

class XQueryParser {

singleType!:
	at:atomicType <<#0=#(#[AST_TYPE], #at);>>
	{QMARK <<#0->addChild(#["optional", AST_MULTIPLICITY]);>>}
;

sequenceType!:
	(  it:itemType {oi:occuranceIndicator}

	   <<#0=#(#[AST_TYPE], #it, #oi);>>

	 | EMPTYSEQ LPAR RPAR
	   <<#0=#(#[AST_TYPE], #[AST_EMPTY]);>>
       )
;

occuranceIndicator!:
	  q:QMARK  <<#0=#["optional", AST_MULTIPLICITY];>>
	| s:STAR   <<#0=#["zero-or-more", AST_MULTIPLICITY];>>
	| p:PLUS   <<#0=#["one-or-more", AST_MULTIPLICITY];>>
;

itemType!:
	  at:atomicType   <<#0=#at;>>
	| kt:kindTest     <<#0=#kt;>>
	| ITEM LPAR RPAR  <<#0=#[AST_ITEM_TEST];>>
;

atomicType!:
	qn:qname
	<<#0=#(#[AST_ATOMIC], #qn);>>
;

kindTest!:
	  dt:documentTest   <<#0=#dt;>>
	| et:elementTest    <<#0=#et;>>
	| at:attributeTest  <<#0=#at;>>
	| pit:piTest        <<#0=#pit;>>
	| ct:commnetTest    <<#0=#ct;>>
	| tt:textTest       <<#0=#tt;>>
	| akt:anyKindTest   <<#0=#akt;>>
;


piTest!:
	<<bool exist_arg=false;>>
	PROCESSING_INSTRUCTION LPAR
	{  
	   (  n:NCNAME <<#0=#(#[AST_PI], #[$n->getText(), AST_LOCAL_NAME]); exist_arg=true;>>
	    | s:STRINGLITERAL <<#0=#(#[AST_PI], #[$s->getText(), AST_STRING_CONST]); exist_arg=true;>>
	   )
	} RPAR
	<<if (!exist_arg)
	     #0=#[AST_PI];
	>>
;


commnetTest!:
	COMMENT_ LPAR RPAR
	<<#0=#[AST_COMMENT_TEST];>>
;

textTest!:
	TEXT LPAR RPAR
	<<#0=#[AST_TEXT_TEST];>>
;

anyKindTest!:
	NODE LPAR RPAR
	<<#0=#[AST_NODE_TEST];>>
; 	

documentTest!:
	DOCUMENT LPAR {et:elementTest} RPAR
	<<#0=#(#[AST_DOCUMENT_TEST], #et);>>
;

elementTest!:
	ELEMENT LPAR {etc:elementTestContent}
                RPAR
	<<if(#etc==NULL)
            #0 = #(#[AST_ELEMENT_TEST], #[AST_EMPTY_ELEMENT_CONTENT]);
	  else
	    #0=#etc;
	>>

;

elementTestContent!:
/*	  (cp:contextPath
	                 
	  <<#0=#(#[AST_ELEMENT_TEST], #cp);>>
	  )?//sintactic predicate

	|*/ enw:elemNameorWildcard {COMMA tnw:typeNameorWildcard {n:nillable}}

          <<#0=#(#[AST_ELEMENT_TEST], #enw, #tnw, #n);>>
;


nillable!:
	NILLABLE
	<<#0=#[AST_NIL];>>
;

attributeTest!:
	ATTRIBUTE LPAR {atc:attributeTestContent} RPAR

	<<if(#atc==NULL) 
	    #0 = #(#[AST_ATTRIBUTE_TEST], #[AST_EMPTY_ATTRIBUTE_CONTENT]);
	  else
	    #0=#atc;
	>>
;

attributeTestContent!:
/*	 (cp:contextPath 
	  <<#0=#(#[AST_ATTRIBUTE_TEST], #cp);>>
	 )?//syntactic predicate

	|*/ anw:attrNameorWildcard {COMMA tnw:typeNameorWildcard}

          <<#0=#(#[AST_ATTRIBUTE_TEST], #anw, #tnw);>>
;


elementName!:
	qn:qname
	<<#0=#(#[AST_ELEMENT_NAME], #qn);>>
;

attributeName!:
	qn:qname
	<<#0=#(#[AST_ATTRIBUTE_NAME], #qn);>>
;

typeName!:
	qn:qname
	<<#0=#(#[AST_TYPE_NAME], #qn);>>
;

elemNameorWildcard!:
	(  en:elementName <<#0=#en;>>
	 | STAR         <<#0=#[AST_WILDCARD];>>
	)
;

attrNameorWildcard!:
	(  an:attributeName <<#0=#an;>>
	 | STAR             <<#0=#[AST_WILDCARD];>> 
	)
;

typeNameorWildcard!:
	(  tn:typeName <<#0=#tn;>>
	 | STAR        <<#0=#[AST_WILDCARD];>>
	)
;

contextPath!:
	<<ASTBase* el_name=NULL;>>
	sgc:schemaGlobalContext  <<#0=#sgc;>>
	(SLASH scs:schemaContextStep 
	  <<if(LA(1)==SLASH)
	      #0->append(#scs);
	    else 
	      el_name=#scs;
	  >>
	)*

	<<#0=#(#[AST_SCHEMA_CONTEXT_PATH], #0);
	  #0->append(el_name);
	>>
;

/*
schemaContextPath!: 
	sgc:schemaGlobalContext SLASH <<#0=#sgc;>>
	(scs:schemaContextStep SLASH <<#0->append(#scs);>>)*
	<<#0=#(#[AST_SCHEMA_CONTEXT_PATH], #0);>>
;
*/

schemaGlobalContext!:
	(  qn:qname          <<#0=#(#[AST_GLOBAL_NAME], #qn);>>
	 | stn:schemaGlobalTypeName <<#0=#stn;>>
        )
;

schemaContextStep!:
	qn:qname
	<<#0=#qn;>>
;

schemaGlobalTypeName!:
	TYPE LPAR qn:qname RPAR

	<<#0=#(#[AST_GLOBAL_TYPE], #qn);>>
;
 

}