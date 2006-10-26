/**/

class XQueryParser

{

queryProlog!:
	<<ASTBase *prolog=NULL;>>
	( ( fd:functionDefn
	    <<if(prolog == NULL) prolog=#fd; else prolog->append(#fd);>>

	   | nd:namespaceDecl
	     <<if(prolog == NULL) prolog=#nd; else prolog->append(#nd);>>

	   | dds:defaultDecls
	     <<if(prolog == NULL) prolog=#dds; else prolog->append(#dds);>>

	   | bsd:boundarySpaceDecl
	     <<if(prolog == NULL) prolog=#bsd; else prolog->append(#bsd);>>

	   | dbur:declareBaseURI
	     <<if(prolog == NULL) prolog=#dbur; else prolog->append(#dbur);>>

	   | dconstr:declareConstruction
	     <<if(prolog == NULL) prolog=#dconstr; else prolog->append(#dconstr);>>

	   | dord:declareOrdering
	     <<if(prolog == NULL) prolog=#dord; else prolog->append(#dord);>>

	   | dcns:declareCopyNamespace
	     <<if(prolog == NULL) prolog=#dcns; else prolog->append(#dcns);>>

	   | dopt:declareOption
	     <<if(prolog == NULL) prolog=#dopt; else prolog->append(#dopt);>>

	   | dv:declareVar
	     <<if(prolog == NULL) prolog=#dv; else prolog->append(#dv);>>

	   | imp:import <<if(prolog == NULL) prolog=#imp; else prolog->append(#imp);>>

	  ) SEMICOLON
	)*

	<<#0=prolog;>>

;


functionDefn!: 
	<<ASTBase* ret_type = NULL;
	  bool body = true;
	>>
	DECLARE FUNCTION qn:qname LPAR {pl:paramList} RPAR {AS st:sequenceType}
    <<if (#st == NULL) ret_type = #(#[AST_TYPE], #[AST_ITEM_TEST], #["zero-or-more", AST_MULTIPLICITY]);
      else ret_type = #st;
	>>
	( e:enclosedExpr | EXTERNAL <<body = false;>> )
	  <<
	    if (body)
	       #0=#(#[AST_FUNCTION], 
	            #qn,
	            #(#[AST_FPARAMS], #pl),
	            #(#[AST_RETURNED_TYPE], ret_type),
	            #(#[AST_BODY_FUNC], #e));
	    else
	       #0=#(#[AST_FUNCTION], 
	            #qn,
	            #(#[AST_FPARAMS], #pl),
	            #(#[AST_RETURNED_TYPE], ret_type),
	            #(#[AST_BODY_FUNC]));

	  >>
;

paramList!:
	p1:param <<#0=#p1;>>
	(COMMA p2:param <<#0->append(#p2);>> )*
;

param!:
	vr:varRef {td:typeDeclaration}

	<<if (#td == NULL) #0=#(#[AST_PARAM], #vr, #(#[AST_TYPE], #[AST_ITEM_TEST], #["zero-or-more", AST_MULTIPLICITY]));
      else #0=#(#[AST_PARAM], #vr, #td);
	>>
;


namespaceDecl!:
	DECLARE NAMESPACE nc:ncname EQUAL s:STRINGLITERAL
	<<#0=#(#[AST_DECL_NSP], 
	     #[#nc->getText(), AST_PREFIX],
	     #[$s->getText(), AST_STRING_CONST]);
	>>
;

declareVar!:
	DECLARE VARIABLE v:varRef {td:typeDeclaration} 
	(  COLONEQUALS e:exprSingle <<#0=#(#[AST_VAR_DECL_EXPR], #v, #e, #td);>>
	 | EXTERNAL <<#0=#(#[AST_VAR_DECL_EXT], #v, #td);>>
	)
;

defaultDecls!:
	DECLARE DEFAULT 
	 (ELEMENT NAMESPACE s:STRINGLITERAL
	  <<#0=#(#[AST_DECL_DEF_ELEM_NSP], 
	         #[$s->getText(), AST_STRING_CONST]);
	  >>
	 |
	  FUNCTION NAMESPACE s2:STRINGLITERAL
	  <<#0=#(#[AST_DECL_DEF_FUNC_NSP], 
	         #[$s2->getText(), AST_STRING_CONST]);
	  >>
	 | ORDER EMPTY 
	     (   GREATEST  <<#0=#[AST_DEF_ORDER_EG];>>

	      | LEAST <<#0=#[AST_DEF_ORDER_EL];>>
	     )
	 | COLLATION s3:STRINGLITERAL <<#0=#(#[AST_DEF_COLL], #[$s3->getText(), AST_STRING_CONST]);>>


	 )

;

boundarySpaceDecl!:
	DECLARE BOUNDARYSPACE (PRESERVE <<#0=#[AST_BSPACE_P]; is_preserve_boundary_space = true;>>| STRIP<<#0=#[AST_BSPACE_S]; is_preserve_boundary_space = false;>>)

;

declareOption!:
	DECLARE OPTION qn:qname s:STRINGLITERAL
	<<#0=#(#[AST_DECLARE_OPT], #qn, #[$s->getText(), AST_STRING_CONST]);>>
;

declareBaseURI!:

	DECLARE BASEURI s:STRINGLITERAL
	<<#0=#(#[AST_DECL_BURI], #[$s->getText(), AST_STRING_CONST]);>>
;

declareConstruction!:
	DECLARE CONSTRUCTION (STRIP <<#0=#[AST_DECL_CONSTR_S];>> | PRESERVE <<#0=#[AST_DECL_CONSTR_P];>>)
;

declareOrdering!:
	DECLARE ORDERING (ORDERED <<#0=#[AST_DECL_ORD];>> | UNORDERED <<#0=#[AST_DECL_UNORD];>>)
;

declareCopyNamespace!:
	DECLARE COPYNAMESPACE p:preserveMode COMMA i:inheritMode
	<<#0=#(#[AST_DECL_COPY_NS], #p, #i);>>
;

preserveMode!:
	  PRESERVE  <<#0=#["preserve", AST_STRING_CONST];>>
	| NOPRESERVE <<#0=#["no-preserve", AST_STRING_CONST];>>
;

inheritMode!:
	  INHERIT <<#0=#["inherit", AST_STRING_CONST];>>
	| NOINHERIT <<#0=#["no-inherit", AST_STRING_CONST];>>
;

import!:
	  importModule
	| importSchema
;

importModule!:
	IMPORT {MODULE NAMESPACE ncname EQUAL}   STRINGLITERAL {AT_ STRINGLITERAL  (COMMA  STRINGLITERAL)*}
	<<throw USER_EXCEPTION(XQST0016);>>
;

importSchema!:
	IMPORT SSHEMA { schemaPrefix } STRINGLITERAL  {AT_ STRINGLITERAL (COMMA STRINGLITERAL)*}
	<<throw USER_EXCEPTION(XQST0009);>>
;

schemaPrefix!:
	  (NAMESPACE ncname  EQUAL)
	| (DEFAULT ELEMENT NAMESPACE)
;

}