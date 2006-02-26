/************************************************************
****************** XPath syntactic analizer rules **********
*************************************************************/

class XQueryParser{

pathExpr!:
	(//  SLASH relativePathExpr     //doens not correspond to the specification
	 //| SLASHSLASH relativePathExpr
	  rpe:relativePathExpr <<#0=#rpe;>>
	)
;       


relativePathExpr!:
        <<std::string sl;
	  ASTBase *steps=NULL, *desc_step=NULL;
	>>
	se1:stepExpr[""] <<steps=#se1;>>

        (slashes>[sl]
	 <<
	   if (sl == "//")
	   {
/*	      desc_step=#(#[AST_AXIS_PATH_STEP],
	                  #(#[AST_STEP], 
	                    #(#[AST_AXIS], #[AST_DESCENDANT_OR_SELF_AXIS]),
	                    #(#[AST_TEST], #[AST_NODE_TEST])),
                          #[AST_PREDICATES]);

	      steps->append(desc_step);
*/
              steps->append(#(#[AST_AXIS_PATH_STEP],
	                      #(#[AST_STEP], 
	                        #(#[AST_AXIS], #[AST_DESCENDANT_OR_SELF_AXIS]),
	                        #(#[AST_TEST], #[AST_NODE_TEST])),
                                #[AST_PREDICATES]));
	   }
	 >>
	 se2:stepExpr[sl] <<steps->append(#se2);>> )*                      

	<<#0=#(#[AST_RELATIVE_PATH], steps);>>
;

slashes! > [std::string sl]:
	(  SLASH       <<$sl="/";>>
	 | SLASHSLASH  <<$sl="//";>>
	)
;

/*
stepExpr! [string sl]:

	  <<
	    (LA(1) != NODE || LA(2) != LPAR) &&
	    (LA(1) != TEXT || LA(2) != LPAR) &&
	    (LA(1) != COMMENT_ || LA(2) != LPAR) &&
	    (LA(1) != PROCESSING_INSTRUCTION || LA(2) != LPAR) &&
	    (LA(1) != DOCUMENT || LA(2) != LPAR) &&
	    (LA(1) != ELEMENT || LA(2) != LPAR) &&
	    (LA(1) != ATTRIBUTE || LA(2) != LPAR)
	  >>? (fs:filterStep <<#0=#fs;>>)?
	| as:axisStep[$sl]   <<#0=#as;>>


;
*/

stepExpr! [std::string sl]:
	  (as:axisStep[$sl]   <<#0=#as;>>)?
	| fs:filterStep <<#0=#fs;>> 
;


axisStep! [std::string sl]:
	( fs:forwardStep[$sl]      <<#0=#fs;>>   
	| rss:reverseStep[$sl] <<#0=#rss;>>) ps:predicates
	
	<<#0=#(#[AST_AXIS_PATH_STEP], #0, #ps);>>
;

filterStep!:
	pe:primaryExpr ps:predicates
	
	<<#0=#(#[AST_FILTER_PATH_STEP], #pe, #ps);>>
;


primaryExpr!: 
	  vr:varRef                        <<#0=#vr;>>
	| l:literal                        <<#0=#l;>>
	| pe:parenthesizedExpr             <<#0=#pe;>> 
	| cie:contextItemExpr              <<#0=#cie;>>
	| fc:functionCall                  <<#0=#fc;>>
	| c:constructor                    <<#0=#c;>>
;

contextItemExpr!:
	DOT <<#0=#[AST_CONTEXT_ITEM];>>
;


predicates!:
	<<ASTBase* ps = NULL;>>
	(LBRACK e:expr  RBRACK << if (ps ==NULL) ps =#e;
	                          else ps->append(#e);
	                       >> )*

	<<#0=#(#[AST_PREDICATES], ps);>>

;	


forwardStep! [std::string sl]:
	(  fa:forwardAxis nt:nodeTest 
	   <<#0=#(#[AST_STEP], 
	          #(#[AST_AXIS], #fa),
	          #(#[AST_TEST], #nt));
	   >>
	 | afs:abbrevForwardStep[$sl] <<#0=#afs;>>
	)
;

reverseStep! [std::string sl]:
	( ra:reverseAxis nt:nodeTest
	  <<#0=#(#[AST_STEP],
	         #(#[AST_AXIS], #ra),
	         #(#[AST_TEST], #nt));
	  >>
	| arss:abbrevReverseStep[sl]

	  <<#0=#arss;>>
	) 
;

abbrevForwardStep! [std::string sl]:
	AT nt1:nodeTest
	<< #0=#(#[AST_STEP],
                #(#[AST_AXIS], #[AST_ATTRIBUTE_AXIS]),
                #(#[AST_TEST], #nt1));
	>>

	| nt2:nodeTest
	<< #0=#(#[AST_STEP],
                #(#[AST_AXIS], #[AST_CHILD_AXIS]),
                #(#[AST_TEST], #nt2));
	  
	>>	
;


abbrevReverseStep! [std::string sl]:
	  DOTDOT <<#0=#[AST_PARENT_AXIS];>>
	  << #0=#(#[AST_STEP], 
	          #(#[AST_AXIS], #[AST_PARENT_AXIS]),
	          #(#[AST_TEST], #[AST_NODE_TEST]));
	  >>
;

forwardAxis!:
	  CHILD DOUBLECOLON              <<#0=#[AST_CHILD_AXIS];>>
	| DESCENDANT DOUBLECOLON         <<#0=#[AST_DESCENDANT_AXIS];>>
	| ATTRIBUTE DOUBLECOLON          <<#0=#[AST_ATTRIBUTE_AXIS];>>
	| SELF DOUBLECOLON               <<#0=#[AST_SELF_AXIS];>>
	| DESCENDANT_OR_SELF DOUBLECOLON <<#0=#[AST_DESCENDANT_OR_SELF_AXIS];>>
	| FOLLOWING_SIBLING DOUBLECOLON  <<#0=#[AST_FOLLOWING_SIBLING_AXIS];>>
	| FOLLOWING DOUBLECOLON          <<#0=#[AST_FOLLOWING_AXIS];>>
;

reverseAxis!:
	  PARENT DOUBLECOLON             <<#0=#[AST_PARENT_AXIS];>>
	| ANCESTOR DOUBLECOLON           <<#0=#[AST_ANCESTOR_AXIS];>>
	| PRECEDING_SIBLING DOUBLECOLON  <<#0=#[AST_PRECEDING_SIBLING_AXIS];>>
	| PRECEDING DOUBLECOLON          <<#0=#[AST_PRECEDING_AXIS];>>
	| ANCESTOR_OR_SELF DOUBLECOLON   <<#0=#[AST_ANCESTOR_OR_SELF_AXIS];>>
;

nodeTest!:
	(  kt:kindTest <<#0=#kt;>>
	 | nt:nameTest <<(LA(1) != LPAR)>>? <<#0=#nt;>>
	)
;	


nameTest!:
	(  
	  (qn:qname <<#0=#qn;>>)?
	 | w:wildcard <<#0=#w;>>  
	)
;

wildcard!:
	(  n1:ncname COLON STAR <<#0=#(#[AST_WILDCARD], 
	                               #[#n1->getText(), AST_PREFIX],
                                       #["*", AST_LOCAL_NAME]);
	                        >>
	 | STAR COLON n2:ncname <<#0=#(#[AST_WILDCARD], 
	                               #["*", AST_PREFIX],
                                       #[#n2->getText(), AST_LOCAL_NAME]);
	                        >>
	 |  s:STAR <<#0=#[AST_WILDCARD];>>
	) 
;

/*
wildcard!:
	(   p1:prefixPart1 COLON STAR
	    <<
	      #0=#()
	    >>
	  | p2:prefixPart2  COLON STAR <<#0=#(#[AST_WILDCARD], 
	                               #[$n1->getText(), AST_PREFIX],
                                       #["*", AST_LOCAL_NAME]);
	  | STAR COLON 
	             (  l1:localPart1
	              | l2:localPart2
	             )
	                        >>
	                          <<#0=#(#[AST_WILDCARD], 
	                               #["*", AST_PREFIX],
                                       #[$n2->getText(), AST_LOCAL_NAME]);
	                        >>
	 |  s:STAR <<#0=#[AST_WILDCARD];>>
	) 
;
*/
functionCall!:
	(
	  qn:qname LPAR {p1:params} RPAR
	   <<#0=#(#[AST_FCALL], #qn, #p1);>>
/*
	 | TEXT LPAR {p2:params} RPAR
	   <<#0=#(#[AST_FCALL], 
	          #(#[AST_QNAME], #["text", AST_LOCAL_NAME]),
	          #p2);

	   >>

	 | EMPTY LPAR {p3:params} RPAR
	   <<#0=#(#[AST_FCALL], 
	          #(#[AST_QNAME], #["empty", AST_LOCAL_NAME]),
	          #p3);
	   >>
*/
/*
	 | ITEM LPAR {p4:params} RPAR
	   <<#0=#(#[AST_FCALL], 
	          #(#[AST_QNAME], #["item", AST_LOCAL_NAME]),
	          #p4);
	   >>
*/

	)
;

params!: 
	e1:exprSingle <<#0=#e1;>> (COMMA e2:exprSingle <<#0->append(#e2);>>)*
;

literal!:
	  i:INTEGERLITERAL  <<#0=#[$i->getText(), AST_INTEGER_CONST];>>	  
	| dc:DECIMALLITERAL <<#0=#[$dc->getText(), AST_DECIMAL_CONST];>>
	| db:DOUBLELITERAL  <<#0=#[$db->getText(), AST_DOUBLE_CONST];>>
	| s:STRINGLITERAL   <<#0=#[$s->getText(), AST_STRING_CONST];>>
;



}