/* */

class XQueryParser {

createExpr![XQueryDLGLexer* lexer_] :
          <<std::string stream; bool b;
	    ASTBase* lst;
	      >>
	  CREATE 
	  ( ( CCOLLECTION e1:exprSingle
	     <<#0=#(#[AST_CREATE_COLLECTION], #e1);>>
	    )

	   | ( CDOCUMENT e2:exprSingle  {CIN_ CCOLLECTION e3:exprSingle}
	     <<
	       if (#e3 == NULL) 
	          #0=#(#[AST_CREATE_DOCUMENT], #e2);
           else 
	          #0=#(#[AST_CREATE_DOCUMENT], #e2, #e3);
	     >>
	     )
	   | ( ROLE s9:STRINGLITERAL
	     <<
	       #0=#(#[AST_CREATE_ROLE], #[$s9->getText(), AST_STRING_CONST]); 	        
	     >> 
	     )
	   | ( USER s19:STRINGLITERAL CWITH PASSWORD s20:STRINGLITERAL
	     <<#0=#(#[AST_CREATE_USER], 
	            #[$s19->getText(), AST_STRING_CONST],
	            #[$s20->getText(), AST_STRING_CONST]);
	     >>
	     )
	   | (INDEX n1:exprSingle CON
	      p1:pathExpr CBY p2:pathExpr CAS t:singleType
	      <<
	         #0=#(#[AST_CREATE_INDEX], #n1, #p1, #p2, #t);
	      >>
	     )
	   | (FULLTEXT INDEX n2_1:exprSingle 
	               CON p3_1:pathExpr CTYPE s21_1:STRINGLITERAL {e2_1:expr}
	      <<if (#e2_1 != NULL)
	           #0=#(#[AST_CREATE_FULLTEXT_INDEX], #n2_1, #p3_1, #[$s21_1->getText(), AST_STRING_CONST], #e2_1);
	        else
	           #0=#(#[AST_CREATE_FULLTEXT_INDEX], #n2_1, #p3_1, #[$s21_1->getText(), AST_STRING_CONST]);
	      >>

	     )

	   | (TRIGGER <<#0=#[AST_CREATE_TRIGGER];>> ct_s:STRINGLITERAL <<#0->addChild(#[$ct_s->getText(), AST_STRING_CONST]);>>
	      (BEFORE <<#0->addChild(#["\"BEFORE\"", AST_STRING_CONST]);>> | AFTER <<#0->addChild(#["\"AFTER\"", AST_STRING_CONST]);>>)
	      (CINSERT <<#0->addChild(#["\"INSERT\"", AST_STRING_CONST]);>> | CDELETE <<#0->addChild(#["\"DELETE\"", AST_STRING_CONST]);>>| CREPLACE <<#0->addChild(#["\"REPLACE\"", AST_STRING_CONST]);>>)
	      CON ct_pe:pathExpr <<#0->addChild(#ct_pe);>>
	      FOR_ EACH (CNODE <<#0->addChild(#["\"NODE\"", AST_STRING_CONST]);>> | STATEMENT <<#0->addChild(#["\"STATEMENT\"", AST_STRING_CONST]);>>)
	      CDO LBRACE ct_e1:triggerDoStmt SEMICOLON <<lst=#(#[AST_DO_STMNT_LIST], #ct_e1);>> ( ct_e2:triggerDoStmt SEMICOLON <<lst->addChild(#ct_e2);>>)* RBRACE
	      <<#0->addChild(lst);>>
	     )
	      

	  )
	  

	| DROP 
	  (  ( CCOLLECTION e5:exprSingle
	       <<#0=#(#[AST_DROP_COLLECTION], #e5);>>
	     )
	     
	   | ( CDOCUMENT e6:exprSingle {CIN_ CCOLLECTION e8:exprSingle}
	     <<
	       if (#e8 == NULL)
	           #0=#(#[AST_DROP_DOCUMENT], #e6);
           else 
	           #0=#(#[AST_DROP_DOCUMENT], #e6, #e8);
	     >>
	     )

	   | ( ROLE s10:STRINGLITERAL
	     <<
	       #0=#(#[AST_DROP_ROLE], #[$s10->getText(), AST_STRING_CONST]);
	     >>
	     )
	   | ( USER s21:STRINGLITERAL 
	     <<#0=#(#[AST_DROP_USER], #[$s21->getText(), AST_STRING_CONST]);>>
	     )

	   | (INDEX n3:exprSingle
	      <<#0=#(#[AST_DROP_INDEX], #n3); >>
	     )

	   | (FULLTEXT INDEX n4:exprSingle
	      <<#0=#(#[AST_DROP_FULLTEXT_INDEX], #n4);>>
	     )

	   | (TRIGGER dt_s:STRINGLITERAL <<#0=#(#[AST_DROP_TRIGGER], #[$dt_s->getText(), AST_STRING_CONST]);>>)

	  )

	| ALTER USER s22:STRINGLITERAL CWITH PASSWORD s23:STRINGLITERAL
	  << #0=#(#[AST_ALTER_USER],
                #[$s22->getText(), AST_STRING_CONST],
                #[$s23->getText(), AST_STRING_CONST]);
	  >>

    |

     LOAD (  s5_1:STRINGLITERAL <<stream=$s5_1->getText();>>
           | STDIN              <<stream = "\"/STDIN/\"";>>
          )
            s6_1:STRINGLITERAL {s7_1:STRINGLITERAL}

	  <<if($s7_1==NULL)
	       #0=#(#[AST_LOAD_FILE], #[stream, AST_STRING_CONST],
                                  #[$s6_1->getText(), AST_STRING_CONST]);
	    else
	       #0=#(#[AST_LOAD_FILE], #[stream, AST_STRING_CONST],
                                  #[$s6_1->getText(), AST_STRING_CONST],
	                              #[$s7_1->getText(), AST_STRING_CONST]);

	  >>

	| LOADFILE e5_1:exprSingle CAS e6_1:exprSingle {CIN_ e7_1:exprSingle}
      <<
         if (#e7_1 == NULL)
            #0=#(#[AST_LOAD_FILE_EX], #e5_1, #e6_1);
         else
            #0=#(#[AST_LOAD_FILE_EX], #e5_1, #e6_1, #e7_1);
      >>


	| GRANT privl1:privList
	  (   CON ( s12:STRINGLITERAL CTO u13:userList
	           <<
	             #0=#(#[AST_GRANT_PRIV_ON_DOC], 
	                  #privl1,
	                  #[$s12->getText(), AST_STRING_CONST],
	                  #u13
                      );
	           >>
	          | DATABASE CTO u13_1:userList
	           <<
	             #0=#(#[AST_GRANT_PRIV], 
	                  #privl1,
	                  #u13_1
                     );
	           >>
	          | CDOCUMENT s13_2:STRINGLITERAL CTO u13_3:userList
	           <<
	             #0=#(#[AST_GRANT_PRIV_ON_DOC], 
	                  #privl1,
	                  #[$s13_2->getText(), AST_STRING_CONST],
	                  #u13_3
                     );
	           >>
	          | CCOLLECTION s13_4:STRINGLITERAL CTO u13_5:userList
	           <<
	             #0=#(#[AST_GRANT_PRIV_ON_COL], 
	                  #privl1,
	                  #[$s13_4->getText(), AST_STRING_CONST],
	                  #u13_5
                     );
	           >>

	         )
	    | CTO u14:userList
	      <<
	        #0=#(#[AST_GRANT_ROLE], 
	             #privl1,
	             #u14
                );
	      >>

	  )

	| REVOKE privl2:privList
	  (   CON  ( s16:STRINGLITERAL FROM u17:userList
	            <<
	              #0=#(#[AST_REVOKE_PRIV_FR_DOC], 
	                   #privl2,
	                   #[$s16->getText(), AST_STRING_CONST],
	                   #u17
                      );
	            >>
	           | DATABASE FROM u18:userList
	            <<
	              #0=#(#[AST_REVOKE_PRIV], 
	                   #privl2,
	                   #u18
                      );
	            >>

	           | CDOCUMENT s19_0:STRINGLITERAL FROM u20_0:userList
	            <<
	              #0=#(#[AST_REVOKE_PRIV_FR_DOC], 
	                   #privl2,
	                   #[$s19_0->getText(), AST_STRING_CONST],
	                   #u20_0
                      );
	            >>

	           | CCOLLECTION s19_1:STRINGLITERAL FROM u20_1:userList
	            <<
	              #0=#(#[AST_REVOKE_PRIV_FR_COL], 
	                   #privl2,
	                   #[$s19_1->getText(), AST_STRING_CONST],
	                   #u20_1
                      );
	            >>

	          )
	    | FROM u25:userList
	      <<
	        #0=#(#[AST_REVOKE_ROLE], 
	             #privl2,
	             #u25
                );
	      >>

	  )
	  
;

privList!:
	( s1:STRINGLITERAL <<#0=#(#[AST_PRIV_LIST], #[$s1->getText(), AST_STRING_CONST]);>>
	  (COMMA s2:STRINGLITERAL <<#0->addChild(#[$s2->getText(), AST_STRING_CONST]);>>)*	
	|
	  ALL 
	  <<#0=#[AST_PRIV_ALL];>>
	)
;
	
userList!:
	( s1:STRINGLITERAL <<#0=#(#[AST_USER_LIST], #[$s1->getText(), AST_STRING_CONST]);>>
	  (COMMA s2:STRINGLITERAL <<#0->addChild(#[$s2->getText(), AST_STRING_CONST]);>>)*	
	|
	  PUBLIC 
	  <<#0=#[AST_USER_PUBLIC];>>
	)
;

triggerDoStmt!:
	  es:exprSingle <<#0=#es;>>
	| ue:updateExpr <<#0=#ue;>>
;

/*collection!:
	s:STRINGLITERAL 
	<<#0=#[$s->getText(), AST_STRING_CONST];>>
*/
}

