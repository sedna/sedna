/* */

class XQueryParser {


metadataExpr!:

	( RETRIEVE 
	  (  <<bool exist_statistics = false;>>
	     METADATA FOR_ (  DOCUMENTS {CWITH STATISTICS <<exist_statistics = true;>>} {CIN_ CCOLLECTION e1:expr}
	                     <<
	                       if (exist_statistics && #e1 != NULL) throw USER_EXCEPTION2(SE4001, "incoorect combination (WITH STATISTICS) and (IN COLLECTION) expressions");

	                       if (exist_statistics && #e1 == NULL)
	                          #0=#(#[AST_RETRIEVE_METADATA_DOCS],  #[AST_STATISTICS]);
                           else if (exist_statistics && #e1 != NULL) 
	                          #0=#(#[AST_RETRIEVE_METADATA_DOCS], #e1, #[AST_STATISTICS]);


	                       else if (!exist_statistics && #e1 == NULL)
	                          #0=#(#[AST_RETRIEVE_METADATA_DOCS]);
                           else
	                          #0=#(#[AST_RETRIEVE_METADATA_DOCS], #e1);

	                     >>
	                
	                   | COLLECTIONS {CWITH STATISTICS <<exist_statistics = true;>>}
	                     <<if (exist_statistics) 
	                          #0=#(#[AST_RETRIEVE_METADATA_COLLS], #[AST_STATISTICS]);
	                       else
	                          #0=#(#[AST_RETRIEVE_METADATA_COLLS]);
	                     >>
	                  )


	   | DESCRIPTIVE SCHEMA FOR_ (  CDOCUMENT e2:expr {CIN_ CCOLLECTION e3:expr}
	                               <<
	                                 if (#e3 == NULL)
	                                    #0=#(#[AST_DESCR_SCHEMA], #e2);
	                                 else
	                                    #0=#(#[AST_DESCR_SCHEMA], #e2, #e3);
	                               >>

	                             | CCOLLECTION e4:expr
	                               <<
	                                 #0=#(#[AST_DESCR_SCHEMA_FOR_COL],
	                                      #e4);
	                               >>
	                            )
	  )
	)
;
	
}