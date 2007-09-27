/* */

class XQueryParser {


metadataExpr!:

	( (RETRIEVE | RETRIEVE_LOWCASE)
	  (  <<bool exist_statistics = false;>>
	     (METADATA | METADATA_LOWCASE) (FOR_ | FOR) (  (DOCUMENTS | DOCUMENTS_LOWCASE) {(CWITH | WITH) (STATISTICS | STATISTICS_LOWCASE) <<exist_statistics = true;>>} {(CIN_ | IN_) (CCOLLECTION | CCOLLECTION_LOWCASE) e1:expr}
	                     <<
	                       if (exist_statistics && #e1 != NULL) throw USER_EXCEPTION2(XPST0003, "incoorect combination (WITH STATISTICS) and (IN COLLECTION) expressions");

	                       if (exist_statistics && #e1 == NULL)
	                          #0=#(#[AST_RETRIEVE_METADATA_DOCS],  #[AST_STATISTICS]);
                           else if (exist_statistics && #e1 != NULL) 
	                          #0=#(#[AST_RETRIEVE_METADATA_DOCS], #e1, #[AST_STATISTICS]);


	                       else if (!exist_statistics && #e1 == NULL)
	                          #0=#(#[AST_RETRIEVE_METADATA_DOCS]);
                           else
	                          #0=#(#[AST_RETRIEVE_METADATA_DOCS], #e1);

	                     >>
	                
	                   | (COLLECTIONS | COLLECTIONS_LOWCASE) {(CWITH | WITH) (STATISTICS | STATISTICS_LOWCASE) <<exist_statistics = true;>>}
	                     <<if (exist_statistics) 
	                          #0=#(#[AST_RETRIEVE_METADATA_COLLS], #[AST_STATISTICS]);
	                       else
	                          #0=#(#[AST_RETRIEVE_METADATA_COLLS]);
	                     >>
	                  )


	   | (DESCRIPTIVE | DESCRIPTIVE_LOWCASE) (SCHEMA | SSHEMA) (FOR_ | FOR) (  (CDOCUMENT | LDOCUMENT) e2:expr {(CIN_ | IN_) (CCOLLECTION | CCOLLECTION_LOWCASE) e3:expr}
	                               <<
	                                 if (#e3 == NULL)
	                                    #0=#(#[AST_DESCR_SCHEMA], #e2);
	                                 else
	                                    #0=#(#[AST_DESCR_SCHEMA], #e2, #e3);
	                               >>

	                             | (CCOLLECTION | CCOLLECTION_LOWCASE) e4:expr
	                               <<
	                                 #0=#(#[AST_DESCR_SCHEMA_FOR_COL],
	                                      #e4);
	                               >>
	                            )
	  )
	)
;
	
}