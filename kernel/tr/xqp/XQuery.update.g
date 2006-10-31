/************************************************************
********** XQuery update syntactic analizer rules ***********
*************************************************************/

class XQueryParser {

updateExpr!:
	UPDATE
	(  ie1:insertExpr          <<#0=#ie1;>>
	 | de1:deleteExpr          <<#0=#de1;>>
	 | due1:delete_undeepExpr  <<#0=#due1;>>
	 | re1:replaceExpr         <<#0=#re1;>>
	 | rene1:renameExpr        <<#0=#rene1;>>
	 | me1:moveExpr            <<#0=#me1;>>
	)

	(  ie2:insertExpr          <<#0->append(#ie2);>>
	 | de2:deleteExpr          <<#0->append(#de2);>>
	 | due2:delete_undeepExpr  <<#0->append(#due2);>>
	 | re2:replaceExpr         <<#0->append(#re2);>>
	 | rene2:renameExpr        <<#0->append(#rene2);>>
	 | me2:moveExpr            <<#0->append(#me2);>>
	)*

;

insertExpr!:
	<<ASTBase *pos=NULL;>>
	INSERT e1:expr 
	(  INTO       <<pos=#[AST_INTO];>>
	 | PRECEDING  <<pos=#[AST_PRECEDING];>>
	 | FOLLOWING  <<pos=#[AST_FOLLOWING];>>
	)
	e2:expr
	<<#0=#(#[AST_INSERT], #e1, #e2, pos);>>
;

deleteExpr!:
	DELETE_ e:expr
	<<#0=#(#[AST_DELETE], #e);>>
;

delete_undeepExpr!:
	DELETE_UNDEEP e:expr
	<<#0=#(#[AST_DELETE_UNDEEP], #e);>>
;

replaceExpr!:
	REPLACE v:varRef {AS st:sequenceType} IN_ e1:expr WITH e2:expr
	<<if (#st != NULL)
	     #0=#(#[AST_REPLACE], #v, #st, #e1, #e2);
	  else
	     #0=#(#[AST_REPLACE],
	          #v,
	          #(#[AST_TYPE], #[AST_ITEM_TEST], #["zero-or-more", AST_MULTIPLICITY]),
	          #e1,
	          #e2);
	
	>>
;


renameExpr!:
	RENAME e:expr ON qn:qname
	<<#0=#(#[AST_RENAME], #e, #qn);>>
;

moveExpr!:
	<<ASTBase *pos=NULL;>>

	MOVE v:varRef AS st:sequenceType IN_ e:expr 
	(  INTO      <<pos=#[AST_INTO];>>
	 | PRECEDING <<pos=#[AST_PRECEDING];>>
	 | FOLLOWING <<pos=#[AST_FOLLOWING];>>
	)
	rpe:pathExpr
	<<#0=#(#[AST_MOVE], #v, #st, #e, #rpe, pos);>>
;

}