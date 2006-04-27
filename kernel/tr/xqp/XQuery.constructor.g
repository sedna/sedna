/************************************************************
******* XQuery constructors syntactic analizer rules ********
*************************************************************/

class XQueryParser{

constructor!:
	  dec:dirElemConstructor <<#0=#dec;>>
	| cc:computedConstructor <<#0=#cc;>>
//	| xc:xmlComment          <<#0=#xc;>>
//	| xpi:xmlPI              <<#0=#xpi;>>
//	| cdc:cdataSection       <<#0=#cdc;>>
;

dirElemConstructor!:
	STARTTAGOPEN qn1:qname al:attributeList

	(  EMPTYTAGCLOSE

	   <<#0=#(#[AST_ELEMENT],
		#(#[AST_ELEMENT_NAME], #qn1),
		#(#[AST_ELEMENT_ATTRIBUTES],#al),
		#(#[AST_CONTENT]));
	   >>
 
	 | STARTTAGCLOSE
 
	   ec:elementContent 

	   ENDTAGOPEN qn2:qname {WS|NL} ENDTAGCLOSE

	   <<if (strcmp(((AST*)(#qn1->down()))->getText(), ((AST*)(#qn2->down()))->getText()) != 0)
	        throw USER_EXCEPTION2(SE4001, ("start tag " + std::string("\'<") + ((AST*)(#qn1->down()))->getText() + ">\'" +
	                                       " does not match close tag " + "\'</" + ((AST*)(#qn2->down()))->getText()  + ">\'" +
	                                       ", line: " + int2string(LT(1)->getLine())).c_str());
	    #0=#(#[AST_ELEMENT],
		     #(#[AST_ELEMENT_NAME], #qn2),
		     #(#[AST_ELEMENT_ATTRIBUTES], #al),
		     #(#[AST_CONTENT], #ec));
	   >>
	)	
;

elementContent!:
	<<std::string val = "";
	  ASTBase* cnt = NULL;
	>>
	( ce:CHAR_ELEM              
	  << if (($ce->getText())[0] == '\"') val += "\\\"";
	     else if (($ce->getText())[0] == '\\') val += "\\\\";
	     else val += $ce->getText();
	  >>
	| DOUBLELBRACE              << val += "{"; >>
	| DOUBLERBRACE              << val += "}"; >>
	| dec:dirElemConstructor
	<<
	  if (val.empty())
	  {
	     if (cnt == NULL) cnt = #dec; else cnt->append(#dec);
	  }
	  else
	  {
	     bool isBoundWhiteSpace = true;
	     std::string::iterator it;
            
	     for (it = val.begin(); it != val.end(); it++)
	     {
	        if(!(*it == ' ' || *it == '\n' || *it == '\t' || *it == '\r'))
	          isBoundWhiteSpace = false;
	     }

	     if (cnt == NULL && isBoundWhiteSpace) cnt = #dec;
	     else
	     if (cnt == NULL && !isBoundWhiteSpace)
	     {
	        cnt = #[val, AST_CHAR_SEQ];
	        cnt->append(#dec);
	     } 
	     else
	     if ( cnt!= NULL && isBoundWhiteSpace)
	        cnt->append(#dec);
	     else
	     {
	        cnt->append(#[val, AST_CHAR_SEQ]);
	        cnt->append(#dec);
	     }
	  }

	  val = "";
	>>

	| ee:enclosedExpr     
	<<
	  if (val.empty())
	  {
	     if (cnt == NULL) cnt = #ee; else cnt->append(#ee);
	  }
	  else
	  {
	     bool isBoundWhiteSpace = true;
	     std::string::iterator it;
            
	     for (it = val.begin(); it != val.end(); it++)
	     {
	        if(!(*it == ' ' || *it == '\n' || *it == '\t' || *it == '\r'))
	          isBoundWhiteSpace = false;
	     }

	     if (cnt == NULL && isBoundWhiteSpace) 
	        cnt = #ee;
	     else
	     if (cnt == NULL && !isBoundWhiteSpace)
	     {
	        cnt = #[val, AST_CHAR_SEQ];
	        cnt->append(#ee);
	     } 
	     else
	     if ( cnt!= NULL && isBoundWhiteSpace)
	        cnt->append(#ee);
	     else
	     {
	        cnt->append(#[val, AST_CHAR_SEQ]);
	        cnt->append(#ee);
	     }
	  }

	  val = "";
	>>
	| per:predefinedEntityRef   << val += #per->getText();>>
//	| cdataSection           
//	| charRef       not supported know
//	| xmlComment
//	| xmlPI
	)*

	<<   
	  //d_printf2("XQParser: constr_val=%s\n", val.c_str());

	  if ( !(val.empty()) )
	  {
	     bool isBoundWhiteSpace = true;
	     std::string::iterator it;
             
	     for (it = val.begin(); it != val.end(); it++)
	     {
	        if(!(*it == ' ' || *it == '\n' || *it == '\t' || *it == '\r'))
	          isBoundWhiteSpace = false;
	     }
	     
	     if ( cnt == NULL && isBoundWhiteSpace)
	       #0= NULL;
	     else
	     if ( cnt == NULL && !isBoundWhiteSpace)
	        #0 = #[val, AST_CHAR_SEQ];
	     else
	     if (cnt!=NULL && isBoundWhiteSpace) #0 = cnt;
	     else
	     {
	       #0 = cnt;
	       #0->append(#[val, AST_CHAR_SEQ]);
	     }
	  }
	  else
	  {
	     #0 = cnt;
	  }   
	>>
;


//enclosedExpr rule is in the XQuery.common.g file

/*
cdataSection!:
	OPENCDATA (CHAR_SEQ_CDATA | RBRACK | RBRANGL)* CLOSECDATA
;
//cdata does not supported in the data internal representation


*/

attributeList!:
	<<ASTBase *atts=NULL;bool isDef=true;>>
	((WS|NL) 
	{ ( qn:qname {WS|NL} EQUAL {WS|NL} av1:attributeValue

	    <<
	      if (#qn->down()->nsiblings() == 1)//there is no prefix
	      {
	         if (strcmp( ((AST*)(#qn->down()))->getText(), "xmlns") == 0)
	             if(atts==NULL) atts=#(#[AST_NAMESPACE],
	                                   #(#[AST_NAMESPACE_NAME], #["\"\"", AST_PREFIX]),
                                       #av1);

	             else atts->append(#(#[AST_NAMESPACE], 
	                                 #(#[AST_NAMESPACE_NAME], #["\"\"", AST_PREFIX]),
                                     #av1));
	         else
	             if(atts==NULL) atts=#(#[AST_ATTRIBUTE],
	                                   #(#[AST_ATTRIBUTE_NAME], #qn),
                                       #av1);

	             else atts->append(#(#[AST_ATTRIBUTE], 
	                                 #(#[AST_ATTRIBUTE_NAME], #qn),
                                     #av1));

	      }
	      else
	      {
	         if (strcmp(((AST*)(#qn->down()->right()))->getText(), "xmlns") == 0)
	         {
	             std::string prefix;
                 prefix = std::string("\"")+ std::string(((AST*)(#qn->down()))->getText())+"\"";

	             if(atts==NULL) atts=#(#[AST_NAMESPACE],
	                                   #(#[AST_NAMESPACE_NAME], #[prefix, AST_PREFIX]),
                                       #av1);

	             else atts->append(#(#[AST_NAMESPACE], 
	                                 #(#[AST_NAMESPACE_NAME], #[prefix, AST_PREFIX]),
                                     #av1));
	         }
	         else
	             if(atts==NULL) atts=#(#[AST_ATTRIBUTE],
	                                   #(#[AST_ATTRIBUTE_NAME], #qn),
                                       #av1);

	             else atts->append(#(#[AST_ATTRIBUTE], 
	                                 #(#[AST_ATTRIBUTE_NAME], #qn),
                                     #av1));

	      }

	    >>
	  )
	}
	)*
	<<#0=atts;>>
;


attributeValue!:
	<<ASTBase *val=NULL;>>
	( (QUOT   qavc:quotAttrValueContent
	           << #0=#qavc; >>
	   QUOT)

	| (APOS   aavc:aposAttrValueContent
	           << #0=#aavc; >>
	   APOS)
	)
	<<#0=#(#[AST_CONTENT], #0);>>
;



quotAttrValueContent!:
	<<std::string val= "";
	  ASTBase *cnt = NULL;
	>>

	( ca:CHAR_ATTR
	  << if (($ca->getText())[0] == '\"') val += "\\\"";
	     else if (($ca->getText())[0] == '\\') val += "\\\\";
	     else val += $ca->getText();
	  >>

	| APOS << val += "\'"; >>
	| DOUBLEQUOT <<val += "\\\"";>>
	| DOUBLELBRACE <<val += "{";>>
	| DOUBLERBRACE <<val += "}";>>
	| per:predefinedEntityRef <<val += #per->getText();>>
	| ee:enclosedExpr 
	<<
	  if (val.empty())
	  {
	    if (cnt == NULL) 
	      cnt = #ee;
	    else 
	      cnt->append(#ee); 
	  }
	  else
	  {
	    if ( cnt == NULL)
	    {
	       cnt = #[val, AST_CHAR_SEQ];
	       cnt->append(#ee);
	    }
	    else
	    {
	       cnt->append(#[val, AST_CHAR_SEQ]);
	       cnt->append(#ee);
	    }  
	  } 

	  val = "";
	>>
        )*

	<<if (cnt == NULL)
	  {
	     if (val.empty()) #0 = NULL;
	     else #0 = #[val, AST_CHAR_SEQ];
	  }
	  else
	  {
	    if (val.empty())
	      #0 = cnt;
	    else
	    {
	      #0 = cnt;
	      #0->append(#[val, AST_CHAR_SEQ]);
	    }
	  }
	>>
;

aposAttrValueContent!:
	<<std::string val= "";
	  ASTBase *cnt = NULL;
	>>

	( ca:CHAR_ATTR

	  << if (($ca->getText())[0] == '\"') val += "\\\"";
	     else if (($ca->getText())[0] == '\\') val += "\\\\";
	     else val += $ca->getText();
	  >>

	| QUOT << val += "\\\""; >>
	| DOUBLEAPOS <<val += "\'";>>
	| DOUBLELBRACE <<val += "{";>>
	| DOUBLERBRACE <<val += "}";>>
	| per:predefinedEntityRef <<val += #per->getText();>>
	| ee:enclosedExpr 
	<<
	  if (val.empty())
	  {
	    if (cnt == NULL) 
	      cnt = #ee;
	    else 
	      cnt->append(#ee); 
	  }
	  else
	  {
	    if ( cnt == NULL)
	    {
	       cnt = #[val, AST_CHAR_SEQ];
	       cnt->append(#ee);
	    }
	    else
	    {
	       cnt->append(#[val, AST_CHAR_SEQ]);
	       cnt->append(#ee);
	    }  
	  } 

	  val = "";
	>>
        )*

	<<if (cnt == NULL)
	  {
	     if (val.empty()) #0 = NULL;
	     else #0 = #[val, AST_CHAR_SEQ];
	  }
	  else
	  {
	    if (val.empty())
	      #0 = cnt;
	    else
	    {
	      #0 = cnt;
	      #0->append(#[val, AST_CHAR_SEQ]);
	    }
	  }
	>>
;


predefinedEntityRef!:
	( AMPLT <<#0 = #["<", AST_CHAR_SEQ];>>
	| AMPGT <<#0 = #[">", AST_CHAR_SEQ];>>
	| AMPAMP <<#0 = #["&", AST_CHAR_SEQ];>>
	| AMPEQUOT <<#0 = #["\\\"", AST_CHAR_SEQ];>>
	| AMPEAPOS <<#0 = #["\'", AST_CHAR_SEQ];>>
	)
;	

             
computedConstructor!:
	  cec:compElemConstructor  <<#0=#cec;>>
	| cac:compAttrConstructor  <<#0=#cac;>>
//	| cnsc:compNSConstructor   <<#0=#cnsc;>>
//	| compDocConstructor
//	| ctc:compTextConstructor  <<#0=#ctc;>>
//	| compXmlPI
//	| compXmlComment

;

compElemConstructor!:
	ELEMENT (  qn:qname
	           <<#0=#(#[AST_ELEMENT_NAME], #qn);>> 
/*
	         | ITEM //temporary decision !!!
	           <<#0=#(#[AST_ELEMENT_NAME], #(#[AST_QNAME], #["item", AST_LOCAL_NAME])); >>
*/
	         | (LBRACE en:expr RBRACE)
	           <<#0=#(#[AST_ELEMENT_NAME], #en);>>
	        )
 
                LBRACE {e:expr} RBRACE

	        <<#0=#(#[AST_ELEMENT], #0,
	               #[AST_ELEMENT_ATTRIBUTES], 
	               #(#[AST_CONTENT], #e));
	        >>
;

compAttrConstructor!:
	ATTRIBUTE
	        (  qn:qname
	           <<#0=#(#[AST_ATTRIBUTE_NAME], #qn);>> 
	         | (LBRACE en:expr RBRACE)
	           <<#0=#(#[AST_ATTRIBUTE_NAME], #en);>>
	        )
 
                LBRACE {e:expr} RBRACE

	        <<#0=#(#[AST_ATTRIBUTE], #0, #(#[AST_CONTENT], #e));
	        >>

;

/*
Not presented in current specification
compNSConstructor!:
	NAMESPACE n:NCNAME LBRACE e:expr RBRACE
	<<#0=#(#[AST_NAMESPACE],
	       #(#[AST_NAMESPACE_NAME], #[$n->getText(), AST_PREFIX]),
	       #(#[AST_CONTENT], #e));
	>>
;
*/
/*
compTextConstructor!:
	TEXT LBRACE e:expr RBRACE

	<<#0=#(#[AST_TEXT], #e);>>
;
*/
}