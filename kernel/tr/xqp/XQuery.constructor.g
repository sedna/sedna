/************************************************************
******* XQuery constructors syntactic analizer rules ********
*************************************************************/

class XQueryParser{

constructor!:
	  cc:computedConstructor <<#0=#cc;>>
	| dc:dirConstructor  <<#0=#dc;>>
//	| xc:xmlComment          <<#0=#xc;>>
//	| xpi:xmlPI              <<#0=#xpi;>>
//	| cdc:cdataSection       <<#0=#cdc;>>
;

dirConstructor!:
	  dec:dirElemConstructor <<#0=#dec;>>
	| dpic:dirPIConstructor	 <<#0=#dpic;>>
	| dcc:dirCommentConstructor  <<#0=#dcc;>>
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

	   <<//check whether local names of start and end tags are equal
	     if (strcmp(((AST*)(#qn1->down()))->getText(), ((AST*)(#qn2->down()))->getText()) != 0)
	        throw USER_EXCEPTION2(XPST0003, ("start tag " + std::string("\'<") + ((AST*)(#qn1->down()))->getText() + ">\'" +
	                                       " does not match close tag " + "\'</" + ((AST*)(#qn2->down()))->getText()  + ">\'" +
	                                       ", line: " + int2string(LT(1)->getLine())).c_str());

	     //check whether the prefixes consistency
	     if ((#qn1->down()->right() == NULL && #qn2->down()->right() != NULL) ||
	         (#qn1->down()->right() != NULL && #qn2->down()->right() == NULL)) 
	        throw USER_EXCEPTION2(XPST0003, (std::string("start tag does not match close tag, line: ") + int2string(LT(1)->getLine())).c_str());

	     if (#qn1->down()->right() != NULL && #qn2->down()->right() != NULL)
	     {
	        if (strcmp(((AST*)(#qn1->down()->right()))->getText(), ((AST*)(#qn2->down()->right()))->getText()) != 0)
	           throw USER_EXCEPTION2(XPST0003, (std::string("start tag does not match close tag, line: ") + int2string(LT(1)->getLine())).c_str());
	         
	     }


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
	| dec:dirConstructor
	<<
	  if (val.empty())
	  {
	     if (cnt == NULL) cnt = #dec; else cnt->append(#dec);
	  }
	  else
	  {

	     if (!is_preserve_boundary_space)
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
	     else
	     {
	         if (cnt == NULL)
	         {
	            cnt = #[val, AST_CHAR_SEQ];
	            cnt->append(#dec);
	         }
	         else
	         {
	            cnt->append(#[val, AST_CHAR_SEQ]);
	            cnt->append(#dec);
	         }
	     }
	  }

	  val = "";
	>>

	| ee:enclosedExpr     
	<<
	  if (val.empty())
	  {
	     if (cnt == NULL) cnt = #(#[AST_SPACE_SEQUENCE], #ee); else cnt->append(#(#[AST_SPACE_SEQUENCE], #ee));
	  }
	  else
	  {

	     if (!is_preserve_boundary_space)
	     {
	       bool isBoundWhiteSpace = true;
	       std::string::iterator it;
            
	       for (it = val.begin(); it != val.end(); it++)
	       {
	          if(!(*it == ' ' || *it == '\n' || *it == '\t' || *it == '\r'))
	            isBoundWhiteSpace = false;
	       }

	       if (cnt == NULL && isBoundWhiteSpace) 
	          cnt = #(#[AST_SPACE_SEQUENCE], #ee);
	       else
	       if (cnt == NULL && !isBoundWhiteSpace)
	       {
	          cnt = #[val, AST_CHAR_SEQ];
	          cnt->append(#(#[AST_SPACE_SEQUENCE], #ee));
	       } 
	       else
	       if ( cnt!= NULL && isBoundWhiteSpace)
	          cnt->append(#(#[AST_SPACE_SEQUENCE], #ee));
	       else
	       {
	          cnt->append(#[val, AST_CHAR_SEQ]);
	          cnt->append(#(#[AST_SPACE_SEQUENCE], #ee));
	       }
	     }
	     else
	     {
	         if (cnt == NULL)
	         {
	            cnt = #[val, AST_CHAR_SEQ];
	            cnt->append(#(#[AST_SPACE_SEQUENCE], #ee));
	         }
	         else
	         {
	            cnt->append(#[val, AST_CHAR_SEQ]);
	            cnt->append(#(#[AST_SPACE_SEQUENCE], #ee));
	         }
	     }

	  }

	  val = "";
	>>
	| per:predefinedEntityRef   << val += #per->getText();>>
	| cs:cdataSection           << val += #cs->getText();>>
	| cr:charRef <<val += #cr->getText();>>
//	| xmlComment
//	| xmlPI
	)*

	<<   
	  //d_printf2("XQParser: constr_val=%s\n", val.c_str());

	  if ( !(val.empty()) )
	  {
	     if (!is_preserve_boundary_space)
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
	         if (cnt == NULL)
	         {
	            #0 = #[val, AST_CHAR_SEQ];
	         }
	         else
	         {
	            #0=cnt;
	            #0->append(#[val, AST_CHAR_SEQ]);
	         }
	     }

	  }
	  else
	  {
	     #0 = cnt;
	  }   
	>>
;

dirPIConstructor!:
    << 
        std::string val = ""; 
        int line = 0;
    >>
	
	OPENPI << line = LT(1)->getLine(); >>

	( PI_QUESTION << val += "?"; >>
	| cs:PI_CHAR 
	    << if ((cs->getText())[0] == '\"') val += "\\\"";
           else if ((cs->getText())[0] == '\\') val += "\\\\";
	       else val += (cs->getText())[0]; >>
	)*

	PI_BRACKET
	<<  val.erase(val.size()-1);
	    std::string name, content;
	    parse_processing_instruction(val, name, content, line);

        #0 = #(#[AST_PI_CONSTR], #[name, AST_LOCAL_NAME]);

        if(!content.empty())
            #0->addChild(#[content, AST_STRING_CONST]); 
    >>
;


dirCommentConstructor!:
    << std::string val = ""; >>
	
	XMLCOMMENTOPEN 
	
	( XML_COMMENT_HYPHEN << val+= "-"; >>
    | cs1:XML_COMMENT_CHAR 
         <<if ((cs1->getText())[0] == '\"') val += "\\\"";
           else if ((cs1->getText())[0] == '\\') val += "\\\\";
	       else val += (cs1->getText())[0]; >>
	)*

	XMLCOMMENTCLOSE
	<<  val.erase(val.size()-1);
        #0 = #(#[AST_COMMENT_CONSTR], #[val, AST_STRING_CONST]); >>
;


cdataSection!:
    << std::string val = ""; >>
	
	OPENCDATA   
	
	( CHAR_SEQ_CDATA << val += "]"; >>
	| cs1:CHAR_SEQ_CDATA_C 
	    << if ((cs1->getText())[0] == '\"') val += "\\\"";
           else if ((cs1->getText())[0] == '\\') val += "\\\\";
	       else val += (cs1->getText())[0]; >>
	)*

	CHAR_SEQ_CDATA_C1
	<<  val.erase(val.size()-2);
	    #0 = #[val, AST_CHAR_SEQ];  >>
;


attributeList!:
	<<ASTBase *atts=NULL;bool isDef=true;>>
	((WS|NL)+ 
	{ ( qn:qname (WS|NL)* EQUAL (WS|NL)* av1:attributeValue

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
	| cr:charRef <<val += #cr->getText();>>
	| ee:enclosedExpr 
	<<
	  if (val.empty())
	  {
	    if (cnt == NULL) 
	      cnt = #(#[AST_SPACE_SEQUENCE], #ee);
	    else 
	      cnt->append(#(#[AST_SPACE_SEQUENCE], #ee)); 
	  }
	  else
	  {
	    if ( cnt == NULL)
	    {
	       cnt = #[val, AST_CHAR_SEQ];
	       cnt->append(#(#[AST_SPACE_SEQUENCE], #ee));
	    }
	    else
	    {
	       cnt->append(#[val, AST_CHAR_SEQ]);
	       cnt->append(#(#[AST_SPACE_SEQUENCE], #ee));
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
	| cr:charRef <<val += #cr->getText();>>
	| ee:enclosedExpr 
	<<
	  if (val.empty())
	  {
	    if (cnt == NULL) 
	      cnt = #(#[AST_SPACE_SEQUENCE], #ee);
	    else 
	      cnt->append(#(#[AST_SPACE_SEQUENCE], #ee)); 
	  }
	  else
	  {
	    if ( cnt == NULL)
	    {
	       cnt = #[val, AST_CHAR_SEQ];
	       cnt->append(#(#[AST_SPACE_SEQUENCE], #ee));
	    }
	    else
	    {
	       cnt->append(#[val, AST_CHAR_SEQ]);
	       cnt->append(#(#[AST_SPACE_SEQUENCE], #ee));
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

charRef!:
	(  d:DEC_CHARREF <<#0=#[$d->getText(), AST_CHAR_SEQ];>>
	 | h:HEX_CHARREF <<#0=#[$h->getText(), AST_CHAR_SEQ];>>
	)
;
             
computedConstructor!:
	  cec:compElemConstructor  <<#0=#cec;>>
	| cac:compAttrConstructor  <<#0=#cac;>>
//	| cnsc:compNSConstructor   <<#0=#cnsc;>>
	| cdc:compDocConstructor   <<#0=#cdc;>>
	| ctc:compTextConstructor  <<#0=#ctc;>>
	| cxpi:compXmlPI           <<#0=#cxpi;>>
	| cxc:compXmlComment       <<#0=#cxc;>>

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

	        <<
                  ASTBase* _e_ = NULL;
                  if (#e != NULL)
                     _e_ = #(#[AST_SPACE_SEQUENCE], #e);
	          #0=#(#[AST_ELEMENT], #0,
	               #[AST_ELEMENT_ATTRIBUTES], 
	               #(#[AST_CONTENT], _e_));
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

	        <<
                  ASTBase* _e_ = NULL;
                  if (#e != NULL)
                     _e_ = #(#[AST_SPACE_SEQUENCE], #e);

	          #0=#(#[AST_ATTRIBUTE], #0, #(#[AST_CONTENT], _e_));
	        >>

;

/*
compNSConstructor!:
	NAMESPACE n:NCNAME LBRACE e:expr RBRACE
	<<#0=#(#[AST_NAMESPACE],
	       #(#[AST_NAMESPACE_NAME], #[$n->getText(), AST_PREFIX]),
	       #(#[AST_CONTENT], #e));
	>>
;
*/

compDocConstructor!:
	LDOCUMENT LBRACE e:expr RBRACE

	<<#0=#(#[AST_DOCUMENT_CONSTR], #(#[AST_SPACE_SEQUENCE], #e));>>
	
;

compTextConstructor!:
	TEXT LBRACE e:expr RBRACE

	<<#0=#(#[AST_TEXT_CONSTR], #(#[AST_SPACE_SEQUENCE], #e));>>
;

compXmlPI!:
	PROCESSING_INSTRUCTION 
	(  n:NCNAME 
	 | (LBRACE e1:expr RBRACE)
	)
	LBRACE { e2:expr } RBRACE
	<<
	  if (#e1 == NULL)
	  {
	     #0=#(#[AST_PI_CONSTR], #[$n->getText(), AST_LOCAL_NAME], #(#[AST_SPACE_SEQUENCE], #e2));
	  }
	  else
	  {
	     #0=#(#[AST_PI_CONSTR], #e1, #(#[AST_SPACE_SEQUENCE], #e2));
	  }
	>>
;

compXmlComment!:
	COMMENT_ LBRACE e:expr RBRACE

	<<#0=#(#[AST_COMMENT_CONSTR], #(#[AST_SPACE_SEQUENCE], #e));>>
;

}