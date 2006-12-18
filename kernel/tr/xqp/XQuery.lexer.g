/******************************************************
************** XQuery lexical analizer*****************
*******************************************************/

#lexaction
<<
#include <stack>
#include <stdlib.h>
#include <iostream>

#include "exceptions.h"
#include "types.h"

using namespace std;

STACK_INT Lexical_Analizer_State;

bool s_after_pi_target = false; 
>>

#lexprefix
<<
#include <stdio.h>
#include "exceptions.h"
>>

#lexmember
<<
virtual void errstd(const char *s){
  throw USER_EXCEPTION2(XPST0003 , (std::string("unknown token: ")+"\'" + lextext()+ "\'" + ", " + "line: " + int2string(line())).c_str());
}

>>

/******************* START (default) tokens *************/

#lexclass START

#token STMNT_DELIM "[\n][\\]" <<newline();>>
#token "[\ \t\r]+" <<skip();>>
#token "[\n]" <<skip(); newline();>>


//-------------------------- keywords ------------------

#token CASE_ "case"
#token TYPESWITCH "typeswitch"
#token DECLARE "declare"
#token FUNCTION "function"
#token EXTERNAL "external"
#token NAMESPACE "namespace"
#token DEFAULT "default"
#token BOUNDARYSPACE "boundary\-space"
#token PRESERVE  "preserve"
#token STRIP      "strip"
#token OPTION  "option"
#token FOR "for"
#token LET "let"
#token IN_ "in"
#token WHERE "where"
#token STABLE "stable"
#token ORDERING "ordering"
#token ORDERED "ordered"
#token ORDER "order"
#token UNORDERED "unordered"
#token BY "by"
#token CBY "BY"
#token AT_ "at"
#token ASCENDING "ascending"
#token DESCENDING "descending"
#token GREATEST "greatest"
#token LEAST "least"
#token COLLATION "collation"
#token RETURN "return"
#token SOME "some"
#token EVERY "every"
#token SATISFIES "satisfies"
#token IF "if"
#token THEN "then"
#token ELSE "else"
#token ELEMENT "element"
#token OF "of"
#token TYPE "type"
#token CTYPE "TYPE"
#token ATTRIBUTE "attribute"
#token XMLNS "xmlns"
#token DEFINE  "define"
#token EMPTYSEQ "empty\-sequence"
#token EMPTY "empty"
//#token EMPTY_GREATEST "empty([\ \t\n])*greatest"
//#token EMPTY_LEAST "empty([\ \t\n])*least"
//#token EMPTY_TYPE "empty([\ \t\n])*\(([\ \t\n])*\)"
//#token ITEM_TEST "item([\ \t\n])*[\(]([\ \t\n])*[\)]"
#token ITEM "item"
#token TEXT "text"
#token NILLABLE "nillable"
#token PROCESSING_INSTRUCTION "processing\-instruction"
#token COMMENT_ "comment"
#token NODE "node"
#token DOCUMENT "document\-node"
#token LDOCUMENT "document"
#token CHILD "child"
#token DESCENDANT "descendant"
#token SELF "self"
#token DESCENDANT_OR_SELF "descendant\-or\-self"
#token FOLLOWING_SIBLING "following\-sibling"
#token FOLLOWING "following"
#token PARENT "parent"
#token ANCESTOR "ancestor"
#token PRECEDING_SIBLING "preceding\-sibling"
#token PRECEDING "preceding"
#token ANCESTOR_OR_SELF "ancestor\-or\-self"
#token CASTABLE "castable"
#token AS "as"
#token CAS "AS"
#token CAST "cast"
#token TO "to"
#token INSTANCE "instance"
#token TREAT "treat"
#token IMPORT "import"
#token MODULE "module"
#token VALIDATE "validate"
#token LAX "lax"
#token STRICT_ "strict"
#token SSHEMA "schema"
#token BASEURI "base\-uri"
#token CONSTRUCTION "construction"
#token COPYNAMESPACE "copy\-namespaces"
#token NOPRESERVE "no\-preserve"
#token INHERIT "inherit"
#token NOINHERIT "no\-inherit"
#token VARIABLE "variable"
#token XQUERY "xquery"
#token VERSION "version"
#token ENCODING "encoding"

/*  This keywords are added for update expressions      */
#token UPDATE "UPDATE" <<mode(UPDATE_CLASS);>>
//#token INSERT "insert"
#token INTO "into"
//#token DELETE_ "delete"
//#token DELETE_UNDEEP "delete_undeep"
//#token REPLACE "replace"
#token WITH "with"
//#token RENAME "rename"
//#token MOVE "move"
#token ON "on"
#token CON "ON"

/*  This keywords are added for manage expressions      */
#token CREATE "CREATE"
#token DROP "DROP"
#token GRANT "GRANT"
#token REVOKE "REVOKE"
#token LOADFILE "LOADFILE"
#token LOAD "LOAD"
#token ALTER "ALTER"

#token ROLE "ROLE"
#token DATABASE "DATABASE"
#token INDEX "INDEX"
#token FULLTEXT "FULL\-TEXT"
#token CTO "TO"
#token FROM "FROM"
#token STDIN "STDIN"
#token USER "USER"
#token PASSWORD "PASSWORD"
#token ALL "ALL"
#token PUBLIC "PUBLIC"
#token TRIGGER "TRIGGER"
#token BEFORE "BEFORE"
#token AFTER "AFTER"
#token CINSERT "INSERT"
#token CDELETE "DELETE"
#token CREPLACE "REPLACE"
#token EACH "EACH"
#token CNODE "NODE"
#token STATEMENT "STATEMENT"
#token CDO "DO"

/* transaction manage operations */
#token COMMIT "COMMIT"
#token ROLLBACK "ROLLBACK"

/*  This keywords are added for retrieve metadata expressions      */
#token RETRIEVE "RETRIEVE"

#token METADATA "METADATA"
#token FOR_ "FOR"
#token DOCUMENTS "DOCUMENTS"
#token CDOCUMENT "DOCUMENT"
#token COLLECTIONS "COLLECTIONS"
#token CCOLLECTION  "COLLECTION"
#token CIN_ "IN"
#token DESCRIPTIVE "DESCRIPTIVE"
#token SCHEMA "SCHEMA"
#token CWITH "WITH"
#token STATISTICS "STATISTICS"
#token CMODULE "MODULE"
#token COR "OR"


//-------------------------- operations -----------------

#token OR "or"
#token AND "and"
#token PLUS "\+"
#token MINUS "\-"
#token DIV "div"
#token IDIV "idiv"
#token MOD "mod"
#token EQ "eq"
#token NE "ne"
#token LT_ "lt"
#token LE "le"
#token GT "gt"
#token GE "ge"
#token EQUAL "="
#token NOTEQUAL "!="
#token LESS "\<[\ ]"
#token LESSEQUAL "\<="
#token GREAT "\>"
#token GREATEQUAL "\>="
#token IDENT "is"
#token LESS_DOC_ORDER "\<\<"
#token GREAT_DOC_ORDER "\>\>"
#token UNION "union"
#token ALT "\|"
#token INTERSECT "intersect"
#token EXCEPT "except"

#token PR_OPEN "\(\#"

<<
  mode(PRAGMA_CONTENT);
  Lexical_Analizer_State.push(START);
>>


#token SLASH "\/"
#token SLASHSLASH "\/\/"
#token DOT "."
#token DOTDOT ".."
#token STAR "\*"
#token QMARK "\?"
//#token DOUBLELBRACE "\{\{"
//#token DOUBLERBRACE "\}\}"


//--------------------------- delimeters -------------------
#token DOLLAR "$"
#token AT "\@"
#token LBRACK "\["
#token RBRACK "\]"
#token LPAR "\("
#token RPAR "\)"
#token LBRACE "\{"

<<
  mode (START);
  Lexical_Analizer_State.push(START);
>>
#token RBRACE "\}"
<<
  if (!Lexical_Analizer_State.empty())
  {
     mode (Lexical_Analizer_State.top());
     Lexical_Analizer_State.pop();
  }
>>

#token COMMA ","
#token SEMICOLON ";"
#token DOUBLECOLON "\:\:"
#token COLON "\:"

#token OPENCDATA "\<\!\[CDATA\["
<<
  mode (XML_CDATA_SECTION);
  Lexical_Analizer_State.push(START);
>>

#token COLONEQUALS  ":="
#token "\(\:" 
<<
  mode (COMMENT);
  Lexical_Analizer_State.push(START);
  skip (); 
>>
//----------------- identifier and const literals -----------

//#token QNAME "[_a-zA-Z]([a-zA-Z0-9_\-])*"
#token NCNAME "[_a-zA-Z\0x80-\0xfe]([a-zA-Z0-9_\-\.\0x80-\0xfe])*" //it is the subset of specification
//#token FULLQNAME "[_a-zA-Z\0x80-\0xfe]([a-zA-Z0-9_\-\.\0x80-\0xfe])*:[_a-zA-Z\0x80-\0xfe]([a-zA-Z0-9_\-\.\0x80-\0xfe])*"
#token INTEGERLITERAL  "[0-9]+" <<replstr((std::string("\"") + lextext() + std::string("\"")).c_str());>>
#token DOUBLELITERAL   "((\.[0-9]+) | ([0-9]+{\.[0-9]*})) (e | E) {\+| \-} [0-9]+" <<replstr((std::string("\"") + lextext() + std::string("\"")).c_str());>>
#token DECIMALLITERAL  "(\.[0-9]+) |  ([0-9]+\.[0-9]*)" <<replstr((std::string("\"") + lextext() + std::string("\"")).c_str());>>
#token STRINGLITERAL "(\"(&lt;|&gt;|&amp;|&quot;|&apos;|\"\"| &#([0-9])+; | &#x([0-9a-fA-F])+; |~[\"&])*\") | (\'(&lt;|&gt;|&amp;|&quot;|&apos;|\'\'| &#([0-9])+; | &#x([0-9a-fA-F])+; | ~[\'&])*\')"
<<
  std::string res = erase_doublequot(lextext());
  replstr(res.c_str());
//  replstr((std::string("\"") + res + std::string("\"")).c_str());
  replace_entity(lextext(), "&lt;", "<");
  replace_entity(lextext(), "&gt;", ">");
  replace_entity(lextext(), "&quot;", "\\\"");
  replace_entity(lextext(), "&apos;", "\\\'");
  replace_entity(lextext(), "&amp;", "&");
  res = replace_charref(lextext());
  replstr(res.c_str());

  int num = strlen(lextext());
  for (int i=0;i<num; i++)
      if ((lextext())[i] == '\n') 
         newline();
>>

#token DEC_CHARREF "&#([0-9])+;" 
<<
  std::string res;
  res = replace_charref(lextext());
  replstr(res.c_str());
>>
#token HEX_CHARREF "&#x([0-9a-fA-F])+;"
<<
  std::string res;
  res = replace_charref(lextext());
  replstr(res.c_str());
>>


// tokens for processing instruction constructor

#token LPI "\<\?" 
<<
  s_after_pi_target = false; mode(PITARGET);
  Lexical_Analizer_State.push(START);
>>
#token RPI "\?\>"

//----------------- tokens in element constructor -----------
#token STARTTAGOPEN "\<"   
<<
  mode (XML_OPEN_TAG);
  Lexical_Analizer_State.push(START);
>>

//----------------- tokens for comment constructor ----------

#token XMLCOMMENTOPEN  "\<!\-\-"
<<
  mode(XML_COMMENT);
  Lexical_Analizer_State.push(START);
>>



/********************* OPEN TAG TOKENS ************************/

#lexclass XML_OPEN_TAG

#token STARTTAGCLOSE "\>"
<<
  mode (XML_ELEMENT_CONTENT);
//  printf("transition to xml element content\n");
>>
#token EMPTYTAGCLOSE "\/\>"
<<
 
  if (!Lexical_Analizer_State.empty())
  {
     mode (Lexical_Analizer_State.top());
     Lexical_Analizer_State.pop();
  }
>>
//#token QNAME "[_a-zA-Z]([a-zA-Z0-9_\-])*"
#token XMLNS "xmlns"
#token NCNAME "[_a-zA-Z\0x80-\0xfe]([a-zA-Z0-9_\-\.\0x80-\0xfe])*" //it is the subset of specification
//#token FULLQNAME "[_a-zA-Z\0x80-\0xfe]([a-zA-Z0-9_\-\.\0x80-\0xfe])*:[_a-zA-Z\0x80-\0xfe]([a-zA-Z0-9_\-\.\0x80-\0xfe])*"


//#token 	     "[\ \t]*" <<skip();>>
#token WS "[\ \t\r]+"
#token NL "([\n])+" 
<<
  int num = strlen(lextext());
  for (int i=0; i<num; i++ )
      if ((lextext())[i] == '\n')
         newline();
>>
#token COLON ":"
#token EQUAL "\="

#token LBRACE "\{"
<<
  mode (START);
  Lexical_Analizer_State.push(XML_OPEN_TAG);
>>
#token QUOT "\""
<<
  mode (XML_QUOT_ATTRIBUTE_CONTENT);
//  printf("to attribute value content\n");
>>
#token APOS "\'"
<<
  mode (XML_APOS_ATTRIBUTE_CONTENT);
>>


//#token LEXERROR "~[]*"  <<printf("\n\nLexical error!!!\n\n");>>

/****************** XML ELEMENT CONTENT TOKENS ****************/

#lexclass XML_ELEMENT_CONTENT

#token ENDTAGOPEN "\<\/"
<<
  mode (XML_END_TAG);
//  printf("transistion to xml end tag\n");
>>
#token STARTTAGOPEN "\<"
<<
  mode (XML_OPEN_TAG);
  Lexical_Analizer_State.push(XML_ELEMENT_CONTENT);
>>
#token LBRACE "\{"
<<
  mode (START);
  Lexical_Analizer_State.push(XML_ELEMENT_CONTENT);
>>

#token LPI "\<\?" 
<<
  s_after_pi_target = false; mode(PITARGET);
  Lexical_Analizer_State.push(XML_ELEMENT_CONTENT);
>>


#token XMLCOMMENTOPEN  "\<!\-\-"
<<
  mode(XML_COMMENT);
  Lexical_Analizer_State.push(XML_ELEMENT_CONTENT);
>>


#token DOUBLELBRACE "\{\{"
#token DOUBLERBRACE "\}\}"

#token AMPLT "&lt;"
#token AMPGT "&gt;"
#token AMPAMP "&amp;"
#token AMPEQUOT "&quot;"
#token AMPEAPOS "&apos;"
#token DEC_CHARREF "&#([0-9])+;"
<<
  std::string res;
  res = replace_charref(lextext());
  replstr(res.c_str());
>>

#token HEX_CHARREF "&#x([0-9a-fA-F])+;"
<<
  std::string res;
  res = replace_charref(lextext());
  replstr(res.c_str());
>>


#token OPENCDATA "\<\!\[CDATA\["
<<
  mode (XML_CDATA_SECTION);
  Lexical_Analizer_State.push(XML_CDATA_SECTION);
>>

#token XMLCOMMENTOPEN  "\<\-\-" 
<<
  mode(XML_COMMENT);
  Lexical_Analizer_State.push(XML_ELEMENT_CONTENT);
>>


#token CHAR_ELEM "~[\{\}\<\&]"  
<<
  int num = strlen(lextext());
  for (int i=0;i<num; i++)
      if ((lextext())[i] == '\n') 
         newline();
>>

//#token LEXERROR "~[]*"  <<printf("\n\nLexical error!!!\n\n");>>
/***************** XML CDATA SECTION TOKENS *******************/

#lexclass XML_CDATA_SECTION

#token CLOSECDATA "\]\]\>"
<<
  if (!Lexical_Analizer_State.empty())
  {
     mode (Lexical_Analizer_State.top());
     Lexical_Analizer_State.pop();
  }
>>
#token RBRACK "\]"
#token RBRANGL "\>" 

#token CHAR_SEQ_CDATA "(~[\]\>])*"
<<
  int num = strlen(lextext());
  for (int i=0;i<num; i++)
      if ((lextext())[i] == '\n') 
         newline();
>>


/******************* CLOSE TAG TOKENS *************************/

#lexclass XML_END_TAG

#token ENDTAGCLOSE "\>"
<<
  if (!Lexical_Analizer_State.empty())
  {
     mode (Lexical_Analizer_State.top());
//  printf("transition to mode=%d\n",Lexical_Analizer_State.top());
     Lexical_Analizer_State.pop();
  }
>>
#token LBRACE "\{"
<<
  mode (START);
  Lexical_Analizer_State.push(XML_END_TAG);
>>
//#token QNAME "[_a-zA-Z]([a-zA-Z0-9_\-])*" <<printf("xml end tag QNAME\n");>>
#token NCNAME "[_a-zA-Z\0x80-\0xfe]([a-zA-Z0-9_\-\.\0x80-\0xfe])*" //it is the subset of specification
//#token FULLQNAME "[_a-zA-Z\0x80-\0xfe]([a-zA-Z0-9_\-\.\0x80-\0xfe])*:[_a-zA-Z\0x80-\0xfe]([a-zA-Z0-9_\-\.\0x80-\0xfe])*"
#token COLON "\:"
#token WS "[\ \t\r]+"
#token NL "([\n])+" 
<<
  int num = strlen(lextext());
  for (int i=0; i< num; i++)
      if ((lextext())[i] == '\n')
          newline();
>>

//#token LEXERROR "~[]*"  <<printf("\n\nLexical error!!!\n\n");>>

/******************** ATTRIBUTE CONTENT TOKENS ****************/

#lexclass XML_QUOT_ATTRIBUTE_CONTENT 

#token QUOT "\"" << mode (XML_OPEN_TAG); >>
#token APOS "\'"

#token LBRACE "\{"
<<
  mode (START);
  Lexical_Analizer_State.push(XML_QUOT_ATTRIBUTE_CONTENT);
>>
#token DOUBLELBRACE "\{\{"
#token DOUBLERBRACE "\}\}"

#token DOUBLEQUOT "\"\""

#token DEC_CHARREF "&#([0-9])+;"
<<
  std::string res;
  res = replace_charref(lextext());
  replstr(res.c_str());
>>

#token HEX_CHARREF "&#x([0-9a-fA-F])+;"
<<
  std::string res;
  res = replace_charref(lextext());
  replstr(res.c_str());
>>


#token CHAR_ATTR "~[\"\'\{\}\<\&]"
<<
  int num = strlen(lextext());
  for (int i=0;i<num; i++)
      if ((lextext())[i] == '\n') 
         newline();
>>




#token AMPLT "&lt;"
#token AMPGT "&gt;"
#token AMPAMP "&amp;"
#token AMPEQUOT "&quot;"
#token AMPAPOS "&apos;"



#lexclass XML_APOS_ATTRIBUTE_CONTENT 

#token QUOT "\"" 
#token APOS "\'" << mode (XML_OPEN_TAG); >>

#token LBRACE "\{"
<<
  mode (START);
  Lexical_Analizer_State.push(XML_APOS_ATTRIBUTE_CONTENT);
>>
#token DOUBLELBRACE "\{\{"
#token DOUBLERBRACE "\}\}"

#token DOUBLEAPOS "\'\'"

#token DEC_CHARREF "&#([0-9])+;"
<<
  std::string res;
  res = replace_charref(lextext());
  replstr(res.c_str());
>>

#token HEX_CHARREF "&#x([0-9a-fA-F])+;"
<<
  std::string res;
  res = replace_charref(lextext());
  replstr(res.c_str());
>>


#token CHAR_ATTR "~[\"\'\{\}\<\&]"
<<
  int num = strlen(lextext());
  for (int i=0;i<num; i++)
      if ((lextext())[i] == '\n') 
         newline();
>>


#token AMPLT "&lt;"
#token AMPGT "&gt;"
#token AMPAMP "&amp;"
#token AMPEQUOT "&quot;"
#token AMPEAPOS "&apos;"


/****************** XML COMMENT TOKENS ************************/

#lexclass COMMENT

#token "([\n])"      << skip(); newline(); >>
#token "\(\:"        << 
                       mode (COMMENT);
                       Lexical_Analizer_State.push(COMMENT);
                       skip (); 
                     >>

#token "\:\)"        << 
                        if (!Lexical_Analizer_State.empty())
                        {
                            mode (Lexical_Analizer_State.top());
                            Lexical_Analizer_State.pop();
                            skip ();
                        }
                     >>
#token "\:"          << skip (); >>
#token "\("          << skip (); >>
#token "@"           <<throw USER_EXCEPTION2(XPST0003, (std::string("bad comments, ") + "line: " + int2string(line())).c_str());>>
#token "~[\:\(\n@]+"  << skip (); >>

//#token LEXERROR "~[]*"  <<printf("\n\nLexical error!!!\n\n");>>

//--------------------------- tokens for debug --------------
#token FUNC "func"
#token EXPR "expr"

/*
#lexclass MANAGE

#token "[\ \t]+" <<skip();>>
#token "[\n\r]"  <<skip(); newline();>>
#token STMNT_DELIM "\n\\" <<mode(START);newline();>>
#token "\(\:" 
<<
  mode (COMMENT);
  Lexical_Analizer_State.push(MANAGE);
  skip (); 
>>


#token ROLE "ROLE"
#token ON "ON"
#token DATABASE "DATABASE"
#token INDEX "INDEX" <<mode(START);>>
#token TO "TO"
#token FROM "FROM"
#token COLLECTION "COLLECTION"
#token DOCUMENT "DOCUMENT"
#token IN__ "IN"
#token STDIN "STDIN"
#token STRINGLITERAL "(\"(&lt;|&gt;|&amp;|&quot;|&apos;|\"\"|~[\"&])*\") | (\'(&lt;|&gt;|&amp;|&quot;|&apos;|\'\'|~[\'&])*\')"
<<
  erase_doublequot(lextext());
  replace_entity(lextext(), "&lt;", "<");
  replace_entity(lextext(), "&gt;", ">");
  replace_entity(lextext(), "&amp;", "&");
  replace_entity(lextext(), "&quot;", "\"");
  replace_entity(lextext(), "&apos;", "\'");

  int num = strlen(lextext());
  for (int i=0;i<num; i++)
      if ((lextext())[i] == '\n') 
         newline();

>>

#token USER "USER"
#token WITH "WITH"
#token PASSWORD "PASSWORD"


#lexclass RETRIEVE_METADATA

#token "[\ \t]+" <<skip();>>
#token "\n"  <<skip(); newline();>>
#token STMNT_DELIM "\n\\" <<mode(START);newline();>>
#token "\(\:" 
<<
  mode (COMMENT);
  Lexical_Analizer_State.push(RETRIEVE_METADATA);
  skip (); 
>>


#token METADATA "METADATA"
#token FOR_ "FOR"
#token DOCUMENTS "DOCUMENTS"
#token DOCUMENT "DOCUMENT"
#token COLLECTIONS "COLLECTIONS"
#token COLLECTION  "COLLECTION"
#token IN_ "IN"
#token DESCRIPTIVE "DESCRIPTIVE"
#token SCHEMA "SCHEMA"
#token WITH "WITH"
#token STATISTICS "STATISTICS"

#token STRINGLITERAL "(\"(&lt;|&gt;|&amp;|&quot;|&apos;|\"\"|~[\"&])*\") | (\'(&lt;|&gt;|&amp;|&quot;|&apos;|\'\'|~[\'&])*\')"
<<
  erase_doublequot(lextext());
  replace_entity(lextext(), "&lt;", "<");
  replace_entity(lextext(), "&gt;", ">");
  replace_entity(lextext(), "&amp;", "&");
  replace_entity(lextext(), "&quot;", "\"");
  replace_entity(lextext(), "&apos;", "\'");
  int num = strlen(lextext());
  for (int i=0;i<num; i++)
      if ((lextext())[i] == '\n') 
         newline();

>>
*/

#lexclass PITARGET

#token WS "[\ \t\r]*" <<if (s_after_pi_target) mode(PIINSTRUCTION);>>
#token NL "([\n])*"
<<
  int num = strlen(lextext());
  for (int i=0; i<num; i++ )
      if ((lextext())[i] == '\n')
         newline();

  if (s_after_pi_target)
  {
     mode(PIINSTRUCTION);
  }
>>

#token NAME "[_a-zA-Z]([a-zA-Z0-9_\-\.])*" <<s_after_pi_target = true;>>

#token RPI "\?\>" 
<<
  mode (Lexical_Analizer_State.top());
  Lexical_Analizer_State.pop();
>>

#lexclass  PIINSTRUCTION


#token INSTRUCTION "~[\?\>]+" 
<<
  string res;
  res = escape_quot(std::string(lextext()));

  res = std::string("\"") + res + std::string("\"");
  replstr(res.c_str());

//  mode (Lexical_Analizer_State.top());
//  Lexical_Analizer_State.pop();
>>

#token RPI "\?\>"
<<
  mode (Lexical_Analizer_State.top());
  Lexical_Analizer_State.pop();
>>

#lexclass XML_COMMENT

//#token XMLCOMMENTCONTENT "(~[\-] | ([\-](~[\-])))*" <<printf("XML COMMENT!!!!\n\n");>>

#token XMLCOMMENTCLOSE    "\-\-\>"
<<
  mode (Lexical_Analizer_State.top());
  Lexical_Analizer_State.pop();
>>

#token XMLCOMMENTCONTENT "(~[\-])+"

#lexclass PRAGMA_CONTENT

#token PR_CLOSE "\#\)"
<<
  mode (Lexical_Analizer_State.top());
  Lexical_Analizer_State.pop();
>>

#token PR_CONTENT "(~[\#])*" <<printf("PR_CONTENT=%s\n", lextext());>>

#lexclass UPDATE_CLASS

#token "[\ \t\r]+" <<skip();>>
#token "([\n])" <<skip(); newline();>>
#token STMNT_DELIM "([\n])[\\]" <<mode(START);newline();>>

#token INSERT "insert" <<mode (START);>>
#token DELETE_ "delete" <<mode(START);>>
#token DELETE_UNDEEP "delete_undeep" <<mode (START);>>
#token REPLACE "replace" <<mode(START);>>
#token RENAME "rename" <<mode(START);>>
#token MOVE "move"  <<mode(START);>>
#token "\(\:" 
<<
  mode (COMMENT);
  Lexical_Analizer_State.push(UPDATE_CLASS);
  skip (); 
>>

