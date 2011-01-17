    /*
    * File:  XQParser.y
    * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
    */

%skeleton "lalr1.cc"
%require "2.3"
%defines
%name-prefix="sedna"
%define "parser_class_name" "XQueryParser"
%error-verbose

%{

namespace sedna
{
    class XQueryDriver;
}

#include "ast/AST.h"

%}

%parse-param {XQueryDriver& driver}

%locations

%union
{
    std::string *littext; /* text for string and numeric literals */
    std::string *qname;  /* text for NCName and QName */
    std::string *scont;  /* text for some textual content (e.g. element and attribute constructor content) */
    ASTNode *node; /* AST tree for the given expression */
    ASTNodesVector *node_list; /* list containing any sequence of AST nodes */
    ASTStringVector *str_list; /* list containing any sequence of std::string nodes */
    int x; /* variable to store enum values mostly; semantics of this int (i.e., type of enum) is decided in-place */
    int isUpper; /* signals that token is in upper-case */
};


%{
#include "XQueryDriver.h"
%}


%token END 0 "end of file"
%token _ERROR_ "lexical error"
%token ST_SEP "statement separator"
%token MINUS "-"
%token PLUS "+"
%token COMMA ","
%token SEMI ";"
%token AXIS "::"
%token ASSIGN ":="
%token NE_SIGN "!="
%token QUEST "?"
%token SLASH "/"
%token SLASH_SLASH "//"
%token DOT "."
%token DOT_DOT ".."
%token LPAREN "("
%token RPAREN ")"
%token LBRACK "["
%token RBRACK "]"
%token ATSIGN "@"
%token DOLLAR "$"
%token STAR "*"
%token PREC "<<"
%token FOLLOW ">>"
%token BAR "|"
%token LE_SIGN "<="
%token EQ_SIGN "="
%token GT_SIGN ">"
%token GE_SIGN ">="
%token ANCESTOR "ancestor"
%token ANCESTOR_OR_SELF "ancestor-or-self"
%token AND "and"
%token AS "as"
%token ASCENDING "ascending"
%token AT "at"
%token ATTRIBUTE "attribute"
%token BASE_URI "base-uri"
%token BOUNDARY_SPACE "boundary-space"
%token BY "by"
%token CASE "case"
%token CAST "cast"
%token CASTABLE "castable"
%token CHILD "child"
%token COLLATION "collation"
%token COMMENT "comment"
%token CONSTRUCTION "construction"
%token COPY_NAMESPACES "copy-namespaces"
%token DECLARE "declare"
%token DEFAULT "default"
%token DESCENDANT "descendant"
%token DESCENDANT_OR_SELF "descendant-or-self"
%token DESCENDING "descending"
%token DIV "div"
%token DOCUMENT "document"
%token DOCUMENT_NODE "document-node"
%token ELEMENT "element"
%token ELSE "else"
%token EMPTY "empty"
%token EMPTY_SEQUENCE "empty-sequence"
%token ENCODING "encoding"
%token EQ "eq"
%token EVERY "every"
%token EXCEPT "except"
%token EXTERNAL "external"
%token FOLLOWING "following"
%token FOLLOWING_SIBLING "following-sibling"
%token FOR "for"
%token FUNCTION "function"
%token GE "ge"
%token GREATEST "greatest"
%token GT "gt"
%token IDIV "idiv"
%token IF "if"
%token IMPORT "import"
%token _IN "in"
%token INHERIT "inherit"
%token INSTANCE "instance"
%token INTERSECT "intersect"
%token IS "is"
%token ITEM "item"
%token LAX "lax"
%token LE "le"
%token LEAST "least"
%token LET "let"
%token LT "lt"
%token MOD "mod"
%token MODULE "module"
%token NAMESPACE "namespace"
%token NE "ne"
%token NODE "node"
%token NO_INHERIT "no-inherit"
%token NO_PRESERVE "no-preserve"
%token OF "of"
%token OPTION "option"
%token OR "or"
%token ORDER "order"
%token ORDERED "ordered"
%token ORDERING "ordering"
%token PARENT "parent"
%token PRECEDING "preceding"
%token PRECEDING_SIBLING "preceding-sibling"
%token PRESERVE "preserve"
%token PROCESSING_INSTRUCTION "processing-instruction"
%token RETURN "return"
%token SATISFIES "satisfies"
%token SCHEMA "schema"
%token SCHEMA_ATTRIBUTE "schema-attribute"
%token SCHEMA_ELEMENT "schema-element"
%token SELF "self"
%token SOME "some"
%token STABLE "stable"
%token _STRICT "strict"
%token STRIP "strip"
%token TEXT "text"
%token THEN "then"
%token TO "to"
%token TREAT "treat"
%token TYPESWITCH "typeswitch"
%token UNION "union"
%token UNORDERED "unordered"
%token VALIDATE "validate"
%token VARIABLE "variable"
%token VERSION "version"
%token WHERE "where"
%token XQUERY "xquery"

    /* Sedna update and other extensions tokens */
%token <isUpper> ALL "all"
%token <isUpper> AFTER "after"
%token <isUpper> ALTER "alter"
%token <isUpper> AS_ "AS"
%token <isUpper> BEFORE "before"
%token <isUpper> BY_ "BY"
%token <isUpper> COLLECTION "collection"
%token <isUpper> COLLECTIONS "collections"
%token <isUpper> CREATE "create"
%token <isUpper> DATABASE "database"
%token <isUpper> _DELETE "delete"
%token <isUpper> DELETE_UNDEEP "delete_undeep"
%token <isUpper> DESCRIPTIVE "descriptive"
%token <isUpper> DO "do"
%token <isUpper> DOCUMENT_ "DOCUMENT"
%token <isUpper> DOCUMENTS "documents"
%token <isUpper> DROP "drop"
%token <isUpper> EACH "each"
%token <isUpper> EXPLAIN "explain"
%token <isUpper> FOLLOWING_ "FOLLOWING"
%token <isUpper> FOR_ "FOR"
%token <isUpper> FROM "from"
%token <isUpper> FULLTEXT "full-text"
%token <isUpper> GRANT "grant"
%token <isUpper> IN_ "IN"
%token <isUpper> INDEX "index"
%token <isUpper> INTO "into"
%token <isUpper> INSERT "insert"
%token <isUpper> LOAD "load"
%token <isUpper> METADATA "metadata"
%token <isUpper> MODULE_ "MODULE"
%token <isUpper> MOVE "move"
%token <isUpper> NODE_ "NODE"
%token <isUpper> ON "on"
%token <isUpper> OR_ "OR"
%token <isUpper> PASSWORD "password"
%token <isUpper> PRECEDING_ "PRECEDING"
%token <isUpper> PROFILE "profile"
%token <isUpper> PUBLIC "public"
%token <isUpper> RENAME "rename"
%token <isUpper> REPLACE "replace"
%token <isUpper> RETRIEVE "retrieve"
%token <isUpper> REVOKE "revoke"
%token <isUpper> ROLE "role"
%token <isUpper> SCHEMA_ "SCHEMA"
%token <isUpper> STATEMENT "statement"
%token <isUpper> STATISTICS "statistics"
%token <isUpper> STDIN "stdin"
%token <isUpper> TO_ "TO"
%token <isUpper> TRIGGER "trigger"
%token <isUpper> TYPE "type"
%token <isUpper> OPTIONS "options"
%token <isUpper> UPDATE "update"
%token <isUpper> USER "user"
%token <isUpper> USING "using"
%token <isUpper> WITH "with"

%token <littext> IntegerLiteral "Integer Literal"
%token <littext> DecimalLiteral "Decimal Literal"
%token <littext> DoubleLiteral "Double Literal"
%token <littext> StringLiteral "String Literal"

%token <qname> PREF_WCARD "NCName:*"
%token <qname> WCARD_LOC "*:NCName"
%token <qname> COMP_CONST_QNAME "QName (computed constructor)"
%token <qname> QNAME "QName"

%token LBRACE "{"
%token RBRACE "}"
%token XML_COMM_BEG "<!--"
%token PI_START "<?"
%token PRAGMA_BEG "(#"
%token CDATA_BEG "<[CDATA["

%token S "space character"
%token APOS "'"
%token QUOT "\""
%token LT_OR_ST "<"
%token EMPTY_TAG "/>"
%token START_TAG_END "> (start tag end)"
%token END_TAG_OPEN "</"
%token END_TAG_CLOSE "> (end tag end)"

%token <scont> PREF "predefined entity"
%token <scont> CREF "char reference"
%token DOUB_LBRACE "{{"
%token DOUB_RBRACE "}}"
%token ESCAPE_APOS "''"
%token ESCAPE_QUOT "\"\""
%token <scont> CHAR_CONT "constructor content'"

%token <scont> XML_CONT_WITH_END "-->"
%token <qname> PI_TARGET "PI target"
%token <scont> PI_CONT_WITH_END "?>"
%token <scont> PRAGMA_CONT_WITH_END "#)"
%token <scont> CDATA_CONT_WITH_END "]]>"

%type <node> module
%type <node> mainModule
%type <node> libraryModule
%type <node> versionDecl
%type <node> moduleDecl
%type <node> prolog
%type <node> setter
%type <node> import
%type <node> namespaceDecl
%type <node> boundarySpaceDecl
%type <node> defaultNamespaceDecl
%type <node> optionDecl
%type <node> orderingModeDecl
%type <node> emptyOrderDecl
%type <node> copyNamespacesDecl
%type <node> defaultCollationDecl
%type <node> baseURIDecl
%type <node> schemaImport
%type <node> moduleImport
%type <str_list> uriLiteralList
%type <node> varDecl
%type <node> constructionDecl
%type <node> functionDecl
%type <node_list> paramList
%type <node> param
%type <node> enclosedExpr
%type <node> queryBody
%type <node> expr
%type <node> exprSingle
%type <node> flworExpr
%type <node_list> flClauses
%type <node_list> forClause
%type <node_list> letClause
%type <node> forClauseInt
%type <node> letClauseInt
%type <node> whereClause
%type <node> positionalVar
%type <node> orderByClause
%type <node_list> orderSpecList
%type <node> orderSpec
%type <node> orderModifier
%type <node> orderADmod
%type <node> orderEGLmod
%type <node> orderCOLmod
%type <node> quantifiedExpr
%type <node_list> quantifiedExprVarList
%type <node> typeswitchExpr
%type <node_list> caseClauseList
%type <node> caseClause
%type <node> ifExpr
%type <node> orExpr
%type <node> andExpr
%type <node> comparisonExpr
%type <node> rangeExpr
%type <node> additiveExpr
%type <node> multiplicativeExpr
%type <node> unionExpr
%type <node> intersectExceptExpr
%type <node> instanceofExpr
%type <node> treatExpr
%type <node> castableExpr
%type <node> castExpr
%type <node> unaryExpr
%type <node> valueExpr
%type <node> generalComp
%type <node> valueComp
%type <node> nodeComp
%type <node> validateExpr
%type <node> extensionExpr
%type <node_list> pragmas
%type <node> pragma
%type <node> pathExpr
%type <node> relativePathExpr
%type <node> stepExpr
%type <node> axisStep
%type <node> forwardStep
%type <node> forwardAxis
%type <node> abbrevForwardStep
%type <node> reverseStep
%type <node> reverseAxis
%type <node> abbrevReverseStep
%type <node> nodeTest
%type <node> nameTest
%type <qname> wildcard
%type <node> filterExpr
%type <node> predicate
%type <node_list> predicateList
%type <node> primaryExpr
%type <node> literal
%type <node> numericLiteral
%type <node> varRef
%type <qname> varName
%type <node> parenthesizedExpr
%type <node> contextItemExpr
%type <node> orderedExpr
%type <node> unorderedExpr
%type <node> functionCall
%type <node_list> funcParams
%type <node> constructor
%type <node> directConstructor
%type <node> dirElemConstructor
%type <node_list> dirAttributeList
%type <node> dirAttribute
%type <node_list> dirAttributeValue
%type <node_list> attributeValue
%type <node_list> dirElementContentList
%type <node> dirElemContent
%type <node> commonContent
%type <node> dirCommentConstructor
%type <node> dirPIConstructor
%type <scont> cdataSection
%type <node> computedConstructor
%type <node> compDocConstructor
%type <node> compElemConstructor
%type <node> contentExpr
%type <node> compAttrConstructor
%type <node> compTextConstructor
%type <node> compCommentConstructor
%type <node> compPIConstructor
%type <node> singleType
%type <node> typeDeclaration
%type <node> sequenceType
%type <node> itemType
%type <qname> atomicType
%type <node> kindTest
%type <node> documentTest
%type <node> elementTest
%type <node> schemaElementTest
%type <node> schemaAttributeTest
%type <node> piTest
%type <node> attributeTest
%type <node> commentTest
%type <node> textTest
%type <node> anyKindTest
%type <qname> attribNameOrWildcard
%type <qname> attributeDeclaration
%type <qname> elementNameOrWildcard
%type <qname> elementDeclaration
%type <qname> attributeName
%type <qname> elementName
%type <node> typeName
%type <littext> uriLiteral
%type <qname> ncName
%type <qname> qName
%type <qname> funcName
%type <node> createExpr
%type <x> baTrigMod
%type <x> idrTrigMod
%type <x> nsTrigMod
%type <scont> privName
%type <scont> userName
%type <node_list> triggerDoStmts
%type <node> triggerDoStmt
%type <node> metadataExpr
%type <node> updateExpr
%type <node> insertExpr
%type <node> deleteExpr
%type <node> deleteUndeepExpr
%type <node> replaceExpr
%type <node> renameExpr
%type <node> moveExpr
%type <str_list> moduleList

// token destructor
%destructor { delete $$; } IntegerLiteral DecimalLiteral DoubleLiteral StringLiteral PREF_WCARD WCARD_LOC QNAME COMP_CONST_QNAME PREF CREF CHAR_CONT XML_CONT_WITH_END PI_TARGET PI_CONT_WITH_END PRAGMA_CONT_WITH_END CDATA_CONT_WITH_END

// AST destructor
%destructor { delete $$; } module mainModule libraryModule versionDecl moduleDecl prolog setter import namespaceDecl boundarySpaceDecl defaultNamespaceDecl optionDecl orderingModeDecl emptyOrderDecl copyNamespacesDecl defaultCollationDecl baseURIDecl schemaImport moduleImport varDecl constructionDecl functionDecl param enclosedExpr queryBody expr exprSingle flworExpr forClauseInt letClauseInt whereClause positionalVar orderByClause orderSpec orderADmod orderEGLmod orderCOLmod quantifiedExpr typeswitchExpr caseClause ifExpr orExpr andExpr comparisonExpr rangeExpr additiveExpr multiplicativeExpr unionExpr intersectExceptExpr instanceofExpr treatExpr castableExpr castExpr unaryExpr valueExpr generalComp valueComp nodeComp validateExpr extensionExpr pragma pathExpr relativePathExpr stepExpr axisStep forwardStep forwardAxis abbrevForwardStep reverseStep reverseAxis abbrevReverseStep nodeTest nameTest wildcard filterExpr predicate primaryExpr literal numericLiteral varRef varName parenthesizedExpr orderedExpr unorderedExpr functionCall constructor directConstructor dirElemConstructor dirAttribute dirElemContent commonContent dirCommentConstructor dirPIConstructor cdataSection computedConstructor compDocConstructor compElemConstructor contentExpr compAttrConstructor compTextConstructor compCommentConstructor compPIConstructor singleType typeDeclaration sequenceType itemType atomicType kindTest documentTest elementTest schemaElementTest schemaAttributeTest piTest attributeTest commentTest textTest anyKindTest attribNameOrWildcard attributeDeclaration elementNameOrWildcard elementDeclaration attributeName elementName typeName uriLiteral ncName qName funcName createExpr userName privName triggerDoStmt metadataExpr updateExpr insertExpr deleteExpr deleteUndeepExpr replaceExpr renameExpr moveExpr

// Some internal static declarations
%{
    #include "tr/xqp/ast/AST.h"
    #include "tr/xqp/XQueryParser.hpp"
    #include "common/errdbg/exceptions.h"

    using namespace sedna;

    static bool isPreserveBoundSpace; // boundary space policy for direct constructors
    static bool isSecondPrologPart;   // true, if declaration from second prolog part have been met

    static void ProcessDirectContent(ASTNodesVector *cont, bool isPreserveBS);

    static void errorc(sedna::XQueryDriver &d, const sedna::XQueryParser::location_type& l, int code);

    static ASTNode *makeQuantExpr(sedna::XQueryParser::location_type& loc,
                                    ASTQuantExpr::QuantMod qt,
                                    ASTNodesVector *var_expr,
                                    ASTNode *sat_expr);

    //static ASTNode *makePathExpr(sedna::XQueryParser::location_type& loc, ASTNodesVector *steps);
    static ASTNode *makePathExpr(ASTNode *xpath, ASTNode *context);

    // make parser call our scan-function
    #undef yylex
    #define yylex driver.lexer->nextToken
%}

%destructor { destroyASTNodesVector($$); } paramList flClauses forClause letClause orderSpecList quantifiedExprVarList caseClauseList pragmas predicateList funcParams dirAttributeList dirAttributeValue attributeValue dirElementContentList triggerDoStmts

%destructor { destroyASTStringVector($$); } uriLiteralList moduleList

    /* we expect 56 conflicts in this grammar:
            1) 1 for Sedna explain feature (default: 'shift' always treats first 'explain' as feature-keyword, thus making impossible such
                queries as 'explain or xxx'). See also 4) constraint on 'load'.
            2) 1 for Sedna profile feature (default: 'shift' always treats first 'profile' as feature-keyword, thus making impossible such
                queries as 'profile or xxx'). See also 4) constraint on 'load'.
            3) 51 for 'xgs:leading-lone-slash' grammar constraint (default: 'shift' is consistent with specification)
            4) 1 for 'load or replace module' Sedna expression since 'load or xxx' is a valid expression
                    (default: 'shift' disables expressions such as 'load or xxx'; possible fix: reject 'or' and accept only 'OR')
            5) 2 for 'xgs:occurrence-indicators' grammar constraint (default: 'shift' is consistent with specification)
    */
%expect 56


    /* set initial location and boundary space policy */
%initial-action
{
    // Initialize the initial location.
    @$.begin.filename = @$.end.filename = NULL;

    // start from first column
    @$.begin.column = @$.end.column = 1;

    // we strip boundary spaces by default
    isPreserveBoundSpace = false;

    // in the start we've got no prolog
    isSecondPrologPart = false;
};

%%

%start script;

script:
        module
        {
            driver.addModule($1, 0);
        }
    |   script ST_SEP module
        {
            driver.addModule($3, 0);
        }
    |   EXPLAIN module
        {
            driver.addModule($2, 1);
        }
    |   PROFILE module
        {
            driver.addModule($2, 2);
        }
    ;

    /* [1]      Module     ::=      VersionDecl? (LibraryModule | MainModule)*/
module:
        versionDecl mainModule
        {
            static_cast<ASTMainModule *>($2)->setVersionDecl($1);

            $$ = $2;
        }
    |   versionDecl libraryModule
        {
            static_cast<ASTLibModule *>($2)->setVersionDecl($1);

            $$ = $2;
        }
    |   mainModule
        {
            $$ = $1;
        }
    |   libraryModule
        {
            $$ = $1;
        }
    ;

    /* [2]     VersionDecl        ::=      "xquery" "version" StringLiteral ("encoding" StringLiteral)? Separator */
versionDecl:
        XQUERY VERSION StringLiteral SEMI
        {
            $$ = new ASTVersionDecl(@$, $3, NULL);
        }
    |   XQUERY VERSION StringLiteral ENCODING StringLiteral SEMI
        {
            $$ = new ASTVersionDecl(@$, $3, $5);
        }
    |   XQUERY error SEMI
        {
            $$ = new ASTVersionDecl(@$, new std::string(""), new std::string(""));
        }
    ;

    /* [3]     MainModule     ::=      Prolog QueryBody */
mainModule:
        prolog queryBody
        {
            $$ = new ASTMainModule(@$, static_cast<ASTProlog *>($1), static_cast<ASTQuery *>($2));
        }
    ;

    /* [4]     LibraryModule      ::=      ModuleDecl Prolog */
libraryModule:
        moduleDecl prolog
        {
            $$ = new ASTLibModule(@$, static_cast<ASTModuleDecl *>($1), static_cast<ASTProlog *>($2));
        }
    ;

    /* [5]      ModuleDecl     ::=      "module" "namespace" NCName "=" URILiteral Separator */
moduleDecl:
        MODULE NAMESPACE ncName EQ_SIGN uriLiteral SEMI
        {
            $$ = new ASTModuleDecl(@$, $3, $5);
        }
    |   MODULE error SEMI
        {
            $$ = new ASTModuleDecl(@$, new std::string(""), new std::string(""));
        }
    ;

    /* [6]    Prolog     ::=      ((DefaultNamespaceDecl | Setter | NamespaceDecl | Import) Separator)* ((VarDecl | FunctionDecl | OptionDecl) Separator)* */
prolog:
        /* empty */
        {
            $$ = new ASTProlog(@$);
        }
    |   prolog defaultNamespaceDecl SEMI
        {
            if (isSecondPrologPart)
            {
                error(@2, "default namespace declaration cannot be in the second prolog part");
                delete $1;
                delete $2;

                YYABORT;
            }

            static_cast<ASTProlog *>($1)->addPrologDecl($2);
            $$ = $1;
        }
    |   prolog setter SEMI
        {
            if (isSecondPrologPart)
            {
                error(@2, "setter declaration cannot be in the second prolog part");
                delete $1;
                delete $2;

                YYABORT;
            }

            static_cast<ASTProlog *>($1)->addPrologDecl($2);
            $$ = $1;
        }
    |   prolog namespaceDecl SEMI
        {
            if (isSecondPrologPart)
            {
                error(@2, "namespace declaration cannot be in the second prolog part");
                delete $1;
                delete $2;

                YYABORT;
            }

            static_cast<ASTProlog *>($1)->addPrologDecl($2);
            $$ = $1;
        }
    |   prolog import SEMI
        {
            if (isSecondPrologPart)
            {
                error(@2, "import declaration cannot be in the second prolog part");
                delete $1;
                delete $2;

                YYABORT;
            }

            static_cast<ASTProlog *>($1)->addPrologDecl($2);
            $$ = $1;
        }

    |   prolog varDecl SEMI
        {
            isSecondPrologPart = true;
            static_cast<ASTProlog *>($1)->addPrologDecl($2);
            $$ = $1;
        }
    |   prolog functionDecl SEMI
        {
            isSecondPrologPart = true;
            static_cast<ASTProlog *>($1)->addPrologDecl($2);
            $$ = $1;
        }
    |   prolog optionDecl SEMI
        {
            isSecondPrologPart = true;
            static_cast<ASTProlog *>($1)->addPrologDecl($2);
            $$ = $1;
        }
    |   prolog error SEMI { $$ = $1; }
    |   prolog DECLARE error SEMI { $$ = $1; }
    |   prolog IMPORT error SEMI { $$ = $1; }
    ;

    /* [7]     Setter     ::=      BoundarySpaceDecl | DefaultCollationDecl | BaseURIDecl | ConstructionDecl | OrderingModeDecl | EmptyOrderDecl | CopyNamespacesDecl */
setter:
        boundarySpaceDecl
    |   defaultCollationDecl
    |   baseURIDecl
    |   constructionDecl
    |   orderingModeDecl
    |   emptyOrderDecl
    |   copyNamespacesDecl
    ;

    /* [8]     Import     ::=      SchemaImport | ModuleImport */
import:
        schemaImport
    |   moduleImport
    ;

    /* [10]        NamespaceDecl      ::=      "declare" "namespace" NCName "=" URILiteral */
namespaceDecl:
        DECLARE NAMESPACE ncName EQ_SIGN uriLiteral
        {
            $$ = new ASTNamespaceDecl(@$, $3, $5);
        }
    ;

    /* [11]        BoundarySpaceDecl      ::=      "declare" "boundary-space" ("preserve" | "strip") */
boundarySpaceDecl:
        DECLARE BOUNDARY_SPACE PRESERVE
        {
            isPreserveBoundSpace = true; // parser parameter concerning element content
            $$ = new ASTBoundSpaceDecl(@$, ASTBoundSpaceDecl::PRESERVE);
        }
    |   DECLARE BOUNDARY_SPACE STRIP
        {
            isPreserveBoundSpace = false; // parser parameter concerning element content
            $$ = new ASTBoundSpaceDecl(@$, ASTBoundSpaceDecl::STRIP);
        }
    ;

    /* [12]        DefaultNamespaceDecl       ::=      "declare" "default" ("element" | "function") "namespace" URILiteral */
defaultNamespaceDecl:
        DECLARE DEFAULT ELEMENT NAMESPACE uriLiteral
        {
            $$ = new ASTDefNamespaceDecl(@$, $5, ASTDefNamespaceDecl::ELEMENT);
        }

    |   DECLARE DEFAULT FUNCTION NAMESPACE uriLiteral
        {
            $$ = new ASTDefNamespaceDecl(@$, $5, ASTDefNamespaceDecl::FUNCTION);
        }
    ;

    /* [13]        OptionDecl     ::=      "declare" "option" QName StringLiteral */
optionDecl:
        DECLARE OPTION qName StringLiteral
        {
            $$ = new ASTOption(@$, $3, $4);
        }
    ;

    /* [14]        OrderingModeDecl       ::=      "declare" "ordering" ("ordered" | "unordered") */
orderingModeDecl:
        DECLARE ORDERING ORDERED
        {
            $$ = new ASTOrder(@$, ASTOrder::ORDERED);
        }
    |   DECLARE ORDERING UNORDERED
        {
            $$ = new ASTOrder(@$, ASTOrder::UNORDERED);
        }
    ;

    /* [15]        EmptyOrderDecl     ::=      "declare" "default" "order" "empty" ("greatest" | "least") */
emptyOrderDecl:
        DECLARE DEFAULT ORDER EMPTY GREATEST
        {
            $$ = new ASTOrderEmpty(@$, ASTOrderEmpty::EMPTY_GREATEST);
        }
    |   DECLARE DEFAULT ORDER EMPTY LEAST
        {
            $$ = new ASTOrderEmpty(@$, ASTOrderEmpty::EMPTY_LEAST);
        }
    ;

    /* [16]        CopyNamespacesDecl     ::=      "declare" "copy-namespaces" PreserveMode "," InheritMode         */
copyNamespacesDecl:
        DECLARE COPY_NAMESPACES PRESERVE COMMA INHERIT
        {
            $$ = new ASTDeclareCopyNsp(@$, ASTDeclareCopyNsp::PRESERVE, ASTDeclareCopyNsp::INHERIT);
        }
    |   DECLARE COPY_NAMESPACES PRESERVE COMMA NO_INHERIT
        {
            $$ = new ASTDeclareCopyNsp(@$, ASTDeclareCopyNsp::PRESERVE, ASTDeclareCopyNsp::NO_INHERIT);
        }
    |   DECLARE COPY_NAMESPACES NO_PRESERVE COMMA INHERIT
        {
            $$ = new ASTDeclareCopyNsp(@$, ASTDeclareCopyNsp::NO_PRESERVE, ASTDeclareCopyNsp::INHERIT);
        }
    |   DECLARE COPY_NAMESPACES NO_PRESERVE COMMA NO_INHERIT
        {
            $$ = new ASTDeclareCopyNsp(@$, ASTDeclareCopyNsp::NO_PRESERVE, ASTDeclareCopyNsp::NO_INHERIT);
        }
    ;

    /* [19]        DefaultCollationDecl       ::=      "declare" "default" "collation" URILiteral */
defaultCollationDecl:
        DECLARE DEFAULT COLLATION uriLiteral
        {
            $$ = new ASTDefCollation(@$, $4);
        }
    ;

    /* [20]        BaseURIDecl        ::=      "declare" "base-uri" URILiteral */
baseURIDecl:
        DECLARE BASE_URI uriLiteral
        {
            $$ = new ASTBaseURI(@$, $3);
        }
    ;

    /* [21]        SchemaImport       ::=      "import" "schema" SchemaPrefix? URILiteral ("at" URILiteral ("," URILiteral)*)? */
    /* NOTE: we use delete explicitly here since bison doesn't destroy right-hand symbols for YYABORT rules */
schemaImport:
        IMPORT SCHEMA schemaPrefix uriLiteral
        {
            delete $4;
            errorc(this->driver, @$, XQST0009);
            YYABORT;

            $$ = NULL;
        }
    |   IMPORT SCHEMA uriLiteral
        {
            delete $3;
            errorc(this->driver, @$, XQST0009);
            YYABORT;

            $$ = NULL;
        }
    |   IMPORT SCHEMA schemaPrefix uriLiteral AT uriLiteralList
        {
            delete $4;
            destroyASTStringVector($6);
            errorc(this->driver, @$, XQST0009);
            YYABORT;

            $$ = NULL;
        }
    |   IMPORT SCHEMA uriLiteral AT uriLiteralList
        {
            delete $3;
            destroyASTStringVector($5);
            errorc(this->driver, @$, XQST0009);
            YYABORT;

            $$ = NULL;
        }
    ;

    /* [22]        SchemaPrefix       ::=      ("namespace" NCName "=") | ("default" "element" "namespace") */
schemaPrefix:
        NAMESPACE ncName EQ_SIGN
        {
            delete $2;
        }
    |   DEFAULT ELEMENT NAMESPACE
    ;

/* [23]        ModuleImport       ::=      "import" "module" ("namespace" NCName "=")? URILiteral ("at" URILiteral ("," URILiteral)*)? */
moduleImport:
        IMPORT MODULE NAMESPACE ncName EQ_SIGN uriLiteral
        {
            $$ = new ASTModImport(@$, $4, $6, NULL);
        }
    |   IMPORT MODULE uriLiteral
        {
            $$ = new ASTModImport(@$, NULL, $3, NULL);
        }
    |   IMPORT MODULE NAMESPACE ncName EQ_SIGN uriLiteral AT uriLiteralList
        {
            $$ = new ASTModImport(@$, $4, $6, $8);
        }
    |   IMPORT MODULE uriLiteral AT uriLiteralList
        {
            $$ = new ASTModImport(@$, NULL, $3, $5);
        }
    ;

uriLiteralList:
        uriLiteral
        {
            $$ = new ASTStringVector();
            $$->push_back($1);
        }
    |   uriLiteralList COMMA uriLiteral
        {
            static_cast<ASTStringVector *>($1)->push_back($3);

            $$ = $1;
        }
    |   error { $$ = new ASTStringVector(); }
    |   uriLiteralList COMMA error { $$ = $1; }
    ;

    /* [24]        VarDecl        ::=      "declare" "variable" "$" QName TypeDeclaration? ((":=" ExprSingle) | "external") */
varDecl:
        DECLARE VARIABLE varRef typeDeclaration ASSIGN exprSingle
        {
            $$ = new ASTVarDecl(@$, static_cast<ASTVar *>($3), static_cast<ASTTypeSeq *>($4), $6);
        }
    |   DECLARE VARIABLE varRef typeDeclaration EXTERNAL
        {
            $$ = new ASTVarDecl(@$, static_cast<ASTVar *>($3), static_cast<ASTTypeSeq *>($4));
        }
    |   DECLARE VARIABLE varRef ASSIGN exprSingle
        {
            $$ = new ASTVarDecl(@$, static_cast<ASTVar *>($3), NULL, $5);
        }
    |   DECLARE VARIABLE varRef EXTERNAL
        {
            $$ = new ASTVarDecl(@$, static_cast<ASTVar *>($3));
        }
    ;

    /* [25]        ConstructionDecl       ::=      "declare" "construction" ("strip" | "preserve") */
constructionDecl:
        DECLARE CONSTRUCTION STRIP
        {
            $$ = new ASTConstDecl(@$, ASTConstDecl::STRIP);
        }
    |   DECLARE CONSTRUCTION PRESERVE
        {
            $$ = new ASTConstDecl(@$, ASTConstDecl::PRESERVE);
        }
    ;

    /* [26]        FunctionDecl       ::=      "declare" "function" QName "(" ParamList? ")" ("as" SequenceType)? (EnclosedExpr | "external") */
functionDecl:
        DECLARE FUNCTION qName LPAREN paramList RPAREN AS sequenceType enclosedExpr
        {
            $$ = new ASTFuncDecl(@$, $3, $5, static_cast<ASTTypeSeq *>($8), $9);
        }
    |   DECLARE FUNCTION qName LPAREN paramList RPAREN AS sequenceType EXTERNAL
        {
            $$ = new ASTFuncDecl(@$, $3, $5, static_cast<ASTTypeSeq *>($8));
        }
    |   DECLARE FUNCTION qName LPAREN RPAREN AS sequenceType enclosedExpr
        {
            $$ = new ASTFuncDecl(@$, $3, NULL, static_cast<ASTTypeSeq *>($7), $8);
        }
    |   DECLARE FUNCTION qName LPAREN RPAREN AS sequenceType EXTERNAL
        {
            $$ = new ASTFuncDecl(@$, $3, NULL, static_cast<ASTTypeSeq *>($7));
        }
    |   DECLARE FUNCTION qName LPAREN paramList RPAREN enclosedExpr
        {
            $$ = new ASTFuncDecl(@$, $3, $5, new ASTTypeSeq(@6, new ASTItemTest(@6), ASTTypeSeq::ZERO_OR_MORE), $7);
        }
    |   DECLARE FUNCTION qName LPAREN paramList RPAREN EXTERNAL
        {
            $$ = new ASTFuncDecl(@$, $3, $5, new ASTTypeSeq(@6, new ASTItemTest(@6), ASTTypeSeq::ZERO_OR_MORE));
        }
    |   DECLARE FUNCTION qName LPAREN RPAREN enclosedExpr
        {
            $$ = new ASTFuncDecl(@$, $3, NULL, new ASTTypeSeq(@6, new ASTItemTest(@6), ASTTypeSeq::ZERO_OR_MORE), $6);
        }
    |   DECLARE FUNCTION qName LPAREN RPAREN EXTERNAL
        {
            $$ = new ASTFuncDecl(@$, $3, NULL, new ASTTypeSeq(@6, new ASTItemTest(@6), ASTTypeSeq::ZERO_OR_MORE));
        }
    ;

    /* [27]        ParamList      ::=      Param ("," Param)* */
paramList:
        param
        {
            $$ = new ASTNodesVector();
            $$->push_back($1);
        }
    |   paramList COMMA param
        {
            static_cast<ASTNodesVector *>($1)->push_back($3);

            $$ = $1;
        }
    |   paramList COMMA error { $$ = $1; }
    |   error { $$ = new ASTNodesVector(); }
    ;

    /* [28]        Param      ::=      "$" QName TypeDeclaration? */
param:
        varRef typeDeclaration
        {
            $$ = new ASTTypeVar(@$, static_cast<ASTTypeSeq *>($2), static_cast<ASTVar *>($1));
        }
    |   varRef /* implicit item()* as type */
        {
            $$ = new ASTTypeVar(@$, new ASTTypeSeq(@1, new ASTItemTest(@1), ASTTypeSeq::ZERO_OR_MORE), static_cast<ASTVar *>($1));
        }
    ;

    /* [29]        EnclosedExpr       ::=      "{" Expr "}" */
enclosedExpr:
        LBRACE expr RBRACE
        {
            $$ = $2;
        }
    |   LBRACE error RBRACE { $$ = new ASTError(@$); }
    ;

    /* [30]        QueryBody      ::=      Expr */
    /* Sedna Update extensions added here */
queryBody:
        expr
        {
            $$ = new ASTQuery(@$, $1, ASTQuery::QUERY);
        }
    |   createExpr
        {
            $$ = new ASTQuery(@$, $1, ASTQuery::CREATE);
        }
    |   updateExpr
        {
            $$ = new ASTQuery(@$, $1, ASTQuery::UPDATE);
        }
    |   metadataExpr
        {
            $$ = new ASTQuery(@$, $1, ASTQuery::META);
        }
    ;

    /* [31]        Expr       ::=      ExprSingle ("," ExprSingle)* */
expr:
        exprSingle
        {
            $$ = $1;
        }
    |   expr COMMA exprSingle
        {
            ASTSeq *seq;

            if ((seq = dynamic_cast<ASTSeq *>($1)))
                seq->addExpr($3);
            else
            {
                seq = new ASTSeq(@1);
                seq->addExpr($1);
                seq->addExpr($3);
            }

            $$ = seq;
        }
    |   expr COMMA error { $$ = $1; }
    ;

    /* [32]        ExprSingle     ::=     FLWORExpr
                                        | QuantifiedExpr
                                        | TypeswitchExpr
                                        | IfExpr
                                        | OrExpr
    */
exprSingle:
        flworExpr
    |   quantifiedExpr
    |   typeswitchExpr
    |   ifExpr
    |   orExpr
    ;

    /* [33]        FLWORExpr      ::=      (ForClause | LetClause)+ WhereClause? OrderByClause? "return" ExprSingle */
flworExpr:
        flClauses whereClause orderByClause RETURN exprSingle
        {
            $$ = new ASTFLWOR(@$, $1, $2, $3, $5);
        }
    |   flClauses whereClause RETURN exprSingle
        {
            $$ = new ASTFLWOR(@$, $1, $2, NULL, $4);
        }
    |   flClauses orderByClause RETURN exprSingle
        {
            $$ = new ASTFLWOR(@$, $1, NULL, $2, $4);
        }
    |   flClauses RETURN exprSingle
        {
            $$ = new ASTFLWOR(@$, $1, NULL, NULL, $3);
        }
    |   flClauses error RETURN exprSingle
        {
            destroyASTNodesVector($1);
            delete $4;

            $$ = new ASTError(@$);
        }
    ;

    /* (ForClause | LetClause)+ */
flClauses:
        forClause
        {
            $$ = $1;
        }
    |   letClause
        {
            $$ = $1;
        }
    |   flClauses forClause
        {
            ASTNodesVector *fl_list = $1, *for_list = $2;

            fl_list->insert(fl_list->end(), for_list->begin(), for_list->end());

            delete for_list;

            $$ = $1;
        }
    |   flClauses letClause
        {
            ASTNodesVector *fl_list = $1, *let_list = $2;

            fl_list->insert(fl_list->end(), let_list->begin(), let_list->end());

            delete let_list;

            $$ = $1;
        }
    ;

    /* [34]        ForClause      ::=      "for" "$" VarName TypeDeclaration? PositionalVar? "in" ExprSingle ("," "$" VarName TypeDeclaration? PositionalVar? "in" ExprSingle)* */
forClause:
        FOR forClauseInt
        {
            $$ = new ASTNodesVector();
            $$->push_back($2);
        }
    |   forClause COMMA forClauseInt
        {
            $1->push_back($3);
            $$ = $1;
        }
    |   FOR DOLLAR error { $$ = $$ = new ASTNodesVector(); }
    |   forClause COMMA error { $$ = $1; }
    ;

    /* "$" VarName TypeDeclaration? PositionalVar? "in" ExprSingle */
forClauseInt:
        varRef typeDeclaration positionalVar _IN exprSingle
        {
            ASTTypeVar *tv = new ASTTypeVar(@1, static_cast<ASTTypeSeq *>($2), static_cast<ASTVar *>($1));
            $$ = new ASTFor(@$, tv, static_cast<ASTPosVar *>($3), $5);
        }
    |   varRef typeDeclaration _IN exprSingle
        {
            ASTTypeVar *tv = new ASTTypeVar(@1, static_cast<ASTTypeSeq *>($2), static_cast<ASTVar *>($1));
            $$ = new ASTFor(@$, tv, NULL, $4);
        }
    |   varRef positionalVar _IN exprSingle
        {
            ASTTypeVar *tv = new ASTTypeVar(@1, new ASTTypeSeq(@1, new ASTType(@1, new std::string("!xs:anyType"), ASTType::ABSTRACT), ASTTypeSeq::ONE), static_cast<ASTVar *>($1));
            $$ = new ASTFor(@$, tv, static_cast<ASTPosVar *>($2), $4);
        }
    |   varRef _IN exprSingle
        {
            ASTTypeVar *tv = new ASTTypeVar(@1, new ASTTypeSeq(@1, new ASTType(@1, new std::string("!xs:anyType"), ASTType::ABSTRACT), ASTTypeSeq::ONE), static_cast<ASTVar *>($1));
            $$ = new ASTFor(@$, tv, NULL, $3);
        }
    |   DOLLAR error _IN error { $$ = new ASTError(@$); }
    |   DOLLAR error _IN exprSingle { delete $4; $$ = new ASTError(@$); }
    ;

    /* [35]        PositionalVar      ::=      "at" "$" VarName */
positionalVar:
        AT varRef
        {
            $$ = new ASTPosVar(@$, static_cast<ASTVar *>($2));
        }
    |   AT error { $$ = new ASTError(@$); }
    ;

    /* [36]        LetClause      ::=      "let" "$" VarName TypeDeclaration? ":=" ExprSingle ("," "$" VarName TypeDeclaration? ":=" ExprSingle)* */
letClause:
        LET letClauseInt
        {
            $$ = new ASTNodesVector();
            $$->push_back($2);
        }
    |   letClause COMMA letClauseInt
        {
            $1->push_back($3);
            $$ = $1;
        }
    |   LET DOLLAR error { $$ = $$ = new ASTNodesVector(); }
    |   letClause COMMA error { $$ = $1; }
    ;

    /* "$" VarName TypeDeclaration? ":=" ExprSingle */
letClauseInt:
        varRef typeDeclaration ASSIGN exprSingle
        {
            ASTTypeVar *tv = new ASTTypeVar(@1, static_cast<ASTTypeSeq *>($2), static_cast<ASTVar *>($1));
            $$ = new ASTLet(@$, tv, $4);
        }
    |   varRef ASSIGN exprSingle
        {
            ASTTypeVar *tv = new ASTTypeVar(@1, new ASTTypeSeq(@1, new ASTType(@1, new std::string("!xs:anyType"), ASTType::ABSTRACT), ASTTypeSeq::ZERO_OR_MORE), static_cast<ASTVar *>($1));
            $$ = new ASTLet(@$, tv, $3);
        }
    |   DOLLAR error ASSIGN error { $$ = new ASTError(@$); }
    |   DOLLAR error ASSIGN exprSingle { delete $4; $$ = new ASTError(@$); }
    ;

    /* [37]        WhereClause        ::=      "where" ExprSingle */
whereClause:
        WHERE exprSingle
        {
            $$ = $2;
        }
    |   WHERE error { $$ = new ASTError(@$); }
    ;

    /* [38]        OrderByClause      ::=      (("order" "by") | ("stable" "order" "by")) OrderSpecList */
orderByClause:
        ORDER BY orderSpecList
        {
            $$ = new ASTOrderBy(@$, ASTOrderBy::UNSTABLE, $3);
        }
    |   STABLE ORDER BY orderSpecList
        {
            $$ = new ASTOrderBy(@$, ASTOrderBy::STABLE, $4);
        }
    |   ORDER BY error { $$ = new ASTError(@$); }
    |   STABLE ORDER BY error { $$ = new ASTError(@$); }
    ;

    /* [39]        OrderSpecList      ::=      OrderSpec ("," OrderSpec)* */
orderSpecList:
        orderSpec
        {
            $$ = new ASTNodesVector();
            $$->push_back($1);
        }
    |   orderSpecList COMMA orderSpec
        {
            $1->push_back($3);
            $$ = $1;
        }
    |   orderSpecList COMMA error { $$ = $1; }
    ;

    /* [40]        OrderSpec      ::=      ExprSingle OrderModifier */
orderSpec:
        exprSingle orderModifier
        {
            $$ = new ASTOrderSpec(@$, $1, static_cast<ASTOrderMod *>($2));
        }
    |   exprSingle
        {
            $$ = new ASTOrderSpec(@$, $1, NULL);
        }
    ;

    /* [41]        OrderModifier      ::=      ("ascending" | "descending")? ("empty" ("greatest" | "least"))? ("collation" URILiteral)? */
orderModifier:
        orderADmod
        {
            $$ = new ASTOrderMod(@$, static_cast<ASTOrderModInt *>($1), NULL, NULL);
        }
    |   orderEGLmod
        {
            $$ = new ASTOrderMod(@$, NULL, static_cast<ASTOrderModInt *>($1), NULL);
        }
    |   orderCOLmod
        {
            $$ = new ASTOrderMod(@$, NULL, NULL, static_cast<ASTOrderModInt *>($1));
        }
    |   orderADmod orderEGLmod
        {
            $$ = new ASTOrderMod(@$, static_cast<ASTOrderModInt *>($1), static_cast<ASTOrderModInt *>($2), NULL);
        }
    |   orderADmod orderCOLmod
        {
            $$ = new ASTOrderMod(@$, static_cast<ASTOrderModInt *>($1), NULL, static_cast<ASTOrderModInt *>($2));
        }
    |   orderEGLmod orderCOLmod
        {
            $$ = new ASTOrderMod(@$, NULL, static_cast<ASTOrderModInt *>($1), static_cast<ASTOrderModInt *>($2));
        }
    |   orderADmod orderEGLmod orderCOLmod
        {
            $$ = new ASTOrderMod(@$, static_cast<ASTOrderModInt *>($1), static_cast<ASTOrderModInt *>($2), static_cast<ASTOrderModInt *>($3));
        }
    ;

orderADmod:
        ASCENDING
        {
            $$ = new ASTOrderModInt(@$, ASTOrderModInt::ASCENDING);
        }
    |   DESCENDING
        {
            $$ = new ASTOrderModInt(@$, ASTOrderModInt::DESCENDING);
        }
    ;

orderEGLmod:
        EMPTY GREATEST
        {
            $$ = new ASTOrderModInt(@$, ASTOrderModInt::EMPTY_GREATEST);
        }
    |   EMPTY LEAST
        {
            $$ = new ASTOrderModInt(@$, ASTOrderModInt::EMPTY_LEAST);
        }
    ;

orderCOLmod:
        COLLATION uriLiteral
        {
            $$ = new ASTOrderModInt(@$, ASTOrderModInt::COLLATION, $2);
        }
    ;

    /* [42]        QuantifiedExpr     ::=      ("some" | "every") "$" VarName TypeDeclaration? "in" ExprSingle ("," "$" VarName TypeDeclaration? "in" ExprSingle)* "satisfies" ExprSingle */
quantifiedExpr:
        SOME quantifiedExprVarList SATISFIES exprSingle
        {
            $$ = makeQuantExpr(@$, ASTQuantExpr::SOME, $2, $4);
        }
    |   EVERY quantifiedExprVarList SATISFIES exprSingle
        {
            $$ = makeQuantExpr(@$, ASTQuantExpr::EVERY, $2, $4);
        }
    |   SOME DOLLAR error SATISFIES error { $$ = new ASTError(@$); }
    |   SOME DOLLAR error SATISFIES exprSingle { delete $5; $$ = new ASTError(@$); }
    |   EVERY DOLLAR error SATISFIES error { $$ = new ASTError(@$); }
    |   EVERY DOLLAR error SATISFIES exprSingle { delete $5; $$ = new ASTError(@$); }
    |   SOME DOLLAR quantifiedExprVarList SATISFIES error { destroyASTNodesVector($3); $$ = new ASTError(@$); }
    |   EVERY DOLLAR quantifiedExprVarList SATISFIES error { destroyASTNodesVector($3); $$ = new ASTError(@$); }
    ;

    /* "$" VarName TypeDeclaration? "in" ExprSingle ("," "$" VarName TypeDeclaration? "in" ExprSingle)* */
quantifiedExprVarList:
        varRef typeDeclaration _IN exprSingle
        {
            $$ = new ASTNodesVector();
            $$->push_back(new ASTTypeVar(@$, static_cast<ASTTypeSeq *>($2), static_cast<ASTVar *>($1)));
            $$->push_back($4);
        }
    |   varRef _IN exprSingle
        {
            $$ = new ASTNodesVector();
            $$->push_back(new ASTTypeVar(@$, new ASTTypeSeq(@1, new ASTType(@1, new std::string("!xs:anyType"), ASTType::ABSTRACT), ASTTypeSeq::ONE), static_cast<ASTVar *>($1)));
            $$->push_back($3);
        }
    |   quantifiedExprVarList COMMA varRef typeDeclaration _IN exprSingle
        {
            $1->push_back(new ASTTypeVar(@$, static_cast<ASTTypeSeq *>($4), static_cast<ASTVar *>($3)));
            $1->push_back($6);
            $$ = $1;
        }
    |   quantifiedExprVarList COMMA varRef _IN exprSingle
        {
            $1->push_back(new ASTTypeVar(@$, new ASTTypeSeq(@1, new ASTType(@1, new std::string("!xs:anyType"), ASTType::ABSTRACT), ASTTypeSeq::ONE), static_cast<ASTVar *>($3)));
            $1->push_back($5);
            $$ = $1;
        }
    |   DOLLAR error _IN error { $$ = new ASTNodesVector(); }
    |   DOLLAR error _IN exprSingle { delete $4; $$ = new ASTNodesVector(); }
    |   quantifiedExprVarList COMMA error { $$ = $1; }
    ;

    /* [43]        TypeswitchExpr     ::=      "typeswitch" "(" Expr ")" CaseClause+ "default" ("$" VarName)? "return" ExprSingle */
typeswitchExpr:
        TYPESWITCH LPAREN expr RPAREN caseClauseList DEFAULT varRef RETURN exprSingle
        {
                $$ = new ASTTypeSwitch(@$, $3, $5, new ASTCase(@6, $7, NULL, $9));
        }
    |   TYPESWITCH LPAREN expr RPAREN caseClauseList DEFAULT RETURN exprSingle
        {
                $$ = new ASTTypeSwitch(@$, $3, $5, new ASTCase(@6, NULL, NULL, $8));
        }

    |   TYPESWITCH LPAREN error RETURN error { $$ = new ASTError(@$); }
    |   TYPESWITCH LPAREN error RETURN exprSingle { delete $5; $$ = new ASTError(@$); }
    |   TYPESWITCH LPAREN expr RPAREN caseClauseList DEFAULT error RETURN error { delete $3; destroyASTNodesVector($5); $$ = new ASTError(@$); }
    |   TYPESWITCH LPAREN expr RPAREN caseClauseList DEFAULT error RETURN exprSingle { delete $3; destroyASTNodesVector($5); delete $9; $$ = new ASTError(@$); }
    |   TYPESWITCH LPAREN expr RPAREN caseClauseList DEFAULT varRef RETURN error { delete $3; destroyASTNodesVector($5); delete $7; $$ = new ASTError(@$); }
    |   TYPESWITCH LPAREN expr RPAREN caseClauseList DEFAULT RETURN error { delete $3; destroyASTNodesVector($5); $$ = new ASTError(@$); }
    ;

    /* CaseClause+ */
caseClauseList:
        caseClause
        {
            $$ = new ASTNodesVector();
            $$->push_back($1);
        }
    |   caseClauseList caseClause
        {
            $1->push_back($2);
            $$ = $1;
        }
    |   error { $$ = new ASTNodesVector(); }
    |   caseClauseList error { $$ = $1; }
    ;

    /* [44]        CaseClause     ::=      "case" ("$" VarName "as")? SequenceType "return" ExprSingle */
caseClause:
        CASE varRef AS sequenceType RETURN exprSingle
        {
            $$ = new ASTCase(@$, $2, static_cast<ASTTypeSeq *>($4), $6);
        }
    |   CASE sequenceType RETURN exprSingle
        {
            $$ = new ASTCase(@$, NULL, static_cast<ASTTypeSeq *>($2), $4);
        }
    ;

    /* [45]        IfExpr     ::=      "if" "(" Expr ")" "then" ExprSingle "else" ExprSingle */
ifExpr:
        IF LPAREN expr RPAREN THEN exprSingle ELSE exprSingle
        {
            $$ = new ASTIf(@$, $3, $6, $8);
        }

    |   IF LPAREN error RPAREN THEN error ELSE error { $$ = new ASTError(@$); }
    |   IF LPAREN error RPAREN THEN error ELSE exprSingle { delete $8; $$ = new ASTError(@$); }
    |   IF LPAREN error RPAREN THEN exprSingle ELSE error { delete $6; $$ = new ASTError(@$); }
    |   IF LPAREN error RPAREN THEN exprSingle ELSE exprSingle { delete $6; delete $8; $$ = new ASTError(@$); }
    |   IF LPAREN expr RPAREN THEN error ELSE exprSingle { delete $3; delete $8; $$ = new ASTError(@$); }
    |   IF LPAREN expr RPAREN THEN exprSingle ELSE error { delete $3; delete $6; $$ = new ASTError(@$); }
    |   IF LPAREN expr RPAREN THEN error ELSE error { delete $3; $$ = new ASTError(@$); }
    ;

    /* [46]        OrExpr     ::=      AndExpr ( "or" AndExpr )* */
orExpr:
        andExpr
        {
            $$ = $1;
        }
    |   orExpr OR andExpr
        {
            $$ = new ASTBop(@$, ASTBop::OR, $1, $3);
        }
    |   orExpr OR error { $$ = $1; }
    ;

    /* [47]        AndExpr        ::=      ComparisonExpr ( "and" ComparisonExpr )* */
andExpr:
        comparisonExpr
        {
            $$ = $1;
        }
    |   andExpr AND comparisonExpr
        {
            $$ = new ASTBop(@$, ASTBop::AND, $1, $3);
        }
    |   andExpr AND error { $$ = $1; }
    ;

    /* [48]        ComparisonExpr     ::=      RangeExpr ( (ValueComp | GeneralComp | NodeComp) RangeExpr )? */
comparisonExpr:
        rangeExpr
        {
            $$ = $1;
        }
    |   rangeExpr valueComp rangeExpr
        {
            ASTBop *bop = static_cast<ASTBop *>($2);

            bop->setLExpr($1);
            bop->setRExpr($3);

            $$ = bop;
        }
    |   rangeExpr generalComp rangeExpr
        {
            ASTBop *bop = static_cast<ASTBop *>($2);

            bop->setLExpr($1);
            bop->setRExpr($3);

            $$ = bop;
        }
    |   rangeExpr nodeComp rangeExpr
        {
            ASTBop *bop = static_cast<ASTBop *>($2);

            bop->setLExpr($1);
            bop->setRExpr($3);

            $$ = bop;
        }
    |   rangeExpr valueComp error { delete $2; $$ = $1; }
    |   rangeExpr generalComp error { delete $2; $$ = $1; }
    |   rangeExpr nodeComp error { delete $2; $$ = $1; }
    ;

    /* [49]        RangeExpr      ::=      AdditiveExpr ( "to" AdditiveExpr )? */
rangeExpr:
        additiveExpr
        {
            $$ = $1;
        }
    |   additiveExpr TO additiveExpr
        {
            $$ = new ASTBop(@$, ASTBop::TO, $1, $3);
        }
    |   additiveExpr TO error { $$ = $1; }
    ;

    /* [50]        AdditiveExpr       ::=      MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )* */
additiveExpr:
        multiplicativeExpr
        {
            $$ = $1;
        }
    |   additiveExpr PLUS multiplicativeExpr
        {
            $$ = new ASTBop(@$, ASTBop::PLUS, $1, $3);
        }
    |   additiveExpr MINUS multiplicativeExpr
        {
            $$ = new ASTBop(@$, ASTBop::MINUS, $1, $3);
        }
    |   additiveExpr PLUS error { $$ = $1; }
    |   additiveExpr MINUS error { $$ = $1; }

    ;

    /* [51]        MultiplicativeExpr     ::=      UnionExpr ( ("*" | "div" | "idiv" | "mod") UnionExpr )* */
multiplicativeExpr:
        unionExpr
        {
            $$ = $1;
        }
    |   multiplicativeExpr STAR unionExpr
        {
            $$ = new ASTBop(@$, ASTBop::MULT, $1, $3);
        }
    |   multiplicativeExpr DIV unionExpr
        {
            $$ = new ASTBop(@$, ASTBop::DIV, $1, $3);
        }
    |   multiplicativeExpr IDIV unionExpr
        {
            $$ = new ASTBop(@$, ASTBop::IDIV, $1, $3);
        }
    |   multiplicativeExpr MOD unionExpr
        {
            $$ = new ASTBop(@$, ASTBop::MOD, $1, $3);
        }
    |   multiplicativeExpr STAR error { $$ = $1; }
    |   multiplicativeExpr DIV error { $$ = $1; }
    |   multiplicativeExpr IDIV error { $$ = $1; }
    |   multiplicativeExpr MOD error { $$ = $1; }
    ;

    /* [52]        UnionExpr      ::=      IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )* */
unionExpr:
        intersectExceptExpr
        {
            $$ = $1;
        }
    |   unionExpr UNION intersectExceptExpr
        {
            $$ = new ASTBop(@$, ASTBop::UNION, $1, $3);
        }
    |   unionExpr BAR intersectExceptExpr
        {
            $$ = new ASTBop(@$, ASTBop::UNION, $1, $3);
        }
    |   unionExpr UNION error { $$ = $1; }
    |   unionExpr BAR error { $$ = $1; }
    ;

    /* [53]        IntersectExceptExpr        ::=      InstanceofExpr ( ("intersect" | "except") InstanceofExpr )* */
intersectExceptExpr:
        instanceofExpr
        {
            $$ = $1;
        }
    |   intersectExceptExpr INTERSECT instanceofExpr
        {
            $$ = new ASTBop(@$, ASTBop::INTERSECT, $1, $3);
        }
    |   intersectExceptExpr EXCEPT instanceofExpr
        {
            $$ = new ASTBop(@$, ASTBop::EXCEPT, $1, $3);
        }
    |   intersectExceptExpr INTERSECT error { $$ = $1; }
    |   intersectExceptExpr EXCEPT error { $$ = $1; }
    ;

    /* [54]        InstanceofExpr     ::=      TreatExpr ( "instance" "of" SequenceType )? */
instanceofExpr:
        treatExpr
        {
            $$ = $1;
        }
    |   treatExpr INSTANCE OF sequenceType
        {
            $$ = new ASTInstOf(@$, $1, static_cast<ASTTypeSeq *>($4));
        }
    |   treatExpr INSTANCE OF error { $$ = $1; }
    ;

    /* [55]        TreatExpr      ::=      CastableExpr ( "treat" "as" SequenceType )? */
treatExpr:
        castableExpr
        {
            $$ = $1;
        }
    |   castableExpr TREAT AS sequenceType
        {
            $$ = new ASTTreat(@$, $1, static_cast<ASTTypeSeq *>($4));
        }
    |   castableExpr TREAT AS error { $$ = $1; }
    ;

    /* [56]        CastableExpr       ::=      CastExpr ( "castable" "as" SingleType )? */
castableExpr:
        castExpr
        {
            $$ = $1;
        }
    |   castExpr CASTABLE AS singleType
        {
            $$ = new ASTCastable(@$, $1, static_cast<ASTTypeSingle *>($4));
        }
    |   castExpr CASTABLE AS error { $$ = $1; }
    ;

    /* [57]        CastExpr       ::=      UnaryExpr ( "cast" "as" SingleType )? */
castExpr:
        unaryExpr
        {
            $$ = $1;
        }
    |   unaryExpr CAST AS singleType
        {
            $$ = new ASTCast(@$, $1, static_cast<ASTTypeSingle *>($4));
        }
    |   unaryExpr CAST AS error { $$ = $1; }
    ;

    /* [58]        UnaryExpr      ::=      ("-" | "+")* ValueExpr */
unaryExpr:
        valueExpr
        {
            $$ = $1;
        }
    |   MINUS unaryExpr
        {
            $$ = new ASTUop(@$, ASTUop::MINUS, $2);
        }
    |   PLUS unaryExpr
        {
            $$ = new ASTUop(@$, ASTUop::PLUS, $2);
        }
    |   MINUS error { $$ = new ASTError(@$); }
    |   PLUS error { $$ = new ASTError(@$); }
    ;

    /* [59]        ValueExpr      ::=      ValidateExpr | PathExpr | ExtensionExpr */
valueExpr:
        validateExpr
    |   pathExpr
    |   extensionExpr
    ;

    /* [60]        GeneralComp        ::=      "=" | "!=" | "<" | "<=" | ">" | ">=" */
generalComp:
        EQ_SIGN
        {
            $$ = new ASTBop(@$, ASTBop::EQ_G);
        }
    |   NE_SIGN
        {
            $$ = new ASTBop(@$, ASTBop::NE_G);
        }
    |   LT_OR_ST
        {
            /* since '<' can be a start tag or actually an lt-operation we must tell lexer to discard constructor state
                since by default lexer interprets '<' as a start-tag
            */
            driver.lexer->xqDiscardConstructor();
            $$ = new ASTBop(@$, ASTBop::LT_G);
        }
    |   LE_SIGN
        {
            $$ = new ASTBop(@$, ASTBop::LE_G);
        }
    |   GT_SIGN
        {
            $$ = new ASTBop(@$, ASTBop::GT_G);
        }
    |   GE_SIGN
        {
            $$ = new ASTBop(@$, ASTBop::GE_G);
        }
    ;

    /* [61]        ValueComp      ::=      "eq" | "ne" | "lt" | "le" | "gt" | "ge" */
valueComp:
        EQ
        {
            $$ = new ASTBop(@$, ASTBop::EQ_V);
        }
    |   NE
        {
            $$ = new ASTBop(@$, ASTBop::NE_V);
        }
    |   LT
        {
            $$ = new ASTBop(@$, ASTBop::LT_V);
        }
    |   LE
        {
            $$ = new ASTBop(@$, ASTBop::LE_V);
        }
    |   GT
        {
            $$ = new ASTBop(@$, ASTBop::GT_V);
        }
    |   GE
        {
            $$ = new ASTBop(@$, ASTBop::GE_V);
        }
    ;

    /* [62]        NodeComp       ::=      "is" | "<<" | ">>" */
nodeComp:
        IS
        {
            $$ = new ASTBop(@$, ASTBop::IS);
        }
    |   PREC
        {
            $$ = new ASTBop(@$, ASTBop::PREC);
        }
    |   FOLLOW
        {
            $$ = new ASTBop(@$, ASTBop::FOLLOW);
        }
    ;

    /* [63]        ValidateExpr       ::=      "validate" ValidationMode? "{" Expr "}" */
validateExpr:
        VALIDATE LBRACE expr RBRACE
        {
            delete $3;
            errorc(this->driver, @$, XQST0075);
            YYABORT;

            $$ = NULL;
        }
    |   VALIDATE validationMode LBRACE expr RBRACE
        {
            delete $4;
            errorc(this->driver, @$, XQST0075);
            YYABORT;

            $$ = NULL;
        }
    |   VALIDATE LBRACE error RBRACE
        {
            errorc(this->driver, @$, XQST0075);
            YYABORT;

            $$ = NULL;
        }
    |   VALIDATE validationMode LBRACE error RBRACE
        {
            errorc(this->driver, @$, XQST0075);
            YYABORT;

            $$ = NULL;
        }
    ;

    /* [64]        ValidationMode     ::=      "lax" | "strict" */
validationMode:
        LAX
    |   _STRICT
    ;

    /* [65]        ExtensionExpr      ::=      Pragma+ "{" Expr? "}"*/
extensionExpr:
        pragmas LBRACE expr RBRACE
        {
            $$ = new ASTExtExpr(@$, $1, $3);
        }
    |   pragmas LBRACE RBRACE
        {
            $$ = new ASTExtExpr(@$, $1, NULL);
        }
    |   pragmas LBRACE error RBRACE { destroyASTNodesVector($1); $$ = new ASTError(@$); }
    ;

    /* pragma+ */
pragmas:
        pragma
        {
            $$ = new ASTNodesVector();
            $$->push_back($1);
        }
    |   pragmas pragma
        {
            $1->push_back($2);
            $$ = $1;
        }
    |   error { $$ = new ASTNodesVector(); }
    ;

    //    [66]        Pragma     ::=      "(#" S? QName (S PragmaContents)? "#)"  /* ws: explicit */
    //    [67]        PragmaContents     ::=      (Char* - (Char* '#)' Char*))
pragma:
        PRAGMA_BEG qName PRAGMA_CONT_WITH_END
        {
            $$ = new ASTPragma(@$, $2, $3);
        }
    ;

    /* [68]        PathExpr       ::=      ("/" RelativePathExpr?)
                                        | ("//" RelativePathExpr)
                                        | RelativePathExpr */
pathExpr:
        SLASH
        {
            $$ = new ASTTreat(@$,
                    new ASTFunCall(@$, new std::string("fn"), new std::string("root"), NULL), new ASTTypeSeq(@$, new ASTDocTest(@$)));
        }
    |   SLASH relativePathExpr
        {
            $$ = makePathExpr($2, new ASTTreat(@$, new ASTFunCall(@$, new std::string("fn"), new std::string("root"), NULL), new ASTTypeSeq(@$, new ASTDocTest(@$))));
        }
    |   SLASH_SLASH relativePathExpr
        {
            ASTAxisStep *cont = new ASTAxisStep(@$, ASTAxisStep::DESCENDANT_OR_SELF, new ASTNodeTest(@$), NULL);
            cont->setContext(new ASTTreat(@$, new ASTFunCall(@$, new std::string("fn"), new std::string("root"), NULL), new ASTTypeSeq(@$, new ASTDocTest(@$))));

            $$ = makePathExpr($2, cont);
        }
    |   relativePathExpr
        {
            $$ = makePathExpr($1, NULL);
        }

    | SLASH_SLASH error { $$ = new ASTError(@$); }
    ;

    // [69]        RelativePathExpr       ::=      StepExpr (("/" | "//") StepExpr)*
relativePathExpr:
        stepExpr
        {
            ASTNode *res = $1;

    /*
            if (ASTFilterStep *fs = dynamic_cast<ASTFilterStep *>($1))
            {
                // get rid of ASTFilterStep for first-step primary expressions
                if (!fs->getPreds() && fs->getExpr())
                {
                    res = fs->getExpr()->dup();
                    delete $1;
                }
            }
    */

            $$ = res;
        }
    |   relativePathExpr SLASH stepExpr
        {
            dynamic_cast<ASTStep *>($3)->setContext($1);
            $$ = $3;
        }
    |   relativePathExpr SLASH_SLASH stepExpr
        {
            ASTAxisStep *ss_step = new ASTAxisStep(@$, ASTAxisStep::DESCENDANT_OR_SELF, new ASTNodeTest(@$), NULL);

            ss_step->setContext($1);
            dynamic_cast<ASTStep *>($3)->setContext(ss_step);
            $$ = $3;
        }
    |   relativePathExpr SLASH error { $$ = $1; }
    |   relativePathExpr SLASH_SLASH error { $$ = $1; }
    ;

    /* [70]        StepExpr       ::=      FilterExpr | AxisStep */
stepExpr:
        filterExpr
    |   axisStep
    ;

    /* [71]        AxisStep       ::=      (ReverseStep | ForwardStep) PredicateList */
axisStep:
        reverseStep
        {
            $$ = $1;
        }
    |   forwardStep
        {
            $$ = $1;
        }
    |   reverseStep predicateList
        {
            static_cast<ASTAxisStep *>($1)->setPredicates($2);

            $$ = $1;
        }
    |   forwardStep predicateList
        {
            static_cast<ASTAxisStep *>($1)->setPredicates($2);

            $$ = $1;
        }
    ;

    /* [72]        ForwardStep        ::=      (ForwardAxis NodeTest) | AbbrevForwardStep */
forwardStep:
        forwardAxis nodeTest
        {
            static_cast<ASTAxisStep *>($1)->setNodeTest($2);
            $$ = $1;
        }
    |   abbrevForwardStep
        {
            $$ = $1;
        }
    ;

    /*[73]        ForwardAxis        ::=      ("child" "::")
                                            | ("descendant" "::")
                                            | ("attribute" "::")
                                            | ("self" "::")
                                            | ("descendant-or-self" "::")
                                            | ("following-sibling" "::")
                                            | ("following" "::")*/
forwardAxis:
        CHILD AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::CHILD);
        }
    |   DESCENDANT AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::DESCENDANT);
        }
    |   ATTRIBUTE AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::ATTRIBUTE);
        }
    |   SELF AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::SELF);
        }
    |   DESCENDANT_OR_SELF AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::DESCENDANT_OR_SELF);
        }
    |   FOLLOWING_SIBLING AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::FOLLOWING_SIBLING);
        }
    |   FOLLOWING AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::FOLLOWING);
        }
    ;

    /* [74]        AbbrevForwardStep      ::=      "@"? NodeTest */
abbrevForwardStep:
        ATSIGN nodeTest
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::ATTRIBUTE, $2);
        }
    |   nodeTest
        {
            if (dynamic_cast<ASTAttribTest *>($1))
                $$ = new ASTAxisStep(@$, ASTAxisStep::ATTRIBUTE, $1);
            else
                $$ = new ASTAxisStep(@$, ASTAxisStep::CHILD, $1);
        }
    ;

    /* [75]        ReverseStep        ::=      (ReverseAxis NodeTest) | AbbrevReverseStep */
reverseStep:
        reverseAxis nodeTest
        {
            static_cast<ASTAxisStep *>($1)->setNodeTest($2);
            $$ = $1;
        }
    |   abbrevReverseStep
        {
            $$ = $1;
        }
    ;

    /*[76]        ReverseAxis        ::=      ("parent" "::")
                                            | ("ancestor" "::")
                                            | ("preceding-sibling" "::")
                                            | ("preceding" "::")
                                            | ("ancestor-or-self" "::")*/
reverseAxis:
        PARENT AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::PARENT);
        }
    |   ANCESTOR AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::ANCESTOR);
        }
    |   PRECEDING_SIBLING AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::PRECEDING_SIBLING);
        }
    |   PRECEDING AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::PRECEDING);
        }
    |   ANCESTOR_OR_SELF AXIS
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::ANCESTOR_OR_SELF);
        }
    ;

    /* [77]        AbbrevReverseStep      ::=      ".." */
abbrevReverseStep:
        DOT_DOT
        {
            $$ = new ASTAxisStep(@$, ASTAxisStep::PARENT, new ASTNodeTest(@$));
        }
    ;

    /* [78]        NodeTest       ::=      KindTest | NameTest */
nodeTest:
        kindTest
    |   nameTest
    ;

    /* [79]        NameTest       ::=      QName | Wildcard */
nameTest:
        qName
        {
            $$ = new ASTNameTest(@$, $1);
        }
    |   wildcard
        {
            $$ = new ASTNameTest(@$, $1);
        }
    ;

    /* [80]        Wildcard       ::=      "*"
                                         | (NCName ":" "*")
                                         | ("*" ":" NCName)*/
wildcard:
        STAR
        {
            $$ = new std::string("*:*");
        }
    |   PREF_WCARD
        {
            $$ = $1;
        }
    |   WCARD_LOC
        {
            $$ = $1;
        }
    ;

    /* [81]        FilterExpr     ::=      PrimaryExpr PredicateList */
filterExpr:
        primaryExpr
        {
            $$ = new ASTFilterStep(@$, $1);
        }
    |   primaryExpr predicateList
        {
            ASTFilterStep *fs = new ASTFilterStep(@$, $1);
            fs->setPredicates($2);

            $$ = fs;
        }
    ;

    // [82]     PredicateList      ::=      Predicate*
predicateList:
        predicate
        {
            $$ = new ASTNodesVector();
            $$->push_back($1);
        }
    |   predicateList predicate
        {
            $1->push_back($2);
            $$ = $1;
        }
    ;

    // [83]        Predicate      ::=      "[" Expr "]"
predicate:
        LBRACK expr RBRACK
        {
            $$ = new ASTPred(@$, $2);
        }
        
    |   LBRACK error RBRACK { $$ = new ASTError(@$); }

    ;

    /* [84]        PrimaryExpr        ::=      Literal | VarRef | ParenthesizedExpr | ContextItemExpr | FunctionCall | OrderedExpr | UnorderedExpr | Constructor */
primaryExpr:
        literal
    |   varRef
    |   parenthesizedExpr
    |   contextItemExpr
    |   functionCall
    |   orderedExpr
    |   unorderedExpr
    |   constructor
    ;

    /* [85]        Literal        ::=      NumericLiteral | StringLiteral */
literal:
        numericLiteral
        {
            $$ = $1;
        }
    |   StringLiteral
        {
            $$ = new ASTLit(@$, ASTLit::STRING, $1);
        }
    ;

    /* [86]        NumericLiteral     ::=      IntegerLiteral | DecimalLiteral | DoubleLiteral */
numericLiteral:
        IntegerLiteral
        {
            $$ = new ASTLit(@$, ASTLit::INTEGER, $1);
        }
    |   DecimalLiteral
        {
            $$ = new ASTLit(@$, ASTLit::DECIMAL, $1);
        }
    |   DoubleLiteral
        {
            $$ = new ASTLit(@$, ASTLit::DOUBLE, $1);
        }
    ;

    /* [87]        VarRef     ::=      "$" VarName */
varRef:
        DOLLAR varName
        {
            $$ = new ASTVar(@$, $2);
        }
    ;

    /* [88]        VarName        ::=      QName */
varName:
        qName
    ;

    // [89]        ParenthesizedExpr      ::=      "(" Expr? ")"
parenthesizedExpr:
        LPAREN RPAREN
        {
            $$ = new ASTSeq(@$);
        }
    |   LPAREN expr RPAREN
        {
            $$ = $2;
        }
    |   LPAREN error RPAREN { $$ = new ASTError(@$); }
    ;

    // [90]        ContextItemExpr        ::=      "."
contextItemExpr:
        DOT
        {
            $$ = NULL; // eventually ASTFilterStep will be created
        }
    ;

    // [91]        OrderedExpr        ::=      "ordered" "{" Expr "}"
orderedExpr:
        ORDERED LBRACE expr RBRACE
        {
            $$ = new ASTOrdExpr(@$, ASTOrdExpr::ORDERED, $3);
        }
    |   ORDERED LBRACE error RBRACE { $$ = new ASTError(@$); }

    ;

    // [92]        UnorderedExpr        ::=      "unordered" "{" Expr "}"
unorderedExpr:
        UNORDERED LBRACE expr RBRACE
        {
            $$ = new ASTOrdExpr(@$, ASTOrdExpr::UNORDERED, $3);
        }
    |   UNORDERED LBRACE error RBRACE { $$ = new ASTError(@$); }
    ;

    // [93]        FunctionCall       ::=      QName "(" (ExprSingle ("," ExprSingle)*)? ")"
    // NOTE: we use funcName instead of QName to grammaticaly distinguish subset of valid function names; see 'xgs:reserved-function-names'
functionCall:
        funcName LPAREN funcParams RPAREN
        {
            $$ = new ASTFunCall(@$, $1, $3);
        }
    |   funcName LPAREN RPAREN
        {
            $$ = new ASTFunCall(@$, $1);
        }
    |   funcName LPAREN error RPAREN { delete $1; $$ = new ASTError(@$); }

    ;

    // (ExprSingle ("," ExprSingle)*)?
funcParams:
        exprSingle
        {
            $$ = new ASTNodesVector();
            $$->push_back($1);
        }
    |   funcParams COMMA exprSingle
        {
            $1->push_back($3);
            $$ = $1;
        }
    |   funcParams COMMA error { $$ = $1; }
    ;

    /*[94]        Constructor        ::=      DirectConstructor
                                            | ComputedConstructor */
constructor:
        directConstructor
    |   computedConstructor
    ;

    /* [95]        DirectConstructor      ::=      DirElemConstructor
                                                 | DirCommentConstructor
                                                 | DirPIConstructor */

directConstructor:
        dirElemConstructor
    |   dirCommentConstructor
    |   dirPIConstructor
    ;

    // [96]        DirElemConstructor     ::=      "<" QName DirAttributeList ("/>" | (">" DirElemContent* "</" QName S? ">"))
dirElemConstructor:
        LT_OR_ST qName dirAttributeList optS EMPTY_TAG
        {
            $$ = new ASTElem(@$, $2, $3);
        }
    |   LT_OR_ST qName optS EMPTY_TAG
        {
            $$ = new ASTElem(@$, $2);
        }
    |   LT_OR_ST qName dirAttributeList optS START_TAG_END END_TAG_OPEN qName optS END_TAG_CLOSE
        {
            // first, check if qNames in open and close tags are equal
            if (*($2) != *($7))
            {
                error(@$, std::string("start tag '") + *($2) + "' does not match end tag '" + *($7) + "'");

                // should clean rhs-symbols
                delete $2;
                destroyASTNodesVector($3); /* do a deep delete */
                delete $7;

                YYABORT;

                $$ = NULL;
            }

            $$ = new ASTElem(@$, $2, $3);
            delete $7;
        }
    |   LT_OR_ST qName optS START_TAG_END END_TAG_OPEN qName optS END_TAG_CLOSE
        {
            // first, check if qNames in open and close tags are equal
            if (*($2) != *($6))
            {
                error(@$, std::string("start tag '") + *($2) + "' does not match end tag '" + *($6) + "'");

                // should clean rhs-symbols
                delete $2;
                delete $6;

                YYABORT;

                $$ = NULL;
            }

            $$ = new ASTElem(@$, $2);
            delete $6;
        }
    |   LT_OR_ST qName optS START_TAG_END dirElementContentList END_TAG_OPEN qName optS END_TAG_CLOSE
        {
            // first, check if qNames in open and close tags are equal
            if (*($2) != *($7))
            {
                error(@$, std::string("start tag '") + *($2) + "' does not match end tag '" + *($7) + "'");

                // should clean rhs-symbols
                delete $2;
                destroyASTNodesVector($5); /* do a deep delete */
                delete $7;

                YYABORT;

                $$ = NULL;
            }

            ProcessDirectContent($5, isPreserveBoundSpace);

            // content became none (e.g. boundary spaces were stripped)
            if ($5->size() == 0)
            {
                delete $5;
                $5 = NULL;
            }

            $$ = new ASTElem(@$, $2, (ASTNodesVector *)NULL, $5);
            delete $7;
        }
    |   LT_OR_ST qName dirAttributeList optS START_TAG_END dirElementContentList END_TAG_OPEN qName optS END_TAG_CLOSE
        {
            // first, check if qNames in open and close tags are equal
            if (*($2) != *($8))
            {
                error(@$, std::string("start tag '") + *($2) + "' does not match end tag '" + *($8) + "'");

                // should clean rhs-symbols
                delete $2;
                destroyASTNodesVector($3); /* do a deep delete */
                destroyASTNodesVector($6); /* do a deep delete */
                delete $8;

                YYABORT;

                $$ = NULL;
            }

            ProcessDirectContent($6, isPreserveBoundSpace);

            // content became none (e.g. boundary spaces were stripped)
            if ($6->size() == 0)
            {
                destroyASTNodesVector($6);
                $6 = NULL;
            }

            $$ = new ASTElem(@$, $2, $3, $6);
            delete $8;
        }

    |   LT_OR_ST qName dirAttributeList optS START_TAG_END error END_TAG_OPEN qName optS END_TAG_CLOSE
        {
            if (*($2) != *($8))
            {
                error(@$, std::string("start tag '") + *($2) + "' does not match end tag '" + *($8) + "'");
            }

            destroyASTNodesVector($3);
            delete $2;
            delete $8;

            $$ = new ASTError(@$);
        }
    ;

    // [97]        DirAttributeList       ::=      (S (QName S? "=" S? DirAttributeValue)?)*
dirAttributeList:
        dirAttribute
        {
            $$ = new ASTNodesVector();

            $$->push_back($1);
        }
    |   dirAttributeList dirAttribute
        {
            $1->push_back($2);
            $$ = $1;
        }
    |   error { $$ = new ASTNodesVector(); }
    |   dirAttributeList error { $$ = $1; }
    ;

    // (S (QName S? "=" S? DirAttributeValue)?)
dirAttribute:
        S qName optS EQ_SIGN optS dirAttributeValue
        {
            ASTNsp *nsp;
            // merge char content
            ProcessDirectContent($6, true);

            // check if attribute is in fact namespace definition
            if ($2->find("xmlns:") == 0)
            {
                if ($6 && ($6->size() != 1 || !dynamic_cast<ASTCharCont *>($6->back())))
                {
                    errorc(this->driver, @6, XQST0022);
                    delete $2;
                    destroyASTNodesVector($6);
                    YYABORT;

                    $$ = NULL;
                }

                $2->erase(0, 6); // erase "xmlns:"
                nsp = new ASTNsp(@$, $2, ($6) ? new std::string(*static_cast<ASTCharCont *>($6->back())->cont) : NULL);

                if ($6 && $6->size() > 0)
                    delete $6->back();
                delete $6;

                $$ = nsp;
            }
            else if (*($2) == "xmlns")
            {
                ASTNsp *nsp;

                if ($6 && ($6->size() != 1 || !dynamic_cast<ASTCharCont *>($6->back())))
                {
                    errorc(this->driver, @6, XQST0022);
                    delete $2;
                    destroyASTNodesVector($6);
                    YYABORT;

                    $$ = NULL;
                }

                *($2) = "";
                nsp = new ASTNsp(@$, $2, ($6) ? new std::string(*static_cast<ASTCharCont *>($6->back())->cont) : NULL);

                if ($6 && $6->size() > 0)
                    delete $6->back();
                delete $6;

                $$ = nsp;
            }
            else
            {
                $$ = new ASTAttr(@$, $2, $6);
            }
        }
    ;

    /* [98]        DirAttributeValue      ::=      ('"' (EscapeQuot | QuotAttrValueContent)* '"')
                                                 | ("'" (EscapeApos | AposAttrValueContent)* "'") */
dirAttributeValue:
        QUOT attributeValue QUOT
        {
            $$ = $2;
        }
    |   QUOT QUOT
        {
            $$ = NULL;
        }
    |   APOS attributeValue APOS
        {
            $$ = $2;
        }
    |   APOS APOS
        {
            $$ = NULL;
        }

    |   QUOT error QUOT { $$ = NULL; }
    |   APOS error APOS { $$ = NULL; }
    ;

    /*  [99]        QuotAttrValueContent       ::=      QuotAttrContentChar
                                                      | CommonContent

        [100]       AposAttrValueContent       ::=      AposAttrContentChar
                                                      | CommonContent

                    (EscapeQuot | QuotAttrValueContent)*
                    (EscapeQuot | QuotAttrValueContent)*

        NOTE: here we merge content for two types of attributes since they are very similar in nature and lexer evens the differences;
              QuotAttrContentChar and AposAttrContentChar are returned as CONT_CHAR by lexer;
    */
attributeValue:
        ESCAPE_QUOT
        {
            $$ = new ASTNodesVector();

            $$->push_back(new ASTCharCont(@$, "\""));
        }
    |   ESCAPE_APOS
        {
            $$ = new ASTNodesVector();

            $$->push_back(new ASTCharCont(@$, "\'"));
        }
    |   CHAR_CONT
        {
            $$ = new ASTNodesVector();

            $$->push_back(new ASTCharCont(@$, $1));
        }
    |   commonContent
        {
            $$ = new ASTNodesVector();

            $$->push_back($1);
        }
    |   attributeValue ESCAPE_QUOT
        {
            $1->push_back(new ASTCharCont(@2, "\""));

            $$ = $1;
        }
    |   attributeValue ESCAPE_APOS
        {
            $1->push_back(new ASTCharCont(@2, "\'"));

            $$ = $1;
        }
    |   attributeValue CHAR_CONT
        {
            $1->push_back(new ASTCharCont(@2, $2));

            $$ = $1;
        }
    |   attributeValue commonContent
        {
            $1->push_back($2);

            $$ = $1;
        }
    |   attributeValue error { $$ = $1; }
    ;

        /* DirElemContent* */
dirElementContentList:
        dirElemContent
        {
            $$ = new ASTNodesVector();
            $$->push_back($1);
        }
    |   dirElementContentList dirElemContent
        {
            $1->push_back($2);
            $$ = $1;
        }
    |   dirElementContentList error { $$ = $1; }
    ;


    /* [101]       DirElemContent     ::=      DirectConstructor
                                            | CDataSection
                                            | CommonContent
                                            | ElementContentChar
    */
dirElemContent:
        directConstructor
        {
            $$ = $1;
        }
    |   cdataSection
        {
            $$ = new ASTCharCont(@$, $1, ASTCharCont::CDATA);
        }

    |   commonContent
        {
            $$ = $1;
        }
    |   CHAR_CONT
        {
            $$ = new ASTCharCont(@$, $1);
        }
    ;

    /* [102]       CommonContent      ::=      PredefinedEntityRef | CharRef | "{{" | "}}" | EnclosedExpr */
commonContent:
        PREF
        {
            $$ = new ASTCharCont(@$, $1);
        }
    |   CREF
        {
            $$ = new ASTCharCont(@$, $1, ASTCharCont::CREF);
        }
    |   DOUB_LBRACE
        {
            $$ = new ASTCharCont(@$, "{");
        }
    |   DOUB_RBRACE
        {
            $$ = new ASTCharCont(@$, "}");
        }
    |   enclosedExpr
        {
            $$ = new ASTSpaceSeq(@$, $1);
        }
    ;

    // [103]       DirCommentConstructor      ::=      "<!--" DirCommentContents "-->"
    // [104]       DirCommentContents     ::=      ((Char - '-') | ('-' (Char - '-')))*
dirCommentConstructor:
        XML_COMM_BEG XML_CONT_WITH_END
        {
            $$ = new ASTXMLComm(@$, $2);
        }
    ;

    // [105]       DirPIConstructor       ::=      "<?" PITarget (S DirPIContents)? "?>"
    // [106]       DirPIContents      ::=      (Char* - (Char* '?>' Char*))
dirPIConstructor:
        PI_START PI_TARGET PI_CONT_WITH_END
        {
            if ($2->find(":") != std::string::npos)
            {
                error(@2, std::string("NCName is expected, but QName is given: '") + *($2) + "'");
                delete $2;
                delete $3;

                $$ = new ASTError(@$);
            }
            else
                $$ = new ASTPi(@$, $2, $3);
        }
    ;

    // [107]       CDataSection       ::=      "<![CDATA[" CDataSectionContents "]]>"
    // [108]       CDataSectionContents       ::=      (Char* - (Char* ']]>' Char*))
cdataSection:
        CDATA_BEG CDATA_CONT_WITH_END
        {
            $$ = $2;
        }
    ;

/* [109]       ComputedConstructor        ::=      CompDocConstructor
                                                 | CompElemConstructor
                                                 | CompAttrConstructor
                                                 | CompTextConstructor
                                                 | CompCommentConstructor
                                                 | CompPIConstructor */
computedConstructor:
        compDocConstructor
    |   compElemConstructor
    |   compAttrConstructor
    |   compTextConstructor
    |   compCommentConstructor
    |   compPIConstructor
    ;

    // [110]       CompDocConstructor     ::=      "document" "{" Expr "}"
compDocConstructor:
        DOCUMENT LBRACE expr RBRACE
        {
            $$ = new ASTDocConst(@$, new ASTSpaceSeq(@3, $3));
        }
    |   DOCUMENT LBRACE error RBRACE { $$ = new ASTError(@$); }
    ;

    // [111]       CompElemConstructor        ::=      "element" (QName | ("{" Expr "}")) "{" ContentExpr? "}"
compElemConstructor:
        ELEMENT COMP_CONST_QNAME LBRACE RBRACE
        {
            $$ = new ASTElemConst(@$, $2);
        }
    |   ELEMENT COMP_CONST_QNAME LBRACE contentExpr RBRACE
        {
            $$ = new ASTElemConst(@$, $2, new ASTSpaceSeq(@4, $4));
        }
    |   ELEMENT LBRACE expr RBRACE LBRACE RBRACE
        {
            $$ = new ASTElemConst(@$, $3);
        }
    |   ELEMENT LBRACE expr RBRACE LBRACE contentExpr RBRACE
        {
            $$ = new ASTElemConst(@$, $3, new ASTSpaceSeq(@6, $6));
        }

    |   ELEMENT COMP_CONST_QNAME LBRACE error RBRACE { delete $2; $$ = new ASTError(@$); }
    |   ELEMENT LBRACE error RBRACE LBRACE RBRACE { $$ = new ASTError(@$); }
    |   ELEMENT LBRACE error RBRACE LBRACE error RBRACE { $$ = new ASTError(@$); }
    |   ELEMENT LBRACE error RBRACE LBRACE contentExpr RBRACE { delete $6; $$ = new ASTError(@$); }
    ;


    /* [112]       ContentExpr        ::=      Expr */
contentExpr:
        expr
    ;

    // [113]       CompAttrConstructor        ::=      "attribute" (QName | ("{" Expr "}")) "{" Expr? "}"
compAttrConstructor:
        ATTRIBUTE COMP_CONST_QNAME LBRACE RBRACE
        {
            $$ = new ASTAttrConst(@$, $2);
        }
    |   ATTRIBUTE COMP_CONST_QNAME LBRACE expr RBRACE
        {
            $$ = new ASTAttrConst(@$, $2, new ASTSpaceSeq(@4, $4));
        }
    |   ATTRIBUTE LBRACE expr RBRACE LBRACE RBRACE
        {
            $$ = new ASTAttrConst(@$, $3);
        }
    |   ATTRIBUTE LBRACE expr RBRACE LBRACE expr RBRACE
        {
            $$ = new ASTAttrConst(@$, $3, new ASTSpaceSeq(@6, $6));
        }

    |   ATTRIBUTE COMP_CONST_QNAME LBRACE error RBRACE { delete $2; $$ = new ASTError(@$); }
    |   ATTRIBUTE LBRACE error RBRACE LBRACE RBRACE { $$ = new ASTError(@$); }
    |   ATTRIBUTE LBRACE error RBRACE LBRACE error RBRACE { $$ = new ASTError(@$); }
    |   ATTRIBUTE LBRACE error RBRACE LBRACE expr RBRACE { delete $6; $$ = new ASTError(@$); }
    ;

    // [114]       CompTextConstructor        ::=      "text" "{" Expr "}"
compTextConstructor:
        TEXT LBRACE expr RBRACE
        {
            $$ = new ASTTextConst(@$, new ASTSpaceSeq(@3, $3));
        }
    |   TEXT LBRACE error RBRACE { $$ = new ASTError(@$); }
    ;

    // [115]       CompCommentConstructor     ::=      "comment" "{" Expr "}"
compCommentConstructor:
        COMMENT LBRACE expr RBRACE
        {
            $$ = new ASTCommentConst(@$, new ASTSpaceSeq(@3, $3));
        }
    |   COMMENT LBRACE error RBRACE { $$ = new ASTError(@$); }
    ;

    // [116]       CompPIConstructor      ::=      "processing-instruction" (NCName | ("{" Expr "}")) "{" Expr? "}"
compPIConstructor:
        PROCESSING_INSTRUCTION COMP_CONST_QNAME LBRACE RBRACE
        {
            // check for NCNameness and reserved name (xml, Xml, etc.)
            if ($2->find(":") != std::string::npos)
            {
                error(@2, std::string("NCName is expected, but QName is given: '") + *($2) + "'");
                delete $2;

                $$ = new ASTError(@$);
            }
            else
                $$ = new ASTPIConst(@$, $2);
        }
    |   PROCESSING_INSTRUCTION COMP_CONST_QNAME LBRACE expr RBRACE
        {
            // check for NCNameness and reserved name (xml, Xml, etc.)
            if ($2->find(":") != std::string::npos)
            {
                error(@2, std::string("NCName is expected, but QName is given: '") + *($2) + "'");
                delete $2;
                delete $4;

                $$ = new ASTError(@$);
            }
            else
                $$ = new ASTPIConst(@$, $2, new ASTSpaceSeq(@4, $4));
        }
    |   PROCESSING_INSTRUCTION LBRACE expr RBRACE LBRACE RBRACE
        {
            $$ = new ASTPIConst(@$, $3);
        }
    |   PROCESSING_INSTRUCTION LBRACE expr RBRACE LBRACE expr RBRACE
        {
            $$ = new ASTPIConst(@$, $3, new ASTSpaceSeq(@6, $6));
        }

    |   PROCESSING_INSTRUCTION COMP_CONST_QNAME LBRACE error RBRACE { delete $2; $$ = new ASTError(@$); }
    |   PROCESSING_INSTRUCTION LBRACE error RBRACE LBRACE RBRACE { $$ = new ASTError(@$); }
    |   PROCESSING_INSTRUCTION LBRACE error RBRACE LBRACE error RBRACE { $$ = new ASTError(@$); }
    |   PROCESSING_INSTRUCTION LBRACE error RBRACE LBRACE expr RBRACE { delete $6; $$ = new ASTError(@$); }
    ;

    // [117]       SingleType     ::=      AtomicType "?"?
singleType:
        atomicType
        {
            $$ = new ASTTypeSingle(@$, new ASTType(@1, $1, ASTType::ATOMIC), ASTTypeSingle::ONE);
        }
    |   atomicType QUEST
        {
            $$ = new ASTTypeSingle(@$, new ASTType(@1, $1, ASTType::ATOMIC), ASTTypeSingle::OPT);
        }
    ;

    /* [118]       TypeDeclaration        ::=      "as" SequenceType */
typeDeclaration:
        AS sequenceType
        {
            $$ = $2;
        }
    ;

    /* [119]       SequenceType       ::=      ("empty-sequence" "(" ")")
                                            | (ItemType OccurrenceIndicator?) */
    // [120]       OccurrenceIndicator        ::=      "?" | "*" | "+"
sequenceType:
        EMPTY_SEQUENCE LPAREN RPAREN
        {
            $$ = new ASTTypeSeq(@$, new ASTEmptyTest(@$), ASTTypeSeq::EMPTY);
        }
    |   itemType
        {
            $$ = new ASTTypeSeq(@$, $1);
        }
    |   itemType QUEST
        {
            $$ = new ASTTypeSeq(@$, $1, ASTTypeSeq::OPT);
        }
    |   itemType STAR
        {
            $$ = new ASTTypeSeq(@$, $1, ASTTypeSeq::ZERO_OR_MORE);
        }
    |   itemType PLUS
        {
            $$ = new ASTTypeSeq(@$, $1, ASTTypeSeq::ONE_OR_MORE);
        }
    ;

    /* [121]       ItemType       ::=      KindTest | ("item" "(" ")") | AtomicType */
itemType:
        kindTest
        {
            $$ = $1;
        }
    |   ITEM LPAREN RPAREN
        {
            $$ = new ASTItemTest(@$);
        }
    |   atomicType
        {
            $$ = new ASTType(@$, $1, ASTType::ATOMIC);
        }
    ;

    /* [122]       AtomicType     ::=      QName */
atomicType:
        qName
        {
            $$ = $1;
        }
    ;

    /* [123]       KindTest       ::=      DocumentTest
                                         | ElementTest
                                         | AttributeTest
                                         | SchemaElementTest
                                         | SchemaAttributeTest
                                         | PITest
                                         | CommentTest
                                         | TextTest
                                         | AnyKindTest
    */
kindTest:
        documentTest
    |   elementTest
    |   attributeTest
    |   schemaElementTest
    |   schemaAttributeTest
    |   piTest
    |   commentTest
    |   textTest
    |   anyKindTest
    ;

    // [124]       AnyKindTest        ::=      "node" "(" ")"
anyKindTest:
        NODE LPAREN RPAREN
        {
            $$ = new ASTNodeTest(@$);
        }
    ;

    /* [125]       DocumentTest       ::=      "document-node" "(" (ElementTest | SchemaElementTest)? ")" */
documentTest:
        DOCUMENT_NODE LPAREN RPAREN
        {
            $$ = new ASTDocTest(@$);
        }
    |   DOCUMENT_NODE LPAREN elementTest RPAREN
        {
            $$ = new ASTDocTest(@$, $3);
        }
    |   DOCUMENT_NODE LPAREN schemaElementTest RPAREN
        {
            $$ = new ASTDocTest(@$, $3);
        }
    |   DOCUMENT_NODE LPAREN error RPAREN { $$ = new ASTError(@$); }
    ;

    // [126]       TextTest       ::=      "text" "(" ")"
textTest:
        TEXT LPAREN RPAREN
        {
            $$ = new ASTTextTest(@$);
        }
    ;

    /* [127]       CommentTest        ::=      "comment" "(" ")" */
commentTest:
        COMMENT LPAREN RPAREN
        {
            $$ = new ASTCommTest(@$);
        }
    ;

    /* [128]       PITest     ::=      "processing-instruction" "(" (NCName | StringLiteral)? ")" */
piTest:
        PROCESSING_INSTRUCTION LPAREN RPAREN
        {
            $$ = new ASTPiTest(@$);
        }
    |   PROCESSING_INSTRUCTION LPAREN ncName RPAREN
        {
            $$ = new ASTPiTest(@$, $3, ASTPiTest::NCNAME);
        }
    |   PROCESSING_INSTRUCTION LPAREN StringLiteral RPAREN
        {
            $$ = new ASTPiTest(@$, $3, ASTPiTest::STRING);
        }
    |   PROCESSING_INSTRUCTION LPAREN error RPAREN { $$ = new ASTError(@$); }
    ;

    // [129]       AttributeTest      ::=      "attribute" "(" (AttribNameOrWildcard ("," TypeName)?)? ")"
attributeTest:
        ATTRIBUTE LPAREN RPAREN
        {
            $$ = new ASTAttribTest(@$);
        }
    |   ATTRIBUTE LPAREN attribNameOrWildcard RPAREN
        {
            $$ = new ASTAttribTest(@$, new ASTNameTest(@3, $3));
        }
    |   ATTRIBUTE LPAREN attribNameOrWildcard COMMA typeName RPAREN
        {
            $$ = new ASTAttribTest(@$, new ASTNameTest(@3, $3), $5);
        }
    |   ATTRIBUTE LPAREN error RPAREN { $$ = new ASTError(@$); }
    ;

    // [130]       AttribNameOrWildcard       ::=      AttributeName | "*"
attribNameOrWildcard:
        attributeName
        {
            $$ = $1;
        }
    |   STAR
        {
            $$ = new std::string("*:*");
        }
    ;

    // [131]       SchemaAttributeTest        ::=      "schema-attribute" "(" AttributeDeclaration ")"
schemaAttributeTest:
        SCHEMA_ATTRIBUTE LPAREN attributeDeclaration RPAREN
        {
            $$ = new ASTSchemaAttrTest(@$, new ASTNameTest(@3, $3));
        }
    |   SCHEMA_ATTRIBUTE LPAREN error RPAREN
        {
            $$ = new ASTError(@$);
        }
    ;

    /* [132]       AttributeDeclaration       ::=      AttributeName */
attributeDeclaration:
        attributeName
    ;

    // [133]       ElementTest        ::=      "element" "(" (ElementNameOrWildcard ("," TypeName "?"?)?)? ")"
elementTest:
        ELEMENT LPAREN RPAREN
        {
            $$ = new ASTElementTest(@$);
        }
    |   ELEMENT LPAREN elementNameOrWildcard RPAREN
        {
            $$ = new ASTElementTest(@$, new ASTNameTest(@3, $3));
        }
    |   ELEMENT LPAREN elementNameOrWildcard COMMA typeName RPAREN
        {
            $$ = new ASTElementTest(@$, new ASTNameTest(@3, $3), $5);
        }
    |   ELEMENT LPAREN elementNameOrWildcard COMMA typeName QUEST RPAREN
        {
            $$ = new ASTElementTest(@$, new ASTNameTest(@3, $3), $5, ASTElementTest::ANY_NIL);
        }
    |   ELEMENT LPAREN error RPAREN { $$ = new ASTError(@$); }
    ;

    // [134]       ElementNameOrWildcard      ::=      ElementName | "*"
elementNameOrWildcard:
        elementName
        {
            $$ = $1;
        }
    |   STAR
        {
            $$ = new std::string("*:*");
        }
    ;

    // [135]       SchemaElementTest      ::=      "schema-element" "(" ElementDeclaration ")"
schemaElementTest:
        SCHEMA_ELEMENT LPAREN elementDeclaration RPAREN
        {
            $$ = new ASTSchemaElemTest(@$, new ASTNameTest(@3, $3));
        }
    |   SCHEMA_ELEMENT LPAREN error RPAREN
        {
            $$ = new ASTError(@$);
        }
    ;

    // [136]       ElementDeclaration     ::=      ElementName
elementDeclaration:
        elementName
    ;

    // [137]       AttributeName      ::=      QName
attributeName:
        qName
    ;

    // [138]       ElementName        ::=      QName
elementName:
        qName
    ;

    // [139]       TypeName       ::=      QName
typeName:
        qName
        {
            $$ = new ASTType(@$, $1, ASTType::ANY);
        }
    ;

    // [140]       URILiteral     ::=      StringLiteral
uriLiteral:
        StringLiteral
    ;

    /* Some auxillary rules start here */

    /* auxillary rule for S? */
optS:
        /* nothing! */
    |   S
    ;

ncName:
        qName
        {
            if ($1->find(":") != std::string::npos)
            {
                error(@$, std::string("NCName is expected, but QName is given: '") + *($1) + "'");
                delete $1;

                $$ = new std::string("dummy");
            }
            else
            {
                $$ = $1;
            }
        }
    ;

qName:
         funcName { $$ = $1; }
    |    ATTRIBUTE { $$ = new std::string("attribute"); }
    |    COMMENT { $$ = new std::string("comment"); }
    |    DOCUMENT_NODE { $$ = new std::string("document-node"); }
    |    ELEMENT { $$ = new std::string("element"); }
    |    EMPTY_SEQUENCE { $$ = new std::string("empty-sequence"); }
    |    IF { $$ = new std::string("if"); }
    |    ITEM { $$ = new std::string("item"); }
    |    NODE { $$ = new std::string("node"); }
    |    PROCESSING_INSTRUCTION { $$ = new std::string("processing-instruction"); }
    |    SCHEMA_ATTRIBUTE { $$ = new std::string("schema-attribute"); }
    |    SCHEMA_ELEMENT { $$ = new std::string("schema-element"); }
    |    TEXT { $$ = new std::string("text"); }
    |    TYPESWITCH { $$ = new std::string("typeswitch"); }
    ;

    /* NOTE:
            1) funcName is introduced to deal with 'xgs:reserved-function-names' constraint gracefully
            2) MODULE and XQUERY are excluded from funcName since they are returned as QNAMEs by lexer;
            3) lexer actually may return MODULE and XQUERY, but only in proper syntactic context: versionDecl, moduleDecl, importModule and Sedna load module expression
    */
funcName:
         QNAME { $$ = $1; }
    |    ANCESTOR { $$ = new std::string("ancestor"); }
    |    ANCESTOR_OR_SELF { $$ = new std::string("ancestor-or-self"); }
    |    AND { $$ = new std::string("and"); }
    |    AS { $$ = new std::string("as"); }
    |    ASCENDING { $$ = new std::string("ascending"); }
    |    AT { $$ = new std::string("at"); }
    |    BASE_URI { $$ = new std::string("base-uri"); }
    |    BOUNDARY_SPACE { $$ = new std::string("boundary-space"); }
    |    BY { $$ = new std::string("by"); }
    |    CASE { $$ = new std::string("case"); }
    |    CAST { $$ = new std::string("cast"); }
    |    CASTABLE { $$ = new std::string("castable"); }
    |    CHILD { $$ = new std::string("child"); }
    |    COLLATION { $$ = new std::string("collation"); }
    |    CONSTRUCTION { $$ = new std::string("construction"); }
    |    COPY_NAMESPACES { $$ = new std::string("copy-namespaces"); }
    |    DECLARE { $$ = new std::string("declare"); }
    |    DEFAULT { $$ = new std::string("default"); }
    |    DESCENDANT { $$ = new std::string("descendant"); }
    |    DESCENDANT_OR_SELF { $$ = new std::string("descendant-or-self"); }
    |    DESCENDING { $$ = new std::string("descending"); }
    |    DIV { $$ = new std::string("div"); }
    |    DOCUMENT { $$ = new std::string("document"); }
    |    ELSE { $$ = new std::string("else"); }
    |    EMPTY { $$ = new std::string("empty"); }
    |    ENCODING { $$ = new std::string("encoding"); }
    |    EQ { $$ = new std::string("eq"); }
    |    EVERY { $$ = new std::string("every"); }
    |    EXCEPT { $$ = new std::string("except"); }
    |    EXTERNAL { $$ = new std::string("external"); }
    |    FOLLOWING { $$ = new std::string("following"); }
    |    FOLLOWING_SIBLING { $$ = new std::string("following-sibling"); }
    |    FOR { $$ = new std::string("for"); }
    |    FUNCTION { $$ = new std::string("function"); }
    |    GE { $$ = new std::string("ge"); }
    |    GREATEST { $$ = new std::string("greatest"); }
    |    GT { $$ = new std::string("gt"); }
    |    IDIV { $$ = new std::string("idiv"); }
    |    IMPORT { $$ = new std::string("import"); }
    |    _IN { $$ = new std::string("in"); }
    |    INHERIT { $$ = new std::string("inherit"); }
    |    INSTANCE { $$ = new std::string("instance"); }
    |    INTERSECT { $$ = new std::string("intersect"); }
    |    IS { $$ = new std::string("is"); }
    |    LAX { $$ = new std::string("lax"); }
    |    LE { $$ = new std::string("le"); }
    |    LEAST { $$ = new std::string("least"); }
    |    LET { $$ = new std::string("let"); }
    |    LT { $$ = new std::string("lt"); }
    |    MOD { $$ = new std::string("mod"); }
    |    NAMESPACE { $$ = new std::string("namespace"); }
    |    NE { $$ = new std::string("ne"); }
    |    NO_INHERIT { $$ = new std::string("no-inherit"); }
    |    NO_PRESERVE { $$ = new std::string("no-preserve"); }
    |    OF { $$ = new std::string("of"); }
    |    OPTION { $$ = new std::string("option"); }
    |    OR { $$ = new std::string("or"); }
    |    ORDER { $$ = new std::string("order"); }
    |    ORDERED { $$ = new std::string("ordered"); }
    |    ORDERING { $$ = new std::string("ordering"); }
    |    PARENT { $$ = new std::string("parent"); }
    |    PRECEDING { $$ = new std::string("preceding"); }
    |    PRECEDING_SIBLING { $$ = new std::string("preceding-sibling"); }
    |    PRESERVE { $$ = new std::string("preserve"); }
    |    RETURN { $$ = new std::string("return"); }
    |    SATISFIES { $$ = new std::string("satisfies"); }
    |    SCHEMA { $$ = new std::string("schema"); }
    |    SELF { $$ = new std::string("self"); }
    |    SOME { $$ = new std::string("some"); }
    |    STABLE { $$ = new std::string("stable"); }
    |    _STRICT { $$ = new std::string("strict"); }
    |    STRIP { $$ = new std::string("strip"); }
    |    THEN { $$ = new std::string("then"); }
    |    TO { $$ = new std::string("to"); }
    |    TREAT { $$ = new std::string("treat"); }
    |    UNION { $$ = new std::string("union"); }
    |    UNORDERED { $$ = new std::string("unordered"); }
    |    VALIDATE { $$ = new std::string("validate"); }
    |    VARIABLE { $$ = new std::string("variable"); }
    |    VERSION { $$ = new std::string("version"); }
    |    WHERE { $$ = new std::string("where"); }
    |    ALL { $$ = new std::string(($1 != 0) ? "ALL" : "all"); }
    |    AFTER { $$ = new std::string(($1 != 0) ? "AFTER" : "after"); }
    |    ALTER { $$ = new std::string(($1 != 0) ? "ALTER" : "alter"); }
    |    AS_ { $$ = new std::string("AS"); }
    |    BEFORE { $$ = new std::string(($1 != 0) ? "BEFORE" : "before"); }
    |    BY_ { $$ = new std::string("BY"); }
    |    COLLECTION { $$ = new std::string(($1 != 0) ? "COLLECTION" : "collection"); }
    |    COLLECTIONS { $$ = new std::string(($1 != 0) ? "COLLECTIONS" : "collections"); }
    |    CREATE { $$ = new std::string(($1 != 0) ? "CREATE" : "create"); }
    |    DATABASE { $$ = new std::string(($1 != 0) ? "DATABASE" : "database"); }
    |    _DELETE { $$ = new std::string(($1 != 0) ? "DELETE" : "delete"); }
    |    DELETE_UNDEEP { $$ = new std::string(($1 != 0) ? "DELETE_UNDEEP" : "delete_undeep"); }
    |    DESCRIPTIVE { $$ = new std::string(($1 != 0) ? "DESCRIPTIVE" : "descriptive"); }
    |    DO { $$ = new std::string(($1 != 0) ? "DO" : "do"); }
    |    DOCUMENT_ { $$ = new std::string("DOCUMENT"); }
    |    DOCUMENTS { $$ = new std::string(($1 != 0) ? "DOCUMENTS" : "documents"); }
    |    DROP { $$ = new std::string(($1 != 0) ? "DROP" : "drop"); }
    |    EACH { $$ = new std::string(($1 != 0) ? "EACH" : "each"); }
    |    EXPLAIN { $$ = new std::string(($1 != 0) ? "EXPLAIN" : "explain"); }
    |    FOLLOWING_ { $$ = new std::string(($1 != 0) ? "FOLLOWING" : "FOLLOWING"); }
    |    FOR_ { $$ = new std::string("FOR"); }
    |    FROM { $$ = new std::string(($1 != 0) ? "FROM" : "from"); }
    |    FULLTEXT { $$ = new std::string(($1 != 0) ? "FULL-TEXT" : "full-text"); }
    |    GRANT { $$ = new std::string(($1 != 0) ? "GRANT" : "grant"); }
    |    IN_ { $$ = new std::string("IN"); }
    |    INDEX { $$ = new std::string(($1 != 0) ? "INDEX" : "index"); }
    |    INTO { $$ = new std::string(($1 != 0) ? "INTO" : "into"); }
    |    INSERT { $$ = new std::string(($1 != 0) ? "INSERT" : "insert"); }
    |    LOAD { $$ = new std::string(($1 != 0) ? "LOAD" : "load"); }
    |    METADATA { $$ = new std::string(($1 != 0) ? "METADATA" : "metadata"); }
    |    MODULE_ { $$ = new std::string("MODULE"); }
    |    MOVE { $$ = new std::string(($1 != 0) ? "MOVE" : "move"); }
    |    NODE_ { $$ = new std::string("NODE"); }
    |    ON { $$ = new std::string(($1 != 0) ? "ON" : "on"); }
    |    OR_ { $$ = new std::string("OR"); }
    |    PASSWORD { $$ = new std::string(($1 != 0) ? "PASSWORD" : "password"); }
    |    PRECEDING_ { $$ = new std::string("preceding"); }
    |    PROFILE { $$ = new std::string(($1 != 0) ? "PROFILE" : "profile"); }
    |    PUBLIC { $$ = new std::string(($1 != 0) ? "PUBLIC" : "public"); }
    |    RENAME { $$ = new std::string(($1 != 0) ? "RENAME" : "rename"); }
    |    REPLACE { $$ = new std::string(($1 != 0) ? "REPLACE" : "replace"); }
    |    RETRIEVE { $$ = new std::string(($1 != 0) ? "RETRIEVE" : "retrieve"); }
    |    REVOKE { $$ = new std::string(($1 != 0) ? "REVOKE" : "revoke"); }
    |    ROLE { $$ = new std::string(($1 != 0) ? "ROLE" : "role"); }
    |    SCHEMA_ { $$ = new std::string("SCHEMA"); }
    |    STATEMENT { $$ = new std::string(($1 != 0) ? "STATEMENT" : "statement"); }
    |    STATISTICS { $$ = new std::string(($1 != 0) ? "STATISTICS" : "statistics"); }
    |    STDIN { $$ = new std::string(($1 != 0) ? "STDIN" : "stdin"); }
    |    TO_ { $$ = new std::string("TO"); }
    |    TRIGGER { $$ = new std::string(($1 != 0) ? "TRIGGER" : "trigger"); }
    |    TYPE { $$ = new std::string(($1 != 0) ? "TYPE" : "type"); }
    |    OPTIONS { $$ = new std::string(($1 != 0) ? "OPTIONS" : "options"); }
    |    UPDATE { $$ = new std::string(($1 != 0) ? "UPDATE" : "update"); }
    |    USER { $$ = new std::string(($1 != 0) ? "USER" : "user"); }
    |    USING { $$ = new std::string(($1 != 0) ? "USING" : "using"); }
    |    WITH { $$ = new std::string(($1 != 0) ? "WITH" : "with"); }
    ;

    /* Sedna update extensions start here
    /* We create rules such _DOCUMENT_ here to accept "document" and "DOCUMENT" tokens
       since outside updates only "document" is allowed as a keyword by XQuery spec */
createExpr:
        CREATE COLLECTION exprSingle
        {
            $$ = new ASTCreateColl(@$, $3);
        }
    |   CREATE _DOCUMENT_ exprSingle
        {
            $$ = new ASTCreateDoc(@$, $3);
        }
    |   CREATE _DOCUMENT_ exprSingle _IN_ COLLECTION exprSingle
        {
            $$ = new ASTCreateDoc(@$, $3, $6);
        }
    |   CREATE ROLE StringLiteral
        {
            $$ = new ASTCreateRole(@$, $3);
        }
    |   CREATE USER StringLiteral WITH PASSWORD StringLiteral
        {
            $$ = new ASTCreateUser(@$, $3, $6);
        }
    |   CREATE INDEX exprSingle ON pathExpr _BY_ pathExpr _AS_ singleType
        {
            $$ = new ASTCreateIndex(@$, $3, $5, $7, static_cast<ASTTypeSingle *>($9), new std::string("btree"));
        }
    |   CREATE INDEX exprSingle ON pathExpr _BY_ pathExpr _AS_ singleType USING StringLiteral
        {
            $$ = new ASTCreateIndex(@$, $3, $5, $7, static_cast<ASTTypeSingle *>($9), $11);
        }
    |   CREATE FULLTEXT INDEX exprSingle ON pathExpr TYPE StringLiteral
        {
            $$ = new ASTCreateFtIndex(@$, $4, $6, $8);
        }
    |   CREATE FULLTEXT INDEX exprSingle ON pathExpr TYPE StringLiteral WITH OPTIONS exprSingle
        {
            $$ = new ASTCreateFtIndex(@$, $4, $6, $8, NULL, $11);
        }
    |   CREATE FULLTEXT INDEX exprSingle ON pathExpr TYPE StringLiteral expr
        {
            $$ = new ASTCreateFtIndex(@$, $4, $6, $8, $9);
        }
    |   CREATE TRIGGER StringLiteral baTrigMod idrTrigMod ON pathExpr _FOR_ EACH nsTrigMod DO LBRACE triggerDoStmts RBRACE
        {
            $$ = new ASTCreateTrg(@$, $3, (ASTCreateTrg::TrgMod)$4, (ASTCreateTrg::TrgMod)$5, $7, (ASTCreateTrg::TrgMod)$10, $13);
        }
    |   RENAME COLLECTION exprSingle INTO exprSingle
        {
            $$ = new ASTRenameColl(@$, $3, $5);
        }
    |   DROP COLLECTION exprSingle
        {
            $$ = new ASTDropColl(@$, $3);
        }
    |   DROP _DOCUMENT_ exprSingle
        {
            $$ = new ASTDropDoc(@$, $3);
        }
    |   DROP _DOCUMENT_ exprSingle _IN_ COLLECTION exprSingle
        {
            $$ = new ASTDropDoc(@$, $3, $6);
        }
    |   DROP ROLE StringLiteral
        {
            $$ = new ASTDropRole(@$, $3);
        }
    |   DROP USER StringLiteral
        {
            $$ = new ASTDropUser(@$, $3);
        }
    |   DROP INDEX exprSingle
        {
            $$ = new ASTDropIndex(@$, $3);
        }
    |   DROP FULLTEXT INDEX exprSingle
        {
            $$ = new ASTDropFtIndex(@$, $4);
        }
    |   DROP TRIGGER StringLiteral
        {
            $$ = new ASTDropTrg(@$, $3);
        }
    |   DROP _MODULE_ StringLiteral
        {
            $$ = new ASTDropMod(@$, $3);
        }
    |   ALTER USER StringLiteral WITH PASSWORD StringLiteral
        {
            $$ = new ASTAlterUser(@$, $3, $6);
        }
    |   GRANT privName ON StringLiteral _TO_ userName
        {
            $$ = new ASTGrantPriv(@$, $2, $4, $6, ASTGrantPriv::DOCUMENT);
        }
    |   GRANT privName ON DATABASE _TO_ userName
        {
            $$ = new ASTGrantPriv(@$, $2, NULL, $6, ASTGrantPriv::DB);
        }
    |   GRANT privName ON COLLECTION StringLiteral _TO_ userName
        {
            $$ = new ASTGrantPriv(@$, $2, $5, $7, ASTGrantPriv::COLLECTION);
        }
    |   GRANT privName ON _DOCUMENT_ StringLiteral _TO_ userName
        {
            $$ = new ASTGrantPriv(@$, $2, $5, $7, ASTGrantPriv::DOCUMENT);
        }
    |   GRANT privName _TO_ userName
        {
            $$ = new ASTGrantRole(@$, $2, $4);
        }
    |   REVOKE privName ON StringLiteral FROM userName
        {
            $$ = new ASTRevokePriv(@$, $2, $4, $6, ASTRevokePriv::DOCUMENT);
        }
    |   REVOKE privName ON DATABASE FROM userName
        {
            $$ = new ASTRevokePriv(@$, $2, NULL, $6, ASTRevokePriv::DB);
        }
    |   REVOKE privName ON COLLECTION StringLiteral FROM userName
        {
            $$ = new ASTRevokePriv(@$, $2, $5, $7, ASTRevokePriv::COLLECTION);
        }
    |   REVOKE privName ON _DOCUMENT_ StringLiteral FROM userName
        {
            $$ = new ASTRevokePriv(@$, $2, $5, $7, ASTRevokePriv::DOCUMENT);
        }
    |   REVOKE privName FROM userName
        {
            $$ = new ASTRevokeRole(@$, $2, $4);
        }
    |   LOAD StringLiteral StringLiteral
        {
            $$ = new ASTLoadFile(@$, $2, $3);
        }
    |   LOAD STDIN StringLiteral
        {
            $$ = new ASTLoadFile(@$, new std::string("/STDIN/"), $3);
        }
    |   LOAD StringLiteral StringLiteral StringLiteral
        {
            $$ = new ASTLoadFile(@$, $2, $3, $4);
        }
    |   LOAD STDIN StringLiteral StringLiteral
        {
            $$ = new ASTLoadFile(@$, new std::string("/STDIN/"), $3, $4);
        }
    |   LOAD _MODULE_ moduleList
        {
            $$ = new ASTLoadModule(@$, $3, ASTLoadModule::LOAD);
        }
    |   LOAD _OR_ REPLACE _MODULE_ moduleList
        {
            $$ = new ASTLoadModule(@$, $5, ASTLoadModule::REPLACE);
        }

    /* error productions */
    |   CREATE _DOCUMENT_ error _IN_ COLLECTION exprSingle { delete $6; $$ = new ASTError(@$); }
    |   CREATE INDEX error ON pathExpr _BY_ pathExpr _AS_ singleType { delete $5; delete $7; delete $9; $$ = new ASTError(@$); }
    |   CREATE INDEX exprSingle ON error _BY_ pathExpr _AS_ singleType { delete $3; delete $7; delete $9; $$ = new ASTError(@$); }
    |   CREATE INDEX error _BY_ pathExpr _AS_ singleType { delete $5; delete $7; $$ = new ASTError(@$); }
    |   CREATE INDEX exprSingle ON pathExpr _BY_ error _AS_ singleType { delete $3; delete $5; delete $9; $$ = new ASTError(@$); }
    |   CREATE INDEX error ON pathExpr _BY_ pathExpr _AS_ singleType USING StringLiteral { delete $5; delete $7; delete $9; delete $11; $$ = new ASTError(@$); }
    |   CREATE INDEX exprSingle ON error _BY_ pathExpr _AS_ singleType USING StringLiteral { delete $3; delete $7; delete $9; delete $11; $$ = new ASTError(@$); }
    |   CREATE INDEX exprSingle ON pathExpr _BY_ error _AS_ singleType USING StringLiteral { delete $3; delete $5; delete $9; delete $11; $$ = new ASTError(@$); }
    |   CREATE FULLTEXT INDEX error ON pathExpr TYPE StringLiteral { delete $6; delete $8; $$ = new ASTError(@$); }
    |   CREATE FULLTEXT INDEX exprSingle ON error TYPE StringLiteral { delete $4; delete $8; $$ = new ASTError(@$); }
    |   CREATE TRIGGER StringLiteral baTrigMod idrTrigMod ON error _FOR_ EACH nsTrigMod DO LBRACE triggerDoStmts RBRACE
        { delete $3; destroyASTNodesVector($13); $$ = new ASTError(@$); }
    |   CREATE TRIGGER StringLiteral baTrigMod idrTrigMod ON pathExpr _FOR_ EACH nsTrigMod DO LBRACE error RBRACE
        { delete $3; delete $7; $$ = new ASTError(@$); }
    |   RENAME COLLECTION error INTO exprSingle { delete $5; $$ = new ASTError(@$); }
    |   DROP _DOCUMENT_ error _IN_ COLLECTION exprSingle { delete $6; $$ = new ASTError(@$); }
    ;

moduleList:
        StringLiteral
        {
            $$ = new ASTStringVector();
            $$->push_back($1);
        }
    |   moduleList COMMA StringLiteral
        {
            $1->push_back($3);
            $$ = $1;
        }
    ;

baTrigMod:
        BEFORE
        {
            $$ = ASTCreateTrg::BEFORE;
        }
    |   AFTER
        {
            $$ = ASTCreateTrg::AFTER;
        }
    ;

idrTrigMod:
        INSERT
        {
            $$ = ASTCreateTrg::INSERT;
        }
    |   _DELETE
        {
            $$ = ASTCreateTrg::DEL;
        }
    |   REPLACE
        {
            $$ = ASTCreateTrg::REPLACE;
        }
    ;

nsTrigMod:
        _NODE_
        {
            $$ = ASTCreateTrg::NODE;
        }
    |   STATEMENT
        {
            $$ = ASTCreateTrg::STATEMENT;
        }
    ;

privName:
        ALL
        {
            $$ = new std::string("ALL");
        }
    |   StringLiteral
        {
            $$ = $1;
        }
    ;

userName:
        PUBLIC
        {
            $$ = new std::string("PUBLIC");
        }
    |   StringLiteral
        {
            $$ = $1;
        }
    ;

triggerDoStmts:
        triggerDoStmt SEMI
        {
            $$ = new ASTNodesVector();
            $$->push_back($1);
        }
    |   triggerDoStmts triggerDoStmt SEMI
        {
            $1->push_back($2);
            $$ = $1;
        }
    ;

triggerDoStmt:
        exprSingle
        {
            $$ = new ASTQuery(@$, $1, ASTQuery::QUERY);
        }
    |   updateExpr
        {
            $$ = new ASTQuery(@$, $1, ASTQuery::UPDATE);
        }
    ;


metadataExpr:
        RETRIEVE METADATA _FOR_ DOCUMENTS WITH STATISTICS
        {
            $$ = new ASTMetaDocs(@$, NULL, true);
        }
    |   RETRIEVE METADATA _FOR_ DOCUMENTS
        {
            $$ = new ASTMetaDocs(@$, NULL, false);
        }
    |   RETRIEVE METADATA _FOR_ DOCUMENTS _IN_ COLLECTION expr
        {
            $$ = new ASTMetaDocs(@$, $7, false);
        }
    |   RETRIEVE METADATA _FOR_ COLLECTIONS WITH STATISTICS
        {
            $$ = new ASTMetaCols(@$, true);
        }
    |   RETRIEVE METADATA _FOR_ COLLECTIONS
        {
            $$ = new ASTMetaCols(@$, false);
        }
    |   RETRIEVE DESCRIPTIVE _SCHEMA_ _FOR_ _DOCUMENT_ expr
        {
            $$ = new ASTMetaSchemaDoc(@$, $6);
        }
    |   RETRIEVE DESCRIPTIVE _SCHEMA_ _FOR_ COLLECTION expr
        {
            $$ = new ASTMetaSchemaCol(@$, $6);
        }
    ;

updateExpr:
        UPDATE insertExpr
        {
            $$ = $2;
        }
    |   UPDATE deleteExpr
        {
            $$ = $2;
        }
    |   UPDATE deleteUndeepExpr
        {
            $$ = $2;
        }
    |   UPDATE replaceExpr
        {
            $$ = $2;
        }
    |   UPDATE renameExpr
        {
            $$ = $2;
        }
    |   UPDATE moveExpr
        {
            $$ = $2;
        }
    ;

insertExpr:
        INSERT expr INTO expr
        {
            $$ = new ASTUpdInsert(@$, $2, $4, ASTUpdInsert::INTO);
        }
    |   INSERT expr _PRECEDING_ expr
        {
            $$ = new ASTUpdInsert(@$, $2, $4, ASTUpdInsert::PRECEDING);
        }
    |   INSERT expr _FOLLOWING_ expr
        {
            $$ = new ASTUpdInsert(@$, $2, $4, ASTUpdInsert::FOLLOWING);
        }

    |   INSERT error INTO expr { delete $4; $$ = new ASTError(@$); }
    |   INSERT error _PRECEDING_ expr { delete $4; $$ = new ASTError(@$); }
    |   INSERT error _FOLLOWING_ expr { delete $4; $$ = new ASTError(@$); }

    ;

deleteExpr:
        _DELETE expr
        {
            $$ = new ASTUpdDel(@$, $2, ASTUpdDel::DEEP);
        }
    ;

deleteUndeepExpr:
        DELETE_UNDEEP expr
        {
            $$ = new ASTUpdDel(@$, $2, ASTUpdDel::UNDEEP);
        }
    ;

replaceExpr:
        REPLACE varRef _IN_ expr WITH expr
        {
            $$ = new ASTUpdReplace(@$, new ASTTypeVar(@2, new ASTTypeSeq(@2, new ASTItemTest(@2), ASTTypeSeq::ZERO_OR_MORE), $2), $4, $6);
        }
    |   REPLACE varRef _AS_ sequenceType _IN_ expr WITH expr
        {
            $$ = new ASTUpdReplace(@$, new ASTTypeVar(@2, $4, $2), $6, $8);
        }
    |   REPLACE varRef _IN_ error WITH expr { delete $2; delete $6; $$ = new ASTError(@$); }
    ;

renameExpr:
        RENAME expr ON qName
        {
            $$ = new ASTUpdRename(@$, $2, $4);
        }
    ;

moveExpr:
        MOVE varRef _AS_ sequenceType _IN_ expr INTO pathExpr
        {
            $$ = new ASTUpdMove(@$, new ASTTypeVar(@2, $4, $2), $6, $8, ASTUpdMove::INTO);
        }
    |   MOVE varRef _AS_ sequenceType _IN_ expr _PRECEDING_ pathExpr
        {
            $$ = new ASTUpdMove(@$, new ASTTypeVar(@2, $4, $2), $6, $8, ASTUpdMove::PRECEDING);
        }
    |   MOVE varRef _AS_ sequenceType _IN_ expr _FOLLOWING_ pathExpr
        {
            $$ = new ASTUpdMove(@$, new ASTTypeVar(@2, $4, $2), $6, $8, ASTUpdMove::FOLLOWING);
        }

    |   MOVE varRef _AS_ sequenceType _IN_ error INTO pathExpr { delete $2; delete $4; delete $8; $$ = new ASTError(@$); }
    |   MOVE varRef _AS_ sequenceType _IN_ error _PRECEDING_ pathExpr { delete $2; delete $4; delete $8; $$ = new ASTError(@$); }
    |   MOVE varRef _AS_ sequenceType _IN_ error _FOLLOWING_ pathExpr { delete $2; delete $4; delete $8; $$ = new ASTError(@$); }
    ;

    /* additional update rules to allow XQuery keywords in upper-case in Sedna update expressions */
_AS_:
        AS
    |   AS_
    ;

_BY_:
        BY
    |   BY_
    ;

_DOCUMENT_:
        DOCUMENT
    |   DOCUMENT_
    ;

_FOLLOWING_:
        FOLLOWING
    |   FOLLOWING_
    ;

_FOR_:
        FOR
    |   FOR_
    ;

_IN_:
        _IN
    |   IN_
    ;

_MODULE_:
        MODULE
    |   MODULE_
    ;

_NODE_:
        NODE
    |   NODE_
    ;

_PRECEDING_:
        PRECEDING
    |   PRECEDING_
    ;

_OR_:
        OR
    |   OR_
    ;

_TO_:
        TO
    |   TO_
    ;

_SCHEMA_:
        SCHEMA
    |   SCHEMA_
    ;

%%

void XQueryParser::error(const sedna::XQueryParser::location_type& l, const std::string& m)
{
    driver.error(l, XPST0003, m.c_str());
}

static void errorc(sedna::XQueryDriver &d, const sedna::XQueryParser::location_type& l, int code)
{
    d.error(l, code, NULL);
}

/* makes an AST tree for quantified expression
   NOTE: 'var_expr' is destroyed during this!!!
*/
static ASTNode *makeQuantExpr(sedna::XQueryParser::location_type& loc, ASTQuantExpr::QuantMod qt, ASTNodesVector *var_expr, ASTNode *sat_expr)
{
    ASTNodesVector::reverse_iterator rit;
    ASTTypeVar *var;
    ASTNode *expr, *res;

    res = sat_expr;
    for (rit = var_expr->rbegin(); rit != var_expr->rend(); rit += 2)
    {
        var = static_cast<ASTTypeVar *>(*(rit + 1));
        expr = *rit;

        res = new ASTQuantExpr(loc, var, expr, res, qt);
    }

    delete var_expr;

    return res;
}

// here we just mark the last step and bind context to the first step
static ASTNode *makePathExpr(ASTNode *xpath, ASTNode *cont)
{
    ASTStep *xp = dynamic_cast<ASTStep *>(xpath);
    ASTStep *fs = NULL, *pfs = NULL;

    U_ASSERT(xp);

    // set flag for the last step
    xp->setAsLast();

    // find the first step and the previous to first
    while (xp)
    {
        // first step is the one without a context
        if (!xp->getContext())
        {
            fs = xp;
            break;
        }

        pfs = xp;
        xp = dynamic_cast<ASTStep *>(xp->getContext());
    }

    // we should have some first step
    U_ASSERT(fs);

    // set context such as fn:root() for leading-slash expressions
    // if first step is an expression without preds then make it usual expression
    if (cont)
    {
        fs->setContext(cont);
    }
    else if (ASTFilterStep *fils = dynamic_cast<ASTFilterStep *>(fs))
    {
        // get rid of ASTFilterStep for first-step primary expressions
        if (!fils->getPreds() && fils->getExpr())
        {
            ASTNode *dup = fils->getExpr()->dup();

            if (pfs)
                pfs->setContext(dup);
            else
                xpath = dup;

            delete fils;
        }
    }

    return xpath;
}

// strips boundary space form element content, merges consecutive character elements
static void ProcessDirectContent(ASTNodesVector *cont, bool isPreserveBS)
{
    unsigned int pos = 0, cpos;
    bool stripBS;
    ASTCharCont *cc, *ncc;

    if (cont == NULL) return;

    while (pos < cont->size())
    {
        cc = dynamic_cast<ASTCharCont *>((*cont)[pos]);

        if (cc)
        {
            // delete empty content, e.g. empty CDATA
            if (*cc->cont == "")
            {
                delete cc;
                cont->erase(cont->begin() + pos);
                continue;
            }

            stripBS = (!isPreserveBS && cc->getOrigin() == ASTCharCont::DIRECT);
            // first, we should merge consecutive char elements
            cpos = pos;
            while (++cpos < cont->size())
            {
                ncc = dynamic_cast<ASTCharCont *>((*cont)[cpos]);

                if (ncc == NULL) break;

                cc->appendContent(ncc);

                // if we encounter CharRef or CDATA, boundary space check is not needed, since CharRef and CDATA never define boundary-spaces
                if (ncc->getOrigin() == ASTCharCont::CDATA || ncc->getOrigin() == ASTCharCont::CREF)
                    stripBS = false;
                    
                delete ncc;
            }

            // delete merged content following pos-node
            cont->erase(cont->begin() + pos + 1, cont->begin() + cpos);

            // then, if we strip boundary spaces, delete current content
            if (stripBS && cc->isSpaceChars())
            {
                delete cc;
                cont->erase(cont->begin() + pos);
            }
        }

        pos++;
    }
}
