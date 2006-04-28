/*
 * File:  print.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <stdio.h>
#include "d_printf.h"

void print_ast_type(int type)
{ 
/*
  switch(type)
  {
    case 0:  d_printf1("NULL"); break
    case 1:  d_printf1("AST_QUERY"); break;
    case 2:  d_printf1("AST_FUNCDEFS"); break;
    case 3:  d_printf1("AST_SEQUENCE"); break;
    case 4:  d_printf1("AST_OR"); break;
    case 5:  d_printf1("AST_AND"); break;
    case 6:  d_printf1("AST_FLWR"); break;
    case 7:  d_printf1("AST_FORLETS"); break;
    case 8:  d_printf1("AST_FOR"); break;
    case 9:  d_printf1("AST_LET"); break;
    case 10: d_printf1("AST_WHERE"); break;
    case 11: d_printf1("AST_RETURN"); break;
    case 12: d_printf1("AST_RPE"); break;
    case 13: d_printf1("AST_VAR"); break;
    case 14: d_printf1("AST_TEST"); break;
    case 15: d_printf1("AST_CHILD"); break;
    case 16: d_printf1("AST_CONTEXT"); break;
    case 17: d_printf1("AST_ATTRIBUTE"); break;
    case 18: d_printf1("AST_QNAME"); break;
    case 19: d_printf1("AST_WILDCARD"); break;
    case 20: d_printf1("AST_TEXT"); break;
    case 21: d_printf1("AST_NODE"); break;
    case 22: d_printf1("AST_FCALL"); break;
    case 23: d_printf1("AST_CONST"); break;
    case 24: d_printf1("AST_SOME"); break;
    case 25: d_printf1("AST_EVERY"); break;
    case 26: d_printf1("AST_IF"); break;
    case 27: d_printf1("AST_B_OP"); break;
    case 28: d_printf1("AST_UNARY_OP"); break;
    case 29: d_printf1("AST_PREDICATES"); break;
    case 30: d_printf1("AST_ELEMENT"); break;
    case 31: d_printf1("AST_ELEMENT_NAME"); break;
    case 32: d_printf1("AST_ELEMENT_ATRIBUTES"); break;
    case 33: d_printf1("AST_CONTENT"); break;
    case 34: d_printf1("AST_ATTRIBUTE_NAME"); break;
    case 35: d_printf1("AST_FUNCTION"); break;
    case 36: d_printf1("AST_FPARAMS"); break;
    case 37: d_printf1("AST_FBODY"); break;
    case 38: d_printf1("AST_UNKNOWN_TYPE"); break;
    case 39: d_printf1("AST_FUNC"); break;
    case 40: d_printf1("AST_EXPR"); break;
    case 41: d_printf1("AST_SLASH"); break;
    case 42: d_printf1("AST_SLASH_SLASH"); break;
    case 43: d_printf1("AST_AXIS_PATH_STEP"); break;
    case 44: d_printf1("AST_FORWARD_STEP"); break;
    case 45: d_printf1("AST_AXIS"); break;
    case 46: d_printf1("AST_CHILD_AXIS"); break;
    case 47: d_printf1("AST_ATTRIBUTE_AXIS"); break;
    case 48: d_printf1("AST_DESCENDANT_AXIS"); break;
    case 49: d_printf1("AST_PARENT_AXIS"); break;
    case 50: d_printf1("AST_SELF_AXIS"); break;
    case 51: d_printf1("AST_DESCENDANT_OR_SELF_AXIS"); break;
    case 52: d_printf1("AST_FOLLOWING_SIBLING_AXIS"); break;
    case 53: d_printf1("AST_FOLLOWING_AXIS"); break;
    case 54: d_printf1("AST_ANCESTOR_AXIS"); break;
    case 55: d_printf1("AST_PRECEDING_SIBLING_AXIS"); break;
    case 56: d_printf1("AST_PRECEDING_AXIS"); break;
    case 57: d_printf1("AST_ANCESTOR_OR_SELF_AXIS"); break;
    case 58: d_printf1("AST_REVERSE_STEP"); break;
    case 59:  break;
    case 60:  break;
    case 61:  break;
    case 62:  break;
    case 63:  break;
    case 64:  break;
    case 65:  break;
    case 66:  break;
    case 67:  break;
    case 68:  break;
    case 69:  break;
    case 70:  break;
    case 71:  break;
    case 72:  break;
    case 73:  break;
    case 74:  break;
    case 75:  break;
    case 76:  break;
    case 77:  break;
    case 78:  break;
    case 79:  break;
    case 80:  break;
    case 81:  break;
    case 82:  break;
    case 83:  break;
    case 84:  break;
    case 85:  break;
    case 86:  break;
    case 87:  break;
    case 88:  break;
    case 89:  break;
    case 90:  break;
    case 91:  break;
    case 92:  break;
    case 93:  break;
    case 94:  break;
    case 95:  break;
    case 96:  break;
    case 97:  break;
    case 98:  break;
    case 99:  break;
    case 100:  break;
    case 101:  break;
    case 102:  break;
    case 103:  break;
    case 104:  break;
    case 105:  break;
    case 106:  break;
    case 107:  break;
    case 108:  break;
    case 109:  break;
    case 110:  break;
    case 111:  break;
    case 112:  break;
    case 113:  break;
    case 114:  break;
    case 115:  break;
    case 116:  break;
    case 117:  break;
    case 118:  break;
    case 119:  break;
    case 120:  break;
    case 121:  break;
    case 122:  break;
    case 123:  break;
    case 124:  break;

  }
*/

  if(type==0) d_printf1("NULL");
  else
  if(type==1) d_printf1("AST_QUERY");
  else
  if(type==2) d_printf1("AST_FUNCDEFS");
  else
  if(type==3) d_printf1("AST_SEQUENCE");
  else
  if(type==4) d_printf1("AST_OR");
  else
  if(type==5) d_printf1("AST_AND");
  else
  if(type==6) d_printf1("AST_FLWR");
  else
  if(type==7) d_printf1("AST_FORLETS");
  else
  if(type==8) d_printf1("AST_FOR");
  else
  if(type==9) d_printf1("AST_LET");
  else
  if(type==10) d_printf1("AST_WHERE");
  else
  if(type==11) d_printf1("AST_RETURN");
  else
  if(type==12) d_printf1("AST_RPE");
  else
  if(type==13) d_printf1("AST_VAR");
  else
  if(type==14) d_printf1("AST_TEST");
  else
  if(type==15) d_printf1("AST_CHILD");
  else
  if(type==16) d_printf1("AST_CONTEXT");
  else
  if(type==17) d_printf1("AST_ATTRIBUTE");
  else
  if(type==18) d_printf1("AST_QNAME");
  else
  if(type==19) d_printf1("AST_WILDCARD");
  else
  if(type==20) d_printf1("AST_TEXT");
  else
  if(type==21) d_printf1("AST_NODE");
  else
  if(type==22) d_printf1("AST_FCALL");
  else
  if(type==23) d_printf1("AST_CONST");
  else
  if(type==24) d_printf1("AST_SOME");
  else
  if(type==25) d_printf1("AST_EVERY");
  else
  if(type==26) d_printf1("AST_IF");
  else
  if(type==27) d_printf1("AST_B_OP");
  else
  if(type==28) d_printf1("AST_UNARY_OP");
  else
  if(type==29) d_printf1("AST_PREDICATES");
  else
  if(type==30) d_printf1("AST_ELEMENT");
  else
  if(type==31) d_printf1("AST_ELEMENT_NAME");
  else
  if(type==32) d_printf1("AST_ELEMENT_ATRIBUTES");
  else
  if(type==33) d_printf1("AST_CONTENT");
  else
  if(type==34) d_printf1("AST_ATTRIBUTE_NAME");
  else
  if(type==35) d_printf1("AST_FUNCTION");
  else
  if(type==36) d_printf1("AST_FPARAMS");
  else
  if(type==37) d_printf1("AST_FBODY");
  else
  if(type==38) d_printf1("AST_UNKNOWN_TYPE");
  else
  if(type==39) d_printf1("AST_FUNC");
  else
  if(type==40) d_printf1("AST_EXPR");
  else
  if(type==41) d_printf1("AST_SLASH");
  else
  if(type==42) d_printf1("AST_SLASH_SLASH");
  else
  if(type==43) d_printf1("AST_AXIS_PATH_STEP");
  else
  if(type==44) d_printf1("AST_FORWARD_STEP");
  else
  if(type==45) d_printf1("AST_AXIS");
  else
  if(type==46) d_printf1("AST_CHILD_AXIS");
  else
  if(type==47) d_printf1("AST_ATTRIBUTE_AXIS");
  else
  if(type==48) d_printf1("AST_DESCENDANT_AXIS");
  else
  if(type==49) d_printf1("AST_PARENT_AXIS");
  else
  if(type==50) d_printf1("AST_SELF_AXIS");
  else
  if(type==51) d_printf1("AST_DESCENDANT_OR_SELF_AXIS");
  else
  if(type==52) d_printf1("AST_FOLLOWING_SIBLING_AXIS");
  else
  if(type==53) d_printf1("AST_FOLLOWING_AXIS");
  else
  if(type==54) d_printf1("AST_ANCESTOR_AXIS");
  else
  if(type==55) d_printf1("AST_PRECEDING_SIBLING_AXIS");
  else
  if(type==56) d_printf1("AST_PRECEDING_AXIS");
  else
  if(type==57) d_printf1("AST_ANCESTOR_OR_SELF_AXIS");
  else
  if(type==58) d_printf1("AST_REVERSE_STEP");
  else
  if(type==59) d_printf1("AST_FILTER_PATH_STEP");
  else 
  if(type==60) d_printf1("AST_BOUND");
  else
  if(type==61) d_printf1("AST_CHAR_SEQ");
  else
  if(type==62) d_printf1("AST_PREFIX");
  else
  if(type==63) d_printf1("AST_LOCAL_NAME");
  else
  if(type==64) d_printf1("AST_CONTEXT_ITEM");
  else
  if(type==65) d_printf1("AST_RELATIVE_PATH");
  else
  if(type==66) d_printf1("AST_STEP");
  else
  if(type==67) d_printf1("AST_TYPE");
  else
  if(type==68) d_printf1("AST_EMPTY");
  else
  if(type==69) d_printf1("AST_MULTIPLICITY");
  else
  if(type==70) d_printf1("AST_ITEM_TEST");
  else
  if(type==71) d_printf1("AST_ATOMIC");
  else
  if(type==72) d_printf1("AST_COMMENT_TEST");
  else
  if(type==73) d_printf1("AST_TEXT_TEST");
  else
  if(type==74) d_printf1("AST_NODE_TEST");
  else
  if(type==75) d_printf1("AST_DOCUMENT_TEST");
  else
  if(type==76) d_printf1("AST_ELEMENT_TEST");
  else
  if(type==77) d_printf1("AST_NIL");
  else
  if(type==78) d_printf1("AST_ATTRIBUTE_TEST");
  else
  if(type==79) d_printf1("AST_TYPE_NAME");
  else
  if(type==80) d_printf1("AST_SCHEMA_CONTEXT_PATH");
  else
  if(type==81) d_printf1("AST_GLOBAL_NAME");
  else
  if(type==82) d_printf1("AST_CONTEXT_NAME");
  else
  if(type==83) d_printf1("AST_GLOBAL_TYPE");
  else
  if(type==84) d_printf1("AST_INTEGER_CONST");
  else
  if(type==85) d_printf1("AST_DECIMAL_CONST");
  else
  if(type==86) d_printf1("AST_DOUBLE_CONST");
  else
  if(type==87) d_printf1("AST_STRING_CONST");
  else
  if(type==88) d_printf1("AST_INSTANCE_OF");
  else
  if(type==89) d_printf1("AST_TREAT");
  else
  if(type==90) d_printf1("AST_CASTABLE");
  else
  if(type==91) d_printf1("AST_CAST");
  else
  if(type==92) d_printf1("AST_UPDATE");
  else
  if(type==93) d_printf1("AST_INTO");
  else
  if(type==94) d_printf1("AST_PRECEDING");
  else
  if(type==95) d_printf1("AST_FOLLOWING");
  else
  if(type==96) d_printf1("AST_INSERT");
  else
  if(type==97) d_printf1("AST_DELETE");
  else
  if(type==98) d_printf1("AST_DELETE_UNDEEP");
  else
  if(type==99) d_printf1("AST_REPLACE");
  else
  if(type==100) d_printf1("AST_RENAME");
  else
  if(type==101) d_printf1("AST_MOVE");
  else
  if(type==102) d_printf1("AST_STABLE_ORDER");
  else
  if(type==103) d_printf1("AST_ORDER_BY");
  else
  if(type==104) d_printf1("AST_ORDER_SPEC");
  else
  if(type==105) d_printf1("AST_ORDER");
  else
  if(type==106) d_printf1("AST_ASC_DESC");
  else
  if(type==107) d_printf1("AST_EMPT_GR_LST");
  else
  if(type==108) d_printf1("AST_COLLATION");
  else
  if(type==109) d_printf1("AST_ORDER_PROPERTY");
  else
  if(type==110) d_printf1("AST_RETURNED_TYPE");
  else
  if(type==111) d_printf1("AST_EXTERNAL_FUNCTION");
  else
  if(type==112) d_printf1("AST_PARAM");
  else
  if(type==113) d_printf1("AST_CREATE_DOCUMENT");
  else
  if(type==114) d_printf1("AST_DROP_DOCUMENT");
  else
  if(type==115) d_printf1("AST_CREATE_COLLECTION");
  else
  if(type==116) d_printf1("AST_DROP_COLLECTION");
  else
  if(type==117) d_printf1("AST_EMPTY_SEQUENCE");
  else
  if(type==118) d_printf1("AST_LOAD_FILE");
/*  else
  if(type==119) d_printf1("AST_RETRIEVE_DOCS_IN_COLL");
  else
  if(type==120) d_printf1("AST_RETRIEVE_DOC_SCHEME");
  else
  if(type==121) d_printf1("AST_RETRIEVE_COLL_SCHEME");
  else
  if(type==122) d_printf1("AST_RETRIEVE_COLLS");
  else
  if(type==123) d_printf1("AST_CREATE_DOCUMENT_IN_COLL");
  else
  if(type==124) d_printf1("AST_DROP_DOCUMENT_IN_COLL");
*/

  else d_printf1("ERROR");
  
}

