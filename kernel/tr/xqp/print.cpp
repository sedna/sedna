/*
 * File:  print.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <stdio.h>

void print_ast_type(int type)
{ 
/*
  switch(type)
  {
    case 0:  printf("NULL"); break
    case 1:  printf("AST_QUERY"); break;
    case 2:  printf("AST_FUNCDEFS"); break;
    case 3:  printf("AST_SEQUENCE"); break;
    case 4:  printf("AST_OR"); break;
    case 5:  printf("AST_AND"); break;
    case 6:  printf("AST_FLWR"); break;
    case 7:  printf("AST_FORLETS"); break;
    case 8:  printf("AST_FOR"); break;
    case 9:  printf("AST_LET"); break;
    case 10: printf("AST_WHERE"); break;
    case 11: printf("AST_RETURN"); break;
    case 12: printf("AST_RPE"); break;
    case 13: printf("AST_VAR"); break;
    case 14: printf("AST_TEST"); break;
    case 15: printf("AST_CHILD"); break;
    case 16: printf("AST_CONTEXT"); break;
    case 17: printf("AST_ATTRIBUTE"); break;
    case 18: printf("AST_QNAME"); break;
    case 19: printf("AST_WILDCARD"); break;
    case 20: printf("AST_TEXT"); break;
    case 21: printf("AST_NODE"); break;
    case 22: printf("AST_FCALL"); break;
    case 23: printf("AST_CONST"); break;
    case 24: printf("AST_SOME"); break;
    case 25: printf("AST_EVERY"); break;
    case 26: printf("AST_IF"); break;
    case 27: printf("AST_B_OP"); break;
    case 28: printf("AST_UNARY_OP"); break;
    case 29: printf("AST_PREDICATES"); break;
    case 30: printf("AST_ELEMENT"); break;
    case 31: printf("AST_ELEMENT_NAME"); break;
    case 32: printf("AST_ELEMENT_ATRIBUTES"); break;
    case 33: printf("AST_CONTENT"); break;
    case 34: printf("AST_ATTRIBUTE_NAME"); break;
    case 35: printf("AST_FUNCTION"); break;
    case 36: printf("AST_FPARAMS"); break;
    case 37: printf("AST_FBODY"); break;
    case 38: printf("AST_UNKNOWN_TYPE"); break;
    case 39: printf("AST_FUNC"); break;
    case 40: printf("AST_EXPR"); break;
    case 41: printf("AST_SLASH"); break;
    case 42: printf("AST_SLASH_SLASH"); break;
    case 43: printf("AST_AXIS_PATH_STEP"); break;
    case 44: printf("AST_FORWARD_STEP"); break;
    case 45: printf("AST_AXIS"); break;
    case 46: printf("AST_CHILD_AXIS"); break;
    case 47: printf("AST_ATTRIBUTE_AXIS"); break;
    case 48: printf("AST_DESCENDANT_AXIS"); break;
    case 49: printf("AST_PARENT_AXIS"); break;
    case 50: printf("AST_SELF_AXIS"); break;
    case 51: printf("AST_DESCENDANT_OR_SELF_AXIS"); break;
    case 52: printf("AST_FOLLOWING_SIBLING_AXIS"); break;
    case 53: printf("AST_FOLLOWING_AXIS"); break;
    case 54: printf("AST_ANCESTOR_AXIS"); break;
    case 55: printf("AST_PRECEDING_SIBLING_AXIS"); break;
    case 56: printf("AST_PRECEDING_AXIS"); break;
    case 57: printf("AST_ANCESTOR_OR_SELF_AXIS"); break;
    case 58: printf("AST_REVERSE_STEP"); break;
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

  if(type==0) printf("NULL");
  else
  if(type==1) printf("AST_QUERY");
  else
  if(type==2) printf("AST_FUNCDEFS");
  else
  if(type==3) printf("AST_SEQUENCE");
  else
  if(type==4) printf("AST_OR");
  else
  if(type==5) printf("AST_AND");
  else
  if(type==6) printf("AST_FLWR");
  else
  if(type==7) printf("AST_FORLETS");
  else
  if(type==8) printf("AST_FOR");
  else
  if(type==9) printf("AST_LET");
  else
  if(type==10) printf("AST_WHERE");
  else
  if(type==11) printf("AST_RETURN");
  else
  if(type==12) printf("AST_RPE");
  else
  if(type==13) printf("AST_VAR");
  else
  if(type==14) printf("AST_TEST");
  else
  if(type==15) printf("AST_CHILD");
  else
  if(type==16) printf("AST_CONTEXT");
  else
  if(type==17) printf("AST_ATTRIBUTE");
  else
  if(type==18) printf("AST_QNAME");
  else
  if(type==19) printf("AST_WILDCARD");
  else
  if(type==20) printf("AST_TEXT");
  else
  if(type==21) printf("AST_NODE");
  else
  if(type==22) printf("AST_FCALL");
  else
  if(type==23) printf("AST_CONST");
  else
  if(type==24) printf("AST_SOME");
  else
  if(type==25) printf("AST_EVERY");
  else
  if(type==26) printf("AST_IF");
  else
  if(type==27) printf("AST_B_OP");
  else
  if(type==28) printf("AST_UNARY_OP");
  else
  if(type==29) printf("AST_PREDICATES");
  else
  if(type==30) printf("AST_ELEMENT");
  else
  if(type==31) printf("AST_ELEMENT_NAME");
  else
  if(type==32) printf("AST_ELEMENT_ATRIBUTES");
  else
  if(type==33) printf("AST_CONTENT");
  else
  if(type==34) printf("AST_ATTRIBUTE_NAME");
  else
  if(type==35) printf("AST_FUNCTION");
  else
  if(type==36) printf("AST_FPARAMS");
  else
  if(type==37) printf("AST_FBODY");
  else
  if(type==38) printf("AST_UNKNOWN_TYPE");
  else
  if(type==39) printf("AST_FUNC");
  else
  if(type==40) printf("AST_EXPR");
  else
  if(type==41) printf("AST_SLASH");
  else
  if(type==42) printf("AST_SLASH_SLASH");
  else
  if(type==43) printf("AST_AXIS_PATH_STEP");
  else
  if(type==44) printf("AST_FORWARD_STEP");
  else
  if(type==45) printf("AST_AXIS");
  else
  if(type==46) printf("AST_CHILD_AXIS");
  else
  if(type==47) printf("AST_ATTRIBUTE_AXIS");
  else
  if(type==48) printf("AST_DESCENDANT_AXIS");
  else
  if(type==49) printf("AST_PARENT_AXIS");
  else
  if(type==50) printf("AST_SELF_AXIS");
  else
  if(type==51) printf("AST_DESCENDANT_OR_SELF_AXIS");
  else
  if(type==52) printf("AST_FOLLOWING_SIBLING_AXIS");
  else
  if(type==53) printf("AST_FOLLOWING_AXIS");
  else
  if(type==54) printf("AST_ANCESTOR_AXIS");
  else
  if(type==55) printf("AST_PRECEDING_SIBLING_AXIS");
  else
  if(type==56) printf("AST_PRECEDING_AXIS");
  else
  if(type==57) printf("AST_ANCESTOR_OR_SELF_AXIS");
  else
  if(type==58) printf("AST_REVERSE_STEP");
  else
  if(type==59) printf("AST_FILTER_PATH_STEP");
  else 
  if(type==60) printf("AST_BOUND");
  else
  if(type==61) printf("AST_CHAR_SEQ");
  else
  if(type==62) printf("AST_PREFIX");
  else
  if(type==63) printf("AST_LOCAL_NAME");
  else
  if(type==64) printf("AST_CONTEXT_ITEM");
  else
  if(type==65) printf("AST_RELATIVE_PATH");
  else
  if(type==66) printf("AST_STEP");
  else
  if(type==67) printf("AST_TYPE");
  else
  if(type==68) printf("AST_EMPTY");
  else
  if(type==69) printf("AST_MULTIPLICITY");
  else
  if(type==70) printf("AST_ITEM_TEST");
  else
  if(type==71) printf("AST_ATOMIC");
  else
  if(type==72) printf("AST_COMMENT_TEST");
  else
  if(type==73) printf("AST_TEXT_TEST");
  else
  if(type==74) printf("AST_NODE_TEST");
  else
  if(type==75) printf("AST_DOCUMENT_TEST");
  else
  if(type==76) printf("AST_ELEMENT_TEST");
  else
  if(type==77) printf("AST_NIL");
  else
  if(type==78) printf("AST_ATTRIBUTE_TEST");
  else
  if(type==79) printf("AST_TYPE_NAME");
  else
  if(type==80) printf("AST_SCHEMA_CONTEXT_PATH");
  else
  if(type==81) printf("AST_GLOBAL_NAME");
  else
  if(type==82) printf("AST_CONTEXT_NAME");
  else
  if(type==83) printf("AST_GLOBAL_TYPE");
  else
  if(type==84) printf("AST_INTEGER_CONST");
  else
  if(type==85) printf("AST_DECIMAL_CONST");
  else
  if(type==86) printf("AST_DOUBLE_CONST");
  else
  if(type==87) printf("AST_STRING_CONST");
  else
  if(type==88) printf("AST_INSTANCE_OF");
  else
  if(type==89) printf("AST_TREAT");
  else
  if(type==90) printf("AST_CASTABLE");
  else
  if(type==91) printf("AST_CAST");
  else
  if(type==92) printf("AST_UPDATE");
  else
  if(type==93) printf("AST_INTO");
  else
  if(type==94) printf("AST_PRECEDING");
  else
  if(type==95) printf("AST_FOLLOWING");
  else
  if(type==96) printf("AST_INSERT");
  else
  if(type==97) printf("AST_DELETE");
  else
  if(type==98) printf("AST_DELETE_UNDEEP");
  else
  if(type==99) printf("AST_REPLACE");
  else
  if(type==100) printf("AST_RENAME");
  else
  if(type==101) printf("AST_MOVE");
  else
  if(type==102) printf("AST_STABLE_ORDER");
  else
  if(type==103) printf("AST_ORDER_BY");
  else
  if(type==104) printf("AST_ORDER_SPEC");
  else
  if(type==105) printf("AST_ORDER");
  else
  if(type==106) printf("AST_ASC_DESC");
  else
  if(type==107) printf("AST_EMPT_GR_LST");
  else
  if(type==108) printf("AST_COLLATION");
  else
  if(type==109) printf("AST_ORDER_PROPERTY");
  else
  if(type==110) printf("AST_RETURNED_TYPE");
  else
  if(type==111) printf("AST_EXTERNAL_FUNCTION");
  else
  if(type==112) printf("AST_PARAM");
  else
  if(type==113) printf("AST_CREATE_DOCUMENT");
  else
  if(type==114) printf("AST_DROP_DOCUMENT");
  else
  if(type==115) printf("AST_CREATE_COLLECTION");
  else
  if(type==116) printf("AST_DROP_COLLECTION");
  else
  if(type==117) printf("AST_EMPTY_SEQUENCE");
  else
  if(type==118) printf("AST_LOAD_FILE");
/*  else
  if(type==119) printf("AST_RETRIEVE_DOCS_IN_COLL");
  else
  if(type==120) printf("AST_RETRIEVE_DOC_SCHEME");
  else
  if(type==121) printf("AST_RETRIEVE_COLL_SCHEME");
  else
  if(type==122) printf("AST_RETRIEVE_COLLS");
  else
  if(type==123) printf("AST_CREATE_DOCUMENT_IN_COLL");
  else
  if(type==124) printf("AST_DROP_DOCUMENT_IN_COLL");
*/

  else printf("ERROR");
  
}

