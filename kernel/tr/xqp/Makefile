
PP = ../../..


VPATH = . .. ../strings

include $(PP)/Makefile.include

all: libxqp$(LIB_EXT) 
	@echo ==============================================================
	@echo                 XQuery Parser  has been Done
	@echo ==============================================================

test: xqp_test$(EXE_EXT) p$(EXE_EXT)
	@echo ==============================================================
	@echo                 XQuery Parser Test  has been done
	@echo ==============================================================



XQUERYPARSER_LIB = libxqp$(LIB_EXT)

COMMON_FILES_LIB = $(PP)/kernel/common/libcommon_files$(LIB_EXT)



############################################################################
# XQuery Parser
############################################################################

.PHONY: generate_by_antlr generate_by_dlg generate_by_sorcerer


.NOTPARALLEL: $(GENERATED_BY_ANTLR) $(GENERATED_BY_DLG) $(GENERATED_BY_SORCERER)

GENERATED_BY_ANTLR = XQueryparser.dlg XQuerytokens.h XQueryParser.cpp XQuery.main.cpp \
                     XQuery.common.cpp XQuery.constructor.cpp XQuery.xpath.cpp XQuery.seqtype.cpp \
                     XQuery.create.cpp XQuery.metadata.cpp XQuery.update.cpp XQuery.arthLgTypeOps.cpp \
                     XQuery.flwr.cpp XQuery.prolog.cpp XQuery.quantifier.cpp  XQuery.condition.cpp XQueryParser.h

$(GENERATED_BY_ANTLR): XQuery.main.g XQuery.common.g XQuery.constructor.g XQuery.condition.g XQuery.prolog.g \
                       XQuery.xpath.g XQuery.arthLgTypeOps.g XQuery.seqtype.g XQuery.create.g XQuery.metadata.g \
                       XQuery.update.g XQuery.flwr.g XQuery.prolog.g XQuery.quantifier.g XQuery.condition.g \
                       XQuery.lexer.g XQuery.include.g
	$(ANTLR) -w2 -k 2 -e3 -fl XQueryparser.dlg -ft XQuerytokens.h  XQuery.include.g  XQuery.lexer.g \
             XQuery.main.g  XQuery.common.g  XQuery.constructor.g  XQuery.xpath.g XQuery.create.g XQuery.metadata.g \
             XQuery.update.g  XQuery.arthLgTypeOps.g XQuery.condition.g XQuery.flwr.g XQuery.quantifier.g \
             XQuery.prolog.g  XQuery.seqtype.g


GENERATED_BY_DLG = XQueryDLGLexer.cpp XQueryDLGLexer.h

$(GENERATED_BY_DLG): XQueryparser.dlg AST.h
	$(DLG) -cl XQueryDLGLexer XQueryparser.dlg


GENERATED_BY_SORCERER = XQueryTreeParser.cpp XQueryTreeParser.h TXQuery.seqtype.cpp TXQuery.xpath.cpp \
                        TXQuery.constructor.cpp TXQuery.arthLgTypeOps.cpp TXQuery.condition.cpp \
                        TXQuery.create.cpp TXQuery.metadata.cpp TXQuery.update.cpp TXQuery.flwr.cpp \
                        TXQuery.prolog.cpp TXQuery.quantifier.cpp TXQuery.main.cpp

$(GENERATED_BY_SORCERER): TXQuery.seqtype.sor TXQuery.xpath.sor TXQuery.arthLgTypeOps.sor \
                          TXQuery.constructor.sor TXQuery.create.sor TXQuery.metadata.sor \
                          TXQuery.update.sor TXQuery.condition.sor TXQuery.flwr.sor \
                          TXQuery.prolog.sor TXQuery.quantifier.sor TXQuery.main.sor SORAST.h
	$(SORCERER) -transform  TXQuery.main.sor TXQuery.xpath.sor TXQuery.arthLgTypeOps.sor \
                TXQuery.constructor.sor TXQuery.create.sor TXQuery.metadata.sor TXQuery.update.sor \
                TXQuery.condition.sor TXQuery.flwr.sor TXQuery.prolog.sor TXQuery.quantifier.sor \
                TXQuery.seqtype.sor


AST$(OBJ_EXT) context$(OBJ_EXT) flwr$(OBJ_EXT) parserutils$(OBJ_EXT) print$(OBJ_EXT) \
quantifier$(OBJ_EXT) XQuerytoLR$(OBJ_EXT): $(GENERATED_BY_ANTLR) $(GENERATED_BY_DLG) $(GENERATED_BY_SORCERER)


$(XQUERYPARSER_LIB):     XQueryDLGLexer$(OBJ_EXT) \
                         XQueryParser$(OBJ_EXT) \
                         XQuery.main$(OBJ_EXT) \
                         XQuery.common$(OBJ_EXT) \
                         XQuery.constructor$(OBJ_EXT) \
                         XQuery.xpath$(OBJ_EXT) \
                         XQuery.seqtype$(OBJ_EXT) \
                         XQuery.create$(OBJ_EXT) \
                         XQuery.metadata$(OBJ_EXT) \
                         XQuery.update$(OBJ_EXT) \
                         XQuery.arthLgTypeOps$(OBJ_EXT) \
                         XQuery.flwr$(OBJ_EXT) \
                         XQuery.prolog$(OBJ_EXT) \
                         XQuery.quantifier$(OBJ_EXT) \
                         XQuery.condition$(OBJ_EXT) \
                         AST$(OBJ_EXT) \
                         context$(OBJ_EXT) \
                         XQueryTreeParser$(OBJ_EXT) \
                         TXQuery.seqtype$(OBJ_EXT) \
                         TXQuery.arthLgTypeOps$(OBJ_EXT) \
                         TXQuery.xpath$(OBJ_EXT) \
                         TXQuery.constructor$(OBJ_EXT) \
                         TXQuery.condition$(OBJ_EXT) \
                         TXQuery.create$(OBJ_EXT) \
                         TXQuery.metadata$(OBJ_EXT) \
                         TXQuery.update$(OBJ_EXT) \
                         TXQuery.flwr$(OBJ_EXT) \
                         TXQuery.prolog$(OBJ_EXT) \
                         TXQuery.quantifier$(OBJ_EXT) \
                         TXQuery.main$(OBJ_EXT) \
                         XQuerytoLR$(OBJ_EXT)  \
                         print$(OBJ_EXT)  \
                         quantifier$(OBJ_EXT) \
                         flwr$(OBJ_EXT) \
                         parserutils$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^	

############################################################################
# XQuery Parser Test
############################################################################		

xqp_test$(EXE_EXT):      $(XQUERYPARSER_LIB) \
                         $(COMMON_FILES_LIB) \
                         xqp_test$(OBJ_EXT)
	$(LD) $(LFLAGS) $(LDOUT)$@ -L. -L$(PP)/kernel/common $^ -lxqp $(LSLIBS) -lcommon_files

p$(EXE_EXT): p$(OBJ_EXT)
	$(LD) $(LFLAGS) $(LDOUT)$@ p$(OBJ_EXT)



############################################################################
# Clean
############################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(XQUERYPARSER_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) xqp_test$(EXE_EXT)
	-$(REMOVE) XQuery.arithmOps.cpp 
	-$(REMOVE) XQuery.common.cpp 
	-$(REMOVE) XQuery.compare.cpp 
	-$(REMOVE) XQuery.condition.cpp 
	-$(REMOVE) XQuery.constructor.cpp 
	-$(REMOVE) XQuery.flwr.cpp 
	-$(REMOVE) XQuery.logicalOps.cpp 
	-$(REMOVE) XQuery.main.cpp 
	-$(REMOVE) XQuery.quantifier.cpp 
	-$(REMOVE) XQuery.seqtype.cpp 
	-$(REMOVE) XQuery.update.cpp
	-$(REMOVE) XQuery.create.cpp
	-$(REMOVE) XQuery.metadata.cpp
	-$(REMOVE) XQuery.condition.cpp 
	-$(REMOVE) XQuery.flwr.cpp
	-$(REMOVE) XQuery.arthLgTypeOps.cpp
	-$(REMOVE) XQuery.prolog.cpp
	-$(REMOVE) XQuery.xpath.cpp
	-$(REMOVE) XQueryDLGLexer.cpp 
	-$(REMOVE) XQueryDLGLexer.h 
	-$(REMOVE) XQueryParser.cpp 
	-$(REMOVE) XQueryParser.h 
	-$(REMOVE) XQueryparser.dlg 
	-$(REMOVE) XQuerytokens.h
	-$(REMOVE) XQueryTreeParser.cpp
	-$(REMOVE) XQueryTreeParser.h
	-$(REMOVE) TXQuery.main.cpp
	-$(REMOVE) TXQuery.xpath.cpp
	-$(REMOVE) TXQuery.seqtype.cpp
	-$(REMOVE) TXQuery.constructor.cpp
	-$(REMOVE) TXQuery.create.cpp
	-$(REMOVE) TXQuery.metadata.cpp
	-$(REMOVE) TXQuery.update.cpp
	-$(REMOVE) TXQuery.condition.cpp
	-$(REMOVE) TXQuery.flwr.cpp
	-$(REMOVE) TXQuery.prolog.cpp
	-$(REMOVE) TXQuery.arthLgTypeOps.cpp
	-$(REMOVE) TXQuery.quantifier.cpp
	-$(REMOVE) xqp_test.suo
	-$(REMOVE) xqp_test.sln
