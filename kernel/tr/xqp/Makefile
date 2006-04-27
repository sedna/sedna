
PP = ../../..


VPATH = . ..

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


XQueryparser.dlg XQuerytokens.h XQueryParser.cpp XQuery.main.cpp \
XQuery.common.cpp XQuery.constructor.cpp XQuery.xpath.cpp XQuery.seqtype.cpp \
XQuery.create.cpp XQuery.metadata.cpp XQuery.update.cpp XQuery.arthLgTypeOps.cpp \
XQuery.flwr.cpp XQuery.prolog.cpp XQuery.quantifier.cpp  XQuery.condition.cpp XQueryParser.h: generate_by_antlr

generate_by_antlr: XQuery.main.g  XQuery.common.g  XQuery.constructor.g  XQuery.condition.g  XQuery.prolog.g \
                   XQuery.xpath.g  XQuery.arthLgTypeOps.g  XQuery.seqtype.g XQuery.create.g XQuery.metadata.g \
                   XQuery.update.g XQuery.flwr.g XQuery.prolog.g XQuery.quantifier.g  XQuery.condition.g \
                   XQuery.lexer.g  XQuery.include.g  AST.h ASTNodeTypes.h  $(PP)/kernel/common/errdbg/exceptions.h
	$(ANTLR) -w2 -k 2 -e3 -fl XQueryparser.dlg -ft XQuerytokens.h  XQuery.include.g  XQuery.lexer.g  XQuery.main.g  XQuery.common.g  XQuery.constructor.g  XQuery.xpath.g XQuery.create.g XQuery.metadata.g  XQuery.update.g  XQuery.arthLgTypeOps.g XQuery.condition.g XQuery.flwr.g XQuery.quantifier.g  XQuery.prolog.g  XQuery.seqtype.g


XQueryDLGLexer.cpp XQueryDLGLexer.h: generate_by_dlg

generate_by_dlg: XQueryparser.dlg AST.h
	$(DLG) -cl XQueryDLGLexer XQueryparser.dlg

XQueryTreeParser.cpp XQueryTreeParser.h TXQuery.seqtype.cpp TXQuery.xpath.cpp \
TXQuery.constructor.cpp TXQuery.arthLgTypeOps.cpp TXQuery.condition.cpp \
TXQuery.create.cpp TXQuery.metadata.cpp TXQuery.update.cpp TXQuery.flwr.cpp \
TXQuery.prolog.cpp TXQuery.quantifier.cpp TXQuery.main.cpp: generate_by_sorcerer

generate_by_sorcerer: TXQuery.seqtype.sor TXQuery.xpath.sor TXQuery.arthLgTypeOps.sor \
                      TXQuery.constructor.sor TXQuery.create.sor TXQuery.metadata.sor \
                      TXQuery.update.sor TXQuery.condition.sor TXQuery.flwr.sor \
                      TXQuery.prolog.sor TXQuery.quantifier.sor TXQuery.main.sor SORAST.h \
                      AST.h ASTNodeTypes.h context.h
	$(SORCERER) -transform  TXQuery.main.sor TXQuery.xpath.sor TXQuery.arthLgTypeOps.sor TXQuery.constructor.sor TXQuery.create.sor TXQuery.metadata.sor TXQuery.update.sor TXQuery.condition.sor TXQuery.flwr.sor TXQuery.prolog.sor TXQuery.quantifier.sor TXQuery.seqtype.sor



TXQuery.arthLgTypeOps$(OBJ_EXT): TXQuery.arthLgTypeOps.cpp \
  ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  ../../libs/pg/h/pccts_stdlib.h XQueryTreeParser.h AST.h \
  XQuerytokens.h ../../libs/pg/h/ATokPtr.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ANTLRToken.h \
  ../../libs/pg/h/ASTBase.h ASTNodeTypes.h SORAST.h \
  context.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.condition$(OBJ_EXT): TXQuery.condition.cpp \
  ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  ../../libs/pg/h/pccts_stdlib.h XQueryTreeParser.h AST.h \
  XQuerytokens.h ../../libs/pg/h/ATokPtr.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ANTLRToken.h \
  ../../libs/pg/h/ASTBase.h ASTNodeTypes.h SORAST.h \
  context.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.constructor$(OBJ_EXT): TXQuery.constructor.cpp \
  ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  ../../libs/pg/h/pccts_stdlib.h XQueryTreeParser.h AST.h \
  XQuerytokens.h ../../libs/pg/h/ATokPtr.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ANTLRToken.h \
  ../../libs/pg/h/ASTBase.h ASTNodeTypes.h SORAST.h \
  context.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.create$(OBJ_EXT): TXQuery.create.cpp ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h \
  ../../libs/pg/h/PCCTSAST.h ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  XQueryTreeParser.h AST.h XQuerytokens.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ANTLRToken.h ../../libs/pg/h/ASTBase.h \
  ASTNodeTypes.h SORAST.h context.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/common/utils.h \
  ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.flwr$(OBJ_EXT): TXQuery.flwr.cpp ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h \
  ../../libs/pg/h/PCCTSAST.h ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  XQueryTreeParser.h AST.h XQuerytokens.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ANTLRToken.h ../../libs/pg/h/ASTBase.h \
  ASTNodeTypes.h SORAST.h context.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/common/utils.h \
  ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.main$(OBJ_EXT): TXQuery.main.cpp ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h \
  ../../libs/pg/h/PCCTSAST.h ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  XQueryTreeParser.h AST.h XQuerytokens.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ANTLRToken.h ../../libs/pg/h/ASTBase.h \
  ASTNodeTypes.h SORAST.h context.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/common/utils.h \
  ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.metadata$(OBJ_EXT): TXQuery.metadata.cpp \
  ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  ../../libs/pg/h/pccts_stdlib.h XQueryTreeParser.h AST.h \
  XQuerytokens.h ../../libs/pg/h/ATokPtr.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ANTLRToken.h \
  ../../libs/pg/h/ASTBase.h ASTNodeTypes.h SORAST.h \
  context.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.prolog$(OBJ_EXT): TXQuery.prolog.cpp ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h \
  ../../libs/pg/h/PCCTSAST.h ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  XQueryTreeParser.h AST.h XQuerytokens.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ANTLRToken.h ../../libs/pg/h/ASTBase.h \
  ASTNodeTypes.h SORAST.h context.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/common/utils.h \
  ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.quantifier$(OBJ_EXT): TXQuery.quantifier.cpp \
  ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  ../../libs/pg/h/pccts_stdlib.h XQueryTreeParser.h AST.h \
  XQuerytokens.h ../../libs/pg/h/ATokPtr.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ANTLRToken.h \
  ../../libs/pg/h/ASTBase.h ASTNodeTypes.h SORAST.h \
  context.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.seqtype$(OBJ_EXT): TXQuery.seqtype.cpp \
  ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  ../../libs/pg/h/pccts_stdlib.h XQueryTreeParser.h AST.h \
  XQuerytokens.h ../../libs/pg/h/ATokPtr.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ANTLRToken.h \
  ../../libs/pg/h/ASTBase.h ASTNodeTypes.h SORAST.h \
  context.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.update$(OBJ_EXT): TXQuery.update.cpp ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h \
  ../../libs/pg/h/PCCTSAST.h ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  XQueryTreeParser.h AST.h XQuerytokens.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ANTLRToken.h ../../libs/pg/h/ASTBase.h \
  ASTNodeTypes.h SORAST.h context.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/common/utils.h \
  ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

TXQuery.xpath$(OBJ_EXT): TXQuery.xpath.cpp ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h \
  ../../libs/pg/h/PCCTSAST.h ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  XQueryTreeParser.h AST.h XQuerytokens.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ANTLRToken.h ../../libs/pg/h/ASTBase.h \
  ASTNodeTypes.h SORAST.h context.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/common/utils.h \
  ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h

XQuery.arthLgTypeOps$(OBJ_EXT): XQuery.arthLgTypeOps.cpp \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  ../../libs/pg/h/pccts_stdlib.h ../../libs/pg/h/pccts_stdio.h \
  XQuerytokens.h ANTLRToken.h XQueryDLGLexer.h \
  ../../libs/pg/h/DLexerBase.h ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.common$(OBJ_EXT): XQuery.common.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  ANTLRToken.h XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.condition$(OBJ_EXT): XQuery.condition.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  ANTLRToken.h XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.constructor$(OBJ_EXT): XQuery.constructor.cpp \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  ../../libs/pg/h/pccts_stdlib.h ../../libs/pg/h/pccts_stdio.h \
  XQuerytokens.h ANTLRToken.h XQueryDLGLexer.h \
  ../../libs/pg/h/DLexerBase.h ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.create$(OBJ_EXT): XQuery.create.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  ANTLRToken.h XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.flwr$(OBJ_EXT): XQuery.flwr.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  ANTLRToken.h XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.main$(OBJ_EXT): XQuery.main.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  ANTLRToken.h XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.metadata$(OBJ_EXT): XQuery.metadata.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  ANTLRToken.h XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.prolog$(OBJ_EXT): XQuery.prolog.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  ANTLRToken.h XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.quantifier$(OBJ_EXT): XQuery.quantifier.cpp \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  ../../libs/pg/h/pccts_stdlib.h ../../libs/pg/h/pccts_stdio.h \
  XQuerytokens.h ANTLRToken.h XQueryDLGLexer.h \
  ../../libs/pg/h/DLexerBase.h ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.seqtype$(OBJ_EXT): XQuery.seqtype.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  ANTLRToken.h XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.update$(OBJ_EXT): XQuery.update.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  ANTLRToken.h XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQuery.xpath$(OBJ_EXT): XQuery.xpath.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  ANTLRToken.h XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ../../libs/pg/h/ATokenStream.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pccts_setjmp.h ../../libs/pg/h/ATokenBuffer.h \
  XQueryParser.h

XQueryDLGLexer$(OBJ_EXT): XQueryDLGLexer.cpp XQuerytokens.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ../../libs/pg/h/ASTBase.h \
  ../../libs/pg/h/PCCTSAST.h ANTLRToken.h XQueryDLGLexer.h \
  ../../libs/pg/h/DLexerBase.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ATokenStream.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h \
  ../../kernel/common/u/u.h ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ASTNodeTypes.h flwr.h \
  quantifier.h parserutils.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  types.h ../../libs/pg/h/AParser.h ../../libs/pg/h/pccts_setjmp.h \
  ../../libs/pg/h/ATokenBuffer.h ../../libs/pg/h/DLexer.h

XQueryParser$(OBJ_EXT): XQueryParser.cpp ../../libs/pg/h/pcctscfg.h \
  ../../libs/pg/h/pccts_stdio.h ../../libs/pg/h/pccts_stdlib.h \
  ../../libs/pg/h/pccts_stdio.h XQuerytokens.h \
  XQueryParser.h ../../libs/pg/h/AParser.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_setjmp.h \
  ../../libs/pg/h/AToken.h ../../libs/pg/h/pccts_string.h \
  ../../libs/pg/h/pccts_stdarg.h ../../libs/pg/h/ATokenBuffer.h \
  ../../libs/pg/h/ATokenStream.h ANTLRToken.h \
  XQueryDLGLexer.h ../../libs/pg/h/DLexerBase.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/utils.h ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h AST.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/ASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ASTNodeTypes.h flwr.h quantifier.h \
  parserutils.h 

XQueryTreeParser$(OBJ_EXT): XQueryTreeParser.cpp XQueryTreeParser.h \
  ../../libs/pg/h/STreeParser.h \
  ../../libs/pg/h/SASTBase.h ../../libs/pg/h/PCCTSAST.h \
  ../../libs/pg/h/pcctscfg.h ../../libs/pg/h/pccts_stdio.h \
  ../../libs/pg/h/pccts_stdlib.h AST.h XQuerytokens.h \
  ../../libs/pg/h/ATokPtr.h ../../libs/pg/h/AToken.h \
  ../../libs/pg/h/pccts_string.h ../../libs/pg/h/pccts_stdarg.h \
  ANTLRToken.h ../../libs/pg/h/ASTBase.h \
  ASTNodeTypes.h SORAST.h context.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/common/utils.h \
  ../../kernel/common/u/u.h \
  \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../driver/c/sp_defs.h ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/u/uutils.h \
  ../../kernel/common/u/u.h


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
