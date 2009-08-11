#
# Makefile for driver to Sedna (Java API)
#

PP = ../..

OUTPUT = out
SRC_PATH = src/ru/ispras/sedna/driver
DOC_PATH = dist/javadoc
JAR_PATH = dist/lib
ERR_PATH = $(PP)/kernel/common/errdbg

VPATH = $(SRC_PATH)

JAVAC   = javac -d $(OUTPUT)
JAVADOC = javadoc -d $(DOC_PATH)

include $(PP)/Makefile.include


all: $(OUTPUT) $(JAR_PATH)/sednadriver.jar $(DOC_PATH)/index.html
	@echo ===================================================================
	@echo Java Driver Done
	@echo ===================================================================

$(OUTPUT): 
	-$(MKDIR) $(OUTPUT)
	-$(MKDIR) $(JAR_PATH)

$(ERR_PATH)/ErrorCodes.java: $(ERR_PATH)/gen_error_codes.c $(ERR_PATH)/error.codes
	NO_DEP=1; export NO_DEP; $(MAKE) -C $(ERR_PATH) generated

$(JAR_PATH)/sednadriver.jar: $(ERR_PATH)/ErrorCodes.java \
                             DriverException.java \
                             $(SRC_PATH)/Debug.java \
                             ResultType.java \
                             NetOps.java \
                             SednaSerializedResult.java \
                             SednaSerializedResultImpl.java \
                             SednaStatement.java \
                             SednaStatementImpl.java \
                             SednaConnection.java \
                             SednaConnectionImpl.java \
                             DatabaseManager.java
	$(JAVAC) $^
	jar cvf $(JAR_PATH)/sednadriver.jar -C $(OUTPUT) ru

$(DOC_PATH)/index.html: $(ERR_PATH)/ErrorCodes.java \
                        DriverException.java \
                        $(SRC_PATH)/Debug.java \
                        ResultType.java \
                        NetOps.java \
                        SednaSerializedResult.java \
                        SednaSerializedResultImpl.java \
                        SednaStatement.java \
                        SednaStatementImpl.java \
                        SednaConnection.java \
                        SednaConnectionImpl.java \
                        DatabaseManager.java
	$(JAVADOC) $^

$(SRC_PATH)/Debug.java: $(SRC_PATH)/Debug.java.m4
	$(M4) -DEL_DEBUG=$(EL_DEBUG) $^ >$@

clean:
	-$(REMOVE) $(SRC_PATH)/Debug.java
	-$(REMOVE) $(OUTPUT)/*.class
	-$(REMOVE_DIR) $(OUTPUT)
	-$(REMOVE) -rf $(DOC_PATH)
	-$(REMOVE) -rf $(JAR_PATH)
