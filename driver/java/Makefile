#
# Makefile for driver to Sedna (Java API)
#

PP = ../..


OUTPUT = output
OUTPUT_PATH= output
PACKAGE_PATH= ru/ispras/sedna/driver
JAVAC = javac -classpath $(CLASSPATH) -d output
PACKAGE =ru.ispras.sedna.driver

VPATH = $(PP)/kernel/common/errdbg

include $(PP)/Makefile.include

ifeq ($(PLATFORM),WIN32)
CLASSPATH= ".;output"
else
CLASSPATH= ".:output"
endif
	
all: $(OUTPUT) sednadriver.jar
	@echo ===================================================================
	@echo DRIVER HAS BEEN DONE
	@echo ===================================================================

$(OUTPUT): 
	-$(MKDIR) $(OUTPUT)

sednadriver.jar: ErrorCodes.java \
                 DriverException.java \
                 Debug.java \
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
	jar cvf sednadriver.jar -C $(OUTPUT) ru


Debug.java: Debug.java.m4
	$(M4) -DEL_DEBUG=$(EL_DEBUG) $^ >$@

clean:
	-$(REMOVE) Debug.java
	-$(REMOVE) $(OUTPUT_PATH)/*.class
	-$(REMOVE_DIR) $(OUTPUT)
	-$(REMOVE) $(OUTPUT_PATH)/*.jar
	-$(REMOVE) *.jar

