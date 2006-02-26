#
# Makefile for driver to Sedna (Java API)
#

PP = ../..


OUTPUT = output
OUTPUT_PATH= output
PACKAGE_PATH= ru/ispras/sedna/driver
JAVAC = javac -classpath $(CLASSPATH) -d output
PACKAGE =ru.ispras.sedna.driver

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


sednadriver.jar: $(OUTPUT_PATH)/$(PACKAGE_PATH)/DriverException.class \
            $(OUTPUT_PATH)/$(PACKAGE_PATH)/Debug.class \
            $(OUTPUT_PATH)/$(PACKAGE_PATH)/NetOps.class\
            $(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaSerializedResult.class\
            $(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaSerializedResultImpl.class\
            $(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaStatement.class \
            $(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaStatementImpl.class \
            $(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaConnection.class\
            $(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaConnectionImpl.class\
            $(OUTPUT_PATH)/$(PACKAGE_PATH)/DatabaseManager.class
	jar cvf sednadriver.jar -C $(OUTPUT) ru

$(OUTPUT_PATH)/$(PACKAGE_PATH)/DriverException.class: DriverException.java
	$(JAVAC) $<

$(OUTPUT_PATH)/$(PACKAGE_PATH)/Debug.class: Debug.java
	$(JAVAC) $<

$(OUTPUT_PATH)/$(PACKAGE_PATH)/NetOps.class: NetOps.java
	$(JAVAC) $<

$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaSerializedResult.class: SednaSerializedResult.java
	$(JAVAC) $<

$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaSerializedResultImpl.class: SednaSerializedResultImpl.java
	$(JAVAC) $<

$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaStatement.class: SednaStatement.java
	$(JAVAC) $<

$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaStatementImpl.class: SednaStatementImpl.java 
	$(JAVAC) $<

$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaConnection.class: SednaConnection.java
	$(JAVAC) $<

$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaConnectionImpl.class: SednaConnectionImpl.java 
	$(JAVAC) $<

$(OUTPUT_PATH)/$(PACKAGE_PATH)/DatabaseManager.class: DatabaseManager.java
	$(JAVAC) $<

Debug.java: Debug.java.m4
	$(M4) -DEL_DEBUG=$(EL_DEBUG) $^ >$@

clean:
	-$(REMOVE) Debug.java
	-$(REMOVE) $(OUTPUT_PATH)/*.class
	-$(REMOVE_DIR) $(OUTPUT)
	-$(REMOVE) $(OUTPUT_PATH)/*.jar
	-$(REMOVE) *.jar

