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

sednadriver.jar: DriverException.java \
                 Debug.java \
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


#$(OUTPUT_PATH)/$(PACKAGE_PATH)/DriverException.class: DriverException.java Debug.java
#	$(JAVAC) $<

#$(OUTPUT_PATH)/$(PACKAGE_PATH)/Debug.class: Debug.java
#	$(JAVAC) $<

#$(OUTPUT_PATH)/$(PACKAGE_PATH)/NetOps.class: NetOps.java Debug.java
#	$(JAVAC) $<

#$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaSerializedResult.class: SednaSerializedResult.java Debug.java
#	$(JAVAC) $<

#$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaSerializedResultImpl.class: SednaSerializedResultImpl.java Debug.java
#	$(JAVAC) $<

#$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaStatement.class: SednaStatement.java Debug.java
#	$(JAVAC) $<

#$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaStatementImpl.class: SednaStatementImpl.java Debug.java
#	$(JAVAC) $<

#$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaConnection.class: SednaConnection.java Debug.java
#	$(JAVAC) $<

#$(OUTPUT_PATH)/$(PACKAGE_PATH)/SednaConnectionImpl.class: SednaConnectionImpl.java Debug.java
#	$(JAVAC) $<

#$(OUTPUT_PATH)/$(PACKAGE_PATH)/DatabaseManager.class: DatabaseManager.java Debug.java
#	$(JAVAC) $<

Debug.java: Debug.java.m4
	$(M4) -DEL_DEBUG=$(EL_DEBUG) $^ >$@

clean:
	-$(REMOVE) Debug.java
	-$(REMOVE) $(OUTPUT_PATH)/*.class
	-$(REMOVE_DIR) $(OUTPUT)
	-$(REMOVE) $(OUTPUT_PATH)/*.jar
	-$(REMOVE) *.jar

