#define TEST_NAME "0003.xmlOptionsTest.xml"

#include "aux/counted_ptr.h"
#include "aux/options/xml_options.h"
#include "common/structures/config_data.h"
#include "gtest/gtest.h"

#include <iostream>


static const char * normalOptions =
  "<sednaOptions><global><dataDirectory>/var/sedna</dataDirectory><listenPort>5050</listenPort><bindAddress>0.0.0.0</bindAddress><logLevel>2</logLevel><stackDepth>4000</stackDepth><keepAlive>0</keepAlive></global>"
  "<databaseDefaults><bufferCount>1600</bufferCount><maxLogFiles>20</maxLogFiles><updateCriteria>0.9</updateCriteria><securityOptions>0</securityOptions><dataFileSize><initial><![CDATA[104857600]]></initial><max><![CDATA[1048576000]]></max><extension><![CDATA[104857600]]></extension></dataFileSize><tmpFileSize><initial><![CDATA[104857600]]></initial><max><![CDATA[1048576000]]></max><extension><![CDATA[104857600]]></extension></tmpFileSize><sessionOptions><executionStackDepth><![CDATA[54]]></executionStackDepth><queryTimeout><![CDATA[65535]]></queryTimeout></sessionOptions></databaseDefaults></sednaOptions>";

static const char * errorOptions1 =
  "<sednaOptions><global><dataDirectory>/var/sedna</dataDirectory><listenPort>50gf50</listenPort><bindAddress>0.0.0.0</bindAddress><logLevel>UNKNOWN</logLevel><stackDepth>4000</stackDepth><keepAlive>0</keepAlive></global>"
  "<databaseDefaults><bufferCount>1600</bufferCount><maxLogFiles>20</maxLogFiles><updateCriteria>0.9</updateCriteria><securityOptions>0</securityOptions><dataFileSize><initial><![CDATA[104857600]]></initial><max><![CDATA[1048576000]]></max><extension><![CDATA[104857600]]></extension></dataFileSize><tmpFileSize><initial><![CDATA[104857600]]></initial><max><![CDATA[1048576000]]></max><extension><![CDATA[104857600]]></extension></tmpFileSize><sessionOptions><executionStackDepth><![CDATA[54]]></executionStackDepth><queryTimeout><![CDATA[65535]]></queryTimeout></sessionOptions></databaseDefaults></sednaOptions>";

static const char * errorOptions2 =
  "<sednaOptions><global><dataDirectory2>/var/sedna</dataDirectory2><listenPort>50gf50</listenPort><bindAddress>0.0.0.0</bindAddress><logLevel>UNKNOWN</logLevel><stackDepth>4000</stackDepth><keepAlive>0</keepAlive></global>"
  "<databaseDefaults><bufferCount>1600</bufferCount><maxLogFiles>20</maxLogFiles><updateCriteria>0.9</updateCriteria><securityOptions>0</securityOptions><dataFileSize><initial><![CDATA[104857600]]></initial><max><![CDATA[1048576000]]></max><extension><![CDATA[104857600]]></extension></dataFileSize><tmpFileSize><initial><![CDATA[104857600]]></initial><max><![CDATA[1048576000]]></max><extension><![CDATA[104857600]]></extension></tmpFileSize><sessionOptions><executionStackDepth><![CDATA[54]]></executionStackDepth><queryTimeout><![CDATA[65535]]></queryTimeout></sessionOptions></databaseDefaults></sednaOptions>";
  
/*
int readGlobalParameters(std::istream * stream) {
  
};

int readDatabaseOptions(const std::string& name, std::istream * options) {
  
};

int saveGlobalParameters(std::ostream * stream) {
  
};

int saveDatabaseOptions(const std::string& name, std::istream * options) {

};
*/

void setDefaultDatabaseOptions(DatabaseOptions * databaseOptions)
{
    databaseOptions->bufferCount = 1600;
    databaseOptions->databaseName = "default";
    databaseOptions->updateCriteria = 1.4;
    databaseOptions->maxLogFiles = 20;
    databaseOptions->securityOptions = 0;
    databaseOptions->dataFile.initial = 100*1024*1024;
    databaseOptions->dataFile.extension = 100*1024*1024;
    databaseOptions->dataFile.max = 100*1024*1024*10;
    databaseOptions->tmpFile = databaseOptions->dataFile;
};

void setDefaultSednaOptions(SednaOptions * sednaOptions)
{
    sednaOptions->bindAddress = "0.0.0.0";
    sednaOptions->dataDirectory = "/var/sedna";
    sednaOptions->listenPort = 5050;
    sednaOptions->logLevel = 2;
    sednaOptions->stackDepth = 4000;
    sednaOptions->keepAlive = 0;
};

TEST(globalOptionsReader, correctOptions) {
    GlobalParameters globals;

    setDefaultDatabaseOptions(&globals.defaultDatabaseParameters);
    setDefaultSednaOptions(&globals.global);
    try {
        globals.loadFromStream(scoped_ptr<std::stringstream>(new std::stringstream(normalOptions)).get());
        globals.saveToStream(&std::cout);
    } catch (std::exception & x) {
        std::cout << x.what() << "\n";
        ASSERT_TRUE(false);
        return;
    };
    
    ASSERT_TRUE(true);
    std::cout << "\n";
}

TEST(globalOptionsReader, errorOptions1) {
    GlobalParameters globals;

    setDefaultDatabaseOptions(&globals.defaultDatabaseParameters);
    setDefaultSednaOptions(&globals.global);
    try {
        globals.loadFromStream(scoped_ptr<std::stringstream>(new std::stringstream(errorOptions1)).get());
        globals.saveToStream(&std::cout);
    } catch (std::exception & x) {
        std::cout << x.what() << "\n";
        ASSERT_TRUE(true);
        return;
    };
    
    ASSERT_TRUE(false);
    std::cout << "\n";
}

TEST(globalOptionsReader, errorOptions2) {
    GlobalParameters globals;

    setDefaultDatabaseOptions(&globals.defaultDatabaseParameters);
    setDefaultSednaOptions(&globals.global);
    try {
        globals.loadFromStream(scoped_ptr<std::stringstream>(new std::stringstream(errorOptions2)).get());
        globals.saveToStream(&std::cout);
    } catch (std::exception & x) {
        std::cout << x.what() << "\n";
        ASSERT_TRUE(true);
        return;
    };
    
    ASSERT_TRUE(false);
    std::cout << "\n";
}

int main(int argc, char** argv) {

    if(argc == 1) {
        std::string cmd;
#ifdef WIN32
        cmd.append("xml:reports\\");
#else
        cmd.append("xml:reports/");
#endif   
        cmd.append(TEST_NAME);
         ::testing::GTEST_FLAG(output) = cmd.c_str();
    }
    ::testing::InitGoogleTest(&argc, argv);
    
    return RUN_ALL_TESTS();
};
