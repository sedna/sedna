#include "aux/options/xml_options.h"
#include "common/structures/config_data.h"

#include <iostream>

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

int main() {
    GlobalParameters globals;

    globals.defaultDatabaseParameters.bufferCount = 1600;
    globals.defaultDatabaseParameters.databaseName = "default";
    globals.defaultDatabaseParameters.updateCriteria = 1.4;

    globals.global.listenPort = 80;
    globals.global.bindAddress = "0.0.0.0";
    
    globals.saveToStream(&std::cout);
};
