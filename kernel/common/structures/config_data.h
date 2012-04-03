#ifndef _CONFIG_DATA_H_
#define _CONFIG_DATA_H_

#include <stdint.h>
#include <string>
#include <iosfwd>
#include <map>

class XmlBuilder;

struct DatabaseOptions {
    std::string databaseName;

    uint32_t bufferCount;
    uint32_t maxLogFiles;

    double updateCriteria;
    int securityOptions;

    struct DatabaseFileSize {
        uint32_t max;
        uint32_t initial;
        uint32_t extension;
    };

    DatabaseFileSize dataFile;
    DatabaseFileSize tmpFile;
};

struct SednaOptions {
    std::string dataDirectory;
    std::string bindAddress;

    int listenPort;
    int osObjectsOffset;
};

struct GlobalParameters {
    SednaOptions global;
    DatabaseOptions defaultDatabaseParameters;
    std::map<std::string, DatabaseOptions> databaseOptions;
};

void saveSednaOptions(SednaOptions * options, XmlBuilder * xmlBuilder);
void saveSednaOptions(SednaOptions * options, std::ostream * stream);

void saveDatabaseOptions(DatabaseOptions * options, XmlBuilder * xmlBuilder);
void saveDatabaseOptions(DatabaseOptions * options, std::ostream * stream);

void saveGlobalParameters(GlobalParameters * options, XmlBuilder * xmlBuilder);
void saveGlobalParameters(GlobalParameters * options, std::ostream * stream);


#endif /* _CONFIG_DATA_H_ */
