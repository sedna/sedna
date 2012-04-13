#ifndef _CONFIG_DATA_H_
#define _CONFIG_DATA_H_

#include <stdint.h>
#include <string>
#include <iosfwd>
#include <map>

class MessageExchanger;
class XmlBuilder;

struct TopLevelAuthentication {
    std::string username;
    std::string password;
    std::string ticket;
};

struct CommonClientAuthentication {
    std::string username;
    std::string databaseName;
    std::string password;
    
    void recvInitialAuth(MessageExchanger * comm);
    void recvPassword(MessageExchanger * comm);
    void recvServiceAuth(MessageExchanger * comm);
};

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
    
    void saveToXml(XmlBuilder * xmlBuilder);
    void saveToStream(std::ostream * stream);
};

struct SednaOptions {
    std::string dataDirectory;
    std::string bindAddress;

    int listenPort;
    int osObjectsOffset;
    
    void saveToXml(XmlBuilder * xmlBuilder);
    void saveToStream(std::ostream * stream);
};

struct GlobalParameters {
    SednaOptions global;
    DatabaseOptions defaultDatabaseParameters;
    std::map<std::string, DatabaseOptions> databaseOptions;

    void saveToXml(XmlBuilder * xmlBuilder);
    void saveToStream(std::ostream * stream);
};




#endif /* _CONFIG_DATA_H_ */
