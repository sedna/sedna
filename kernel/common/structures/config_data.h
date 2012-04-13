#ifndef _CONFIG_DATA_H_
#define _CONFIG_DATA_H_

#include <stdint.h>
#include <string>
#include <iosfwd>
#include <map>

class XMLBuilder;
class XmlNodeReader;
class MessageExchanger;

struct TopLevelAuthentication {
    std::string username;
    std::string password;

    void recvServiceAuth(MessageExchanger * comm);
};

struct CommonClientAuthentication {
    std::string username;
    std::string databaseName;
    std::string password;
    
    void recvInitialAuth(MessageExchanger * comm);
    void recvPassword(MessageExchanger * comm);
};

struct SessionOptions {
    int executionStackDepth;
    int queryTimeout;

    void saveToXml(XMLBuilder * xmlBuilder) const;
    void saveToStream(std::ostream * stream) const;

    XmlNodeReader * createReader();
    void loadFromStream(std::istream * stream);
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

        void saveToXml(XMLBuilder * xmlBuilder) const;
    };

    DatabaseFileSize dataFile;
    DatabaseFileSize tmpFile;

    SessionOptions sessionOptions;
    
    void saveToXml(XMLBuilder * xmlBuilder) const;
    void saveToStream(std::ostream * stream) const;

    XmlNodeReader * createReader();
    void loadFromStream(std::istream * stream);
};

struct SednaOptions {
    std::string dataDirectory;
    std::string bindAddress;

    int listenPort;
    int osObjectsOffset;

    int logLevel;
    
    void saveToXml(XMLBuilder * xmlBuilder) const;
    void saveToStream(std::ostream * stream) const;

    XmlNodeReader * createReader();
    void loadFromStream(std::istream * stream);
};

struct GlobalParameters {
    SednaOptions global;
    DatabaseOptions defaultDatabaseParameters;

    std::map<std::string, DatabaseOptions> databaseOptions;

    void saveToStream(std::ostream * stream) const;
    void loadFromStream(std::istream * stream);
};


#endif /* _CONFIG_DATA_H_ */
