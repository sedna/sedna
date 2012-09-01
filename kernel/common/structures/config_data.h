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
    XmlNodeReader * createReader();
};

struct DatabaseOptions {
    int32_t databaseId;
    
    std::string databaseName;
    std::string dataFilePath; /* Path to database files */
//    std::string tmpFilePath; /* Path to temporary files */

    uint32_t bufferCount;
    uint32_t maxLogFiles;
    uint32_t logFileSize;

    double updateCriteria;
    int securityOptions;

    int sessionPoolSize;
    uint32_t layerSize;
    bool autoStart;

    struct DatabaseFileSize {
        uint32_t max;
        uint32_t initial;
        uint32_t extension;

        void saveToXml(XMLBuilder * xmlBuilder) const;
    };

    DatabaseFileSize dataFileSize;
    DatabaseFileSize tmpFileSize;

    SessionOptions sessionOptions;

    std::string dataFileName;
    std::string tmpFileName;

    void saveToXml(XMLBuilder * xmlBuilder) const;
    XmlNodeReader * createReader();

    void saveToStream(std::ostream * stream) const;
    void loadFromStream(std::istream * stream);

    DatabaseOptions();
};

struct SednaOptions {
    std::string dataDirectory;
    std::string bindAddress;

    int listenPort;
    int stackDepth;
    int logLevel;
    int keepAlive;

    void saveToXml(XMLBuilder * xmlBuilder) const;
    XmlNodeReader * createReader();

    SednaOptions();
};

struct GlobalParameters {
    SednaOptions global;
    DatabaseOptions defaultDatabaseParameters;

    typedef std::map<std::string, DatabaseOptions> DatabaseOptionMap;

    DatabaseOptionMap databaseOptions;

    void saveDatabaseToStream(const std::string &dbname, std::ostream * stream) const;
    void loadDatabaseFromStream(const std::string &dbname, std::istream * stream);

    void saveToStream(std::ostream * stream) const;
    void loadFromStream(std::istream * stream);
};

#endif /* _CONFIG_DATA_H_ */
