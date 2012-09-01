/* General database options structure */
/* However move this to config parameters */

#include "config_data.h"

#include "auxiliary/cppcast.h"
#include "auxiliary/options/xml_options.h"

#include "common/socketutils/socketutils.h"

#include <sstream>

/* NOTE: this options are set in the only place: in gov_globals.cpp */
/* WARNING: Ilya, check please options datafilesize and tmpfilesize. I'm not sure about values */
void GlobalParameters::setDefaultOptions() {
    global.bindAddress     = "0.0.0.0";
    global.dataDirectory   = "/var/sedna";
    global.listenPort      = 5050;
    global.logLevel        = 2;
    global.stackDepth      = 4000;
    global.keepAlive       = 0;
    defaultDatabaseParameters.autoStart = true;
    defaultDatabaseParameters.bufferCount = 1600;
    defaultDatabaseParameters.databaseId = -1;
    defaultDatabaseParameters.databaseName = "";
    defaultDatabaseParameters.dataFileName = "";
    defaultDatabaseParameters.dataFileSize.max = INT_MAX;
    defaultDatabaseParameters.dataFileSize.initial = 100;
    defaultDatabaseParameters.dataFileSize.extension = 100;
    defaultDatabaseParameters.layerSize = -1;
    defaultDatabaseParameters.logFileSize = 100;
    defaultDatabaseParameters.maxLogFiles = 3;
    defaultDatabaseParameters.securityOptions = 1;
    defaultDatabaseParameters.sessionOptions.queryTimeout = 0;
    defaultDatabaseParameters.sessionOptions.executionStackDepth = global.stackDepth;
    defaultDatabaseParameters.sessionPoolSize = 50;
    defaultDatabaseParameters.tmpFileName = "";
    defaultDatabaseParameters.tmpFileSize.max = 1000;
    defaultDatabaseParameters.tmpFileSize.initial = 1000;
    defaultDatabaseParameters.tmpFileSize.extension = 1000;
    defaultDatabaseParameters.updateCriteria = 0.25;
}

void CommonClientAuthentication::recvInitialAuth(MessageExchanger* comm)
{
     comm->readString(username, SE_MAX_LOGIN_LENGTH);
     comm->readString(databaseName, SE_MAX_DB_NAME_LENGTH);
}

void CommonClientAuthentication::recvPassword(MessageExchanger* comm)
{
     comm->readString(password, SE_MAX_PASSWORD_LENGTH);
}

void TopLevelAuthentication::recvServiceAuth(MessageExchanger* comm)
{
     comm->readString(username, SE_MAX_LOGIN_LENGTH);
     comm->readString(password, SE_MAX_PASSWORD_LENGTH);
}

#define EXPANDR(OPT) #OPT, &options->OPT 
#define EXPANDW(OPT) #OPT, OPT
#define EXPANDWC(OPT) #OPT, cast_to_string(OPT)

struct SessionOptionsXmlReader : public XmlNodeReader {
    SessionOptionsXmlReader(SessionOptions* options) {
        this->readIntValue(EXPANDR(executionStackDepth));
        this->readIntValue(EXPANDR(queryTimeout));
    }
};

XmlNodeReader* SessionOptions::createReader()
{
    return new SessionOptionsXmlReader(this);
}

void SessionOptions::saveToXml(XMLBuilder* xmlBuilder) const
{
    xmlBuilder->addElement(EXPANDWC(executionStackDepth));
    xmlBuilder->addElement(EXPANDWC(queryTimeout));
}

struct DatabaseFileSizeXmlReader : public XmlNodeReader {
    DatabaseFileSizeXmlReader(DatabaseOptions::DatabaseFileSize* options) {
        this->readUintValue(EXPANDR(initial));
        this->readUintValue(EXPANDR(max));
        this->readUintValue(EXPANDR(extension));
    };
};

void DatabaseOptions::DatabaseFileSize::saveToXml(XMLBuilder* xmlBuilder) const
{
    xmlBuilder->addElement(EXPANDWC(initial));
    xmlBuilder->addElement(EXPANDWC(max));
    xmlBuilder->addElement(EXPANDWC(extension));
}

struct DatabaseOptionsXmlReader : public XmlNodeReader {
    DatabaseFileSizeXmlReader dataFileSizeReader, tmpFileSizeReader;
    SessionOptionsXmlReader sessionReader;

    DatabaseOptionsXmlReader(DatabaseOptions* options) 
      : dataFileSizeReader(&options->dataFileSize),
        tmpFileSizeReader(&options->tmpFileSize),
        sessionReader(&options->sessionOptions)
    {
//         this->readIntValue(EXPANDR(databaseId));
        this->readStringValue(EXPANDR(databaseName));
        this->readStringValue(EXPANDR(dataFilePath));
        this->readUintValue(EXPANDR(bufferCount));
        this->readUintValue(EXPANDR(maxLogFiles));
        this->readUintValue(EXPANDR(logFileSize));
        this->readDoubleValue(EXPANDR(updateCriteria));
        this->readIntValue(EXPANDR(securityOptions));
        this->readIntValue(EXPANDR(sessionPoolSize));
        this->readUintValue(EXPANDR(layerSize));
        // FIXME !!! Uncomment
//        this->readIntValue(EXPANDR(autoStart));

        this->setElementReader("dataFileSize", &dataFileSizeReader);
        this->setElementReader("tmpFileSize", &tmpFileSizeReader);

        this->setElementReader("sessionOptions", &sessionReader);
    };
};

void DatabaseOptions::saveToXml(XMLBuilder* xmlBuilder) const
{
    xmlBuilder->addElement(EXPANDWC(databaseId));
    xmlBuilder->addElement(EXPANDW(databaseName));
    xmlBuilder->addElement(EXPANDW(dataFilePath));
    xmlBuilder->addElement(EXPANDWC(bufferCount));
    xmlBuilder->addElement(EXPANDWC(maxLogFiles));
    xmlBuilder->addElement(EXPANDWC(updateCriteria));
    xmlBuilder->addElement(EXPANDWC(securityOptions));
    xmlBuilder->addElement(EXPANDWC(sessionPoolSize));
    xmlBuilder->addElement(EXPANDWC(layerSize));
    xmlBuilder->addElement(EXPANDWC(autoStart));

    xmlBuilder->beginElement("dataFileSize");
    dataFileSize.saveToXml(xmlBuilder);
    xmlBuilder->endElement();

    xmlBuilder->beginElement("tmpFileSize");
    tmpFileSize.saveToXml(xmlBuilder);
    xmlBuilder->endElement();

    xmlBuilder->beginElement("sessionOptions");
    sessionOptions.saveToXml(xmlBuilder);
    xmlBuilder->endElement();
}

XmlNodeReader* DatabaseOptions::createReader()
{
    return new DatabaseOptionsXmlReader(this);
}

/*
DatabaseOptions& DatabaseOptions::operator=(const DatabaseOptions& _x)
{
    if (this != &_x) {
        databaseName = _x.databaseName;
        bufferCount = _x.bufferCount;
        maxLogFiles = _x.maxLogFiles;
        updateCriteria = _x.updateCriteria;
        securityOptions = _x.securityOptions;
        sessionPoolSize = _x.sessionPoolSize;
        autoStart = _x.autoStart;
    }

    return &this;
}
*/

struct SednaOptionsXmlReader : public XmlNodeReader {
    SednaOptionsXmlReader(SednaOptions* options)
    {
        this->readStringValue(EXPANDR(dataDirectory));
        this->readIntValue(EXPANDR(listenPort));
        this->readStringValue(EXPANDR(bindAddress));
        this->readIntValue(EXPANDR(logLevel));
        this->readIntValue(EXPANDR(stackDepth));
        this->readIntValue(EXPANDR(keepAlive));
    }
};

XmlNodeReader* SednaOptions::createReader()
{
    return new SednaOptionsXmlReader(this);
}

void SednaOptions::saveToXml(XMLBuilder* xmlBuilder) const
{
    xmlBuilder->addElement(EXPANDW(dataDirectory));
    xmlBuilder->addElement(EXPANDWC(listenPort));
    xmlBuilder->addElement(EXPANDW(bindAddress));
    xmlBuilder->addElement(EXPANDWC(logLevel));
    xmlBuilder->addElement(EXPANDWC(stackDepth));
    xmlBuilder->addElement(EXPANDWC(keepAlive));
}

void GlobalParameters::loadFromStream(std::istream* stream)
{
    SednaOptionsXmlReader sednaOptionsReader(&this->global);
    DatabaseOptionsXmlReader dbOptionsReader(&this->defaultDatabaseParameters);
    XmlReader parser;
    
    XmlNodeReader * root =
      parser.getDocNodeReader()->createElementReader("sednaOptions", new XmlNodeReader());

    root->setJealousMode(true);
    root->setElementReader("global", &sednaOptionsReader);
    root->setElementReader("databaseDefaults", &dbOptionsReader);
    
    parser.readStream(stream);
}

void GlobalParameters::saveToStream(std::ostream* stream) const
{
    XMLBuilder writer(stream);

    writer.beginElement("sednaOptions");

    writer.beginElement("global");
    global.saveToXml(&writer);
    writer.endElement();

    writer.beginElement("databaseDefaults");
    defaultDatabaseParameters.saveToXml(&writer);
    writer.endElement();

    writer.close();
}

void GlobalParameters::loadDatabaseFromStream(const std::string& dbname, std::istream* stream)
{
    DatabaseOptionMap::iterator dboptIt = databaseOptions.find(dbname);
    DatabaseOptions * dbopts;

    if (dboptIt == databaseOptions.end()) {
        dbopts = &(databaseOptions.insert(DatabaseOptionMap::value_type(dbname, defaultDatabaseParameters)).first->second);
    } else {
        dbopts = &dboptIt->second;
    };

    DatabaseOptionsXmlReader dbOptionsReader(dbopts);
    XmlReader parser;
    parser.getDocNodeReader()->setElementReader("databaseOptions", &dbOptionsReader);
    parser.readStream(stream);
    
    dbopts->databaseName = dbname;
}

void GlobalParameters::saveDatabaseToStream(const std::string& dbname, std::ostream* stream) const
{
    DatabaseOptionMap::const_iterator dbopt = databaseOptions.find(dbname);

    if (dbopt == databaseOptions.end()) {
//        throw ;
    };

    XMLBuilder writer(stream);
    dbopt->second.saveToXml(&writer);
    writer.close();
}
