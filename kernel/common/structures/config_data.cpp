/* General database options structure */
/* However move this to config parameters */

#include "config_data.h"

#include "aux/cppcast.h"
#include "aux/options/xml_options.h"

#include "common/socketutils/socketutils.h"

#include <sstream>

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
    xmlBuilder->addElement(EXPANDWC(executionStackDepth));
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
      : dataFileSizeReader(&options->dataFile),
        tmpFileSizeReader(&options->tmpFile),
        sessionReader(&options->sessionOptions)
        
    {
        this->readStringValue(EXPANDR(databaseName));
        this->readUintValue(EXPANDR(bufferCount));
        this->readUintValue(EXPANDR(maxLogFiles));
        this->readDoubleValue(EXPANDR(updateCriteria));
        this->readIntValue(EXPANDR(securityOptions));

        this->setElementReader("dataFileSize", &dataFileSizeReader);
        this->setElementReader("tmpFileSize", &tmpFileSizeReader);

        this->setElementReader("sessionOptions", &sessionReader);
    };
};

void DatabaseOptions::saveToXml(XMLBuilder* xmlBuilder) const
{
    xmlBuilder->addElement(EXPANDW(databaseName));
    xmlBuilder->addElement(EXPANDWC(bufferCount));
    xmlBuilder->addElement(EXPANDWC(maxLogFiles));
    xmlBuilder->addElement(EXPANDWC(updateCriteria));
    xmlBuilder->addElement(EXPANDWC(securityOptions));

    xmlBuilder->beginElement("dataFileSize");
    dataFile.saveToXml(xmlBuilder);
    xmlBuilder->endElement();

    xmlBuilder->beginElement("tmpFileSize");
    tmpFile.saveToXml(xmlBuilder);
    xmlBuilder->endElement();

    xmlBuilder->beginElement("sessionOptions");
    sessionOptions.saveToXml(xmlBuilder);
    xmlBuilder->endElement();
}

XmlNodeReader* DatabaseOptions::createReader()
{
    return new DatabaseOptionsXmlReader(this);
}

struct SednaOptionsXmlReader : public XmlNodeReader {
    SednaOptionsXmlReader(SednaOptions* options)
    {
        this->readStringValue(EXPANDR(dataDirectory));
        this->readIntValue(EXPANDR(listenPort));
        this->readStringValue(EXPANDR(bindAddress));
        this->readIntValue(EXPANDR(osObjectsOffset));
        this->readIntValue(EXPANDR(logLevel));
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
    xmlBuilder->addElement(EXPANDWC(osObjectsOffset));
    xmlBuilder->addElement(EXPANDWC(logLevel));
}

void GlobalParameters::loadFromStream(std::istream* stream)
{
    SednaOptionsXmlReader sednaOptionsReader(&this->global);
    DatabaseOptionsXmlReader dbOptionsReader(&this->defaultDatabaseParameters);
    XmlReader parser;
    
    XmlNodeReader * root =
      parser.getDocNodeReader()->createElementReader("sednaOptions", new XmlNodeReader());

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
