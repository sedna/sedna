/* General database options structure */
/* However move this to config parameters */

#include "config_data.h"

#include "aux/cppcast.h"
#include "aux/options/xml_options.h"

#include <sstream>

#define EXPANDR(OPT) #OPT, &options->OPT 
#define EXPANDW(OPT) #OPT, options->OPT 

struct SednaOptionsXmlReader : public XmlNodeReader {
    SednaOptionsXmlReader(SednaOptions* options) {
        this->readStringValue(EXPANDR(dataDirectory));
        this->readIntValue(EXPANDR(listenPort));
        this->readStringValue(EXPANDR(bindAddress));
        this->readIntValue(EXPANDR(osObjectsOffset));
    }
};

struct DatabaseFileSizeXmlReader : public XmlNodeReader {
    DatabaseFileSizeXmlReader(DatabaseOptions::DatabaseFileSize* options) {
        this->readUintValue(EXPANDR(initial));
        this->readUintValue(EXPANDR(max));
        this->readUintValue(EXPANDR(extension));
    };
};

struct DatabaseOptionsXmlReader : public XmlNodeReader {
    DatabaseFileSizeXmlReader dataFileSizeReader, tmpFileSizeReader;

    DatabaseOptionsXmlReader(DatabaseOptions* options) 
      : dataFileSizeReader(&options->dataFile), tmpFileSizeReader(&options->tmpFile)
    {
        this->readStringValue(EXPANDR(databaseName));
        this->readUintValue(EXPANDR(bufferCount));
        this->readUintValue(EXPANDR(maxLogFiles));
        this->readDoubleValue(EXPANDR(updateCriteria));
        this->readIntValue(EXPANDR(securityOptions));

        this->setElementReader("dataFileSize", &dataFileSizeReader);
        this->setElementReader("tmpFileSize", &tmpFileSizeReader);
    };
};

void saveSednaOptions(SednaOptions* options, XMLBuilder* xmlBuilder)
{
    xmlBuilder->beginElement("sednaOptions");

    xmlBuilder->addElement("dataDirectory", options->dataDirectory);
    xmlBuilder->addElement("listenPort", cast_to_string(options->listenPort), false);
    xmlBuilder->addElement("bindAddress", options->bindAddress, false);
    xmlBuilder->addElement("osObjectsOffset", cast_to_string(options->osObjectsOffset), false);

    xmlBuilder->endElement();
};

static
void saveDatabaseFileSize(DatabaseOptions::DatabaseFileSize * options, XMLBuilder* xmlBuilder)
{
    xmlBuilder->addElement("initial", cast_to_string(options->initial), false);
    xmlBuilder->addElement("max", cast_to_string(options->max), false);
    xmlBuilder->addElement("extension", cast_to_string(options->extension), false);
};

void saveDatabaseOptions(DatabaseOptions * options, XMLBuilder* xmlBuilder)
{
    xmlBuilder->beginElement("databaseOptions").addAttribute("name", options->databaseName);

    xmlBuilder->addElement("databaseName", options->databaseName);
    xmlBuilder->addElement("bufferCount", cast_to_string(options->bufferCount), false);
    xmlBuilder->addElement("maxLogFiles", cast_to_string(options->maxLogFiles), false);
    xmlBuilder->addElement("updateCriteria", cast_to_string(options->updateCriteria), false);
    xmlBuilder->addElement("securityOptions", cast_to_string(options->securityOptions), false);

    xmlBuilder->beginElement("dataFile");
    saveDatabaseFileSize(&options->dataFile, xmlBuilder);
    xmlBuilder->endElement();

    xmlBuilder->beginElement("tmpFile");
    saveDatabaseFileSize(&options->tmpFile, xmlBuilder);
    xmlBuilder->endElement();

    xmlBuilder->endElement();
};

#include <iostream>

//void saveSednaOptions(SednaOptions * options, std::ostream * stream);
//void saveDatabaseOptions(DatabaseOptions * options, std::ostream * stream);
//void saveGlobalParameters(GlobalParameters * options, std::ostream * stream);

void loadSednaOptions(SednaOptions * options, std::istream * stream);
void loadDatabaseOptions(DatabaseOptions * options, std::istream * stream);
void loadGlobalParameters(GlobalParameters * options, std::istream * stream);

static SednaOptions sednaOptions;

int main() {
    static SednaOptionsXmlReader sednaOptionsReader(&sednaOptions);

    sednaOptions.bindAddress = "0.0.0.0";
    sednaOptions.dataDirectory = "../data";
    sednaOptions.listenPort = 5050;
    sednaOptions.osObjectsOffset = 1200;

    XmlReader reader;
    reader.getDocNodeReader()->setElementReader(SimpleNodeTest("sednaOptions", false, true), &sednaOptionsReader);
    reader.readStream(&(std::cin));

    XMLBuilder builder(&(std::cout));
    saveSednaOptions(&sednaOptions, &builder);
    builder.close();

    std::cout << "\n";

    return 0;
}

