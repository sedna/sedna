#include "xml_options.h"
#include "common/cppcast.h"

#include <stdio.h>
#include <string.h>

#include <map>
#include <sstream>

#include <expat.h>

using namespace std;

#define ERROR_BUFFER_SIZE 64
static char errorBuffer[ERROR_BUFFER_SIZE];

XMLBuilder& XMLBuilder::beginElement(const string& qname) {
    closeElement();

    if (hasLeafElement) {
        endElement();
        hasLeafElement = false;
    };

    (*out) << "<" << qname;
    openElement = true;
    elementStack.push(qname);

    return *this;
}

void XMLBuilder::closeElement() {
    if (openElement) {
        (*out) << ">";
        openElement = false;
    }
}

XMLBuilder& XMLBuilder::endElement() {
    if (openElement) {
        (*out) << "/>";
    } else {
        (*out) << "</";
        (*out) << elementStack.top();
        (*out) << ">";
    }

    elementStack.pop();
    openElement = false;

    return *this;
}

XMLBuilder& XMLBuilder::leafElement(const string& qname) {
    beginElement(qname);
    hasLeafElement = true;

    return *this;
}

XMLBuilder& XMLBuilder::addAttribute(const string& qname, const std::string& value) {
    if (!openElement) {
        throw EXMLBuilderException("Cannot insert an attribute with no open element tag");
    }

    *out << " " << qname << "=\"";
    writeEscapedXML(value, true);
    *out << "\"";

    return *this;
}

XMLBuilder& XMLBuilder::addElement(const std::string& qname, const std::string& value, bool fast) {
    return this->beginElement(qname).addText(value, fast).endElement();
}

XMLBuilder& XMLBuilder::addTrustedText(const std::string& value) {
    closeElement();
    *out << value;

    return *this;
}

XMLBuilder& XMLBuilder::addText(const std::string& value, bool fast) {
    closeElement();

    if (fast) {
        *out << "<![CDATA[" << value << "]]>";
    } else {
        writeEscapedXML(value, false);
    }

    return *this;
}

XMLBuilder::XMLBuilder() :
  ownStream(NULL), out(NULL), hasLeafElement(false), openElement(false)
{
    ownStream = new std::stringstream();
    out = ownStream;
}

XMLBuilder::XMLBuilder(std::ostream* _out) :
  ownStream(NULL), out(_out), hasLeafElement(false), openElement(false)
{
}

void XMLBuilder::close() {
    while (!elementStack.empty()) {
        endElement();
    }
    out->flush();
}

void XMLBuilder::writeEscapedXML(const std::string& data, bool escapeQuotes) {
    *out << data;
}

static void setDouble(const SimpleNodeTest & test, const std::string & value, void * data) {
    * (double *) data = cast_string<double>(value);
};

static void setInt(const SimpleNodeTest & test, const std::string & value, void * data) {
    * (int *) data = cast_string<int>(value);
};

static void setUint(const SimpleNodeTest & test, const std::string & value, void * data) {
    * (uint32_t *) data = cast_string<uint32_t>(value);
};

static void setLong(const SimpleNodeTest & test, const std::string & value, void * data) {
    * (int64_t *) data = cast_string<int64_t>(value);
};

static void setUlong(const SimpleNodeTest & test, const std::string & value, void * data) {
    * (uint64_t *) data = cast_string<uint64_t>(value);
};

static void setString(const SimpleNodeTest & test, const std::string & value, void * data) {
    * (std::string *) data = value;
};

static const char SEPARATOR = '>';

/* XML Reader */

struct XmlParserTrigger;

struct XmlNodeProcessingInfo {
    SimpleNodeTest test;

    void * data;
    TypedValueHit hitProc;
    XmlParserTrigger * trigger;

    XmlNodeProcessingInfo(const SimpleNodeTest &_test, XmlParserTrigger * _trigger)
      : test(_test), data(NULL), hitProc(NULL), trigger(_trigger) {};
    XmlNodeProcessingInfo(const SimpleNodeTest &_test, TypedValueHit _hitProc, void * _data)
      : test(_test), data(_data), hitProc(_hitProc), trigger(NULL) {};
    
    inline bool fit(const char* _name, bool _attribute, bool _deep) const {
        return hitProc != NULL && test.matches(_name, _attribute, _deep);
    };
    
    inline void hit(const std::string & value) {
        (*hitProc)(test, value, data);
    };
};

bool SimpleNodeTest::matches(const char* _name, bool _attribute, bool _deep) const
{
    const char * localName;

    if (!((_attribute == attribute) && (deep || !_deep))) {
        return false;
    }

    if (NULL != (localName = strchr(_name, SEPARATOR))) {
        size_t uriLen = localName - _name;
        localName++;

        return ((uri != NULL) && (strcmp(name, localName) == 0) && (strncmp(_name, uri, uriLen) == 0));
    } else {
        return ((uri == NULL) && (strcmp(name, _name) == 0));
    };
}

struct XmlParserTrigger {
    typedef std::map<std::string, XmlNodeProcessingInfo *> ParserTreeMap;
    ParserTreeMap hitMap;

    ~XmlParserTrigger();

    // We do not pass string here in case we would want to process namespaces
    XmlParserTrigger * onElement(const char * name, bool deep);
    void onValue(const char * name, const std::string & value, bool attribute, bool deep);
/*
    void onText(const std::string & value, bool deep);
*/
};

XmlParserTrigger::~XmlParserTrigger()
{
    for (ParserTreeMap::iterator i = hitMap.begin(); i != hitMap.end(); ++i) {
        delete i->second;
    };
}

XmlParserTrigger* XmlParserTrigger::onElement(const char* name, bool deep)
{
    ParserTreeMap::const_iterator it = hitMap.find(std::string(name));

    if (it != hitMap.end() && (it->second->trigger != NULL) && it->second->test.matches(name, false, deep)) {
        return it->second->trigger;
    }

    return NULL;
};

void XmlParserTrigger::onValue(const char* name, const std::string& value, bool attribute, bool deep)
{
    ParserTreeMap::const_iterator it = hitMap.find((attribute ? "@" : "") + std::string(name));

    if (it != hitMap.end() && it->second->fit(name, attribute, deep)) {
        it->second->hit(value);
    };
}

XmlNodeReader::XmlNodeReader() : trigger(NULL)
{
    trigger = new XmlParserTrigger();
}

XmlNodeReader::~XmlNodeReader()
{
    for (std::vector<XmlNodeReader *>::iterator it = ownedReaders.begin();
            it != ownedReaders.end(); ++it) {
        delete *it;
    };

    delete trigger;
}

void XmlNodeReader::setElementReader(const SimpleNodeTest& expr, XmlNodeReader* reader)
{
    trigger->hitMap.insert(
      XmlParserTrigger::ParserTreeMap::value_type(expr.toString(),
        new XmlNodeProcessingInfo(expr, reader->getTrigger())));
}

void XmlNodeReader::readDoubleValue(const SimpleNodeTest& expr, double* value)
{
    trigger->hitMap.insert(
      XmlParserTrigger::ParserTreeMap::value_type(expr.toString(),
        new XmlNodeProcessingInfo(expr, &setDouble, value)));
}

void XmlNodeReader::readIntValue(const SimpleNodeTest& expr, int* value)
{
    trigger->hitMap.insert(
      XmlParserTrigger::ParserTreeMap::value_type(expr.toString(),
        new XmlNodeProcessingInfo(expr, &setInt, value)));
}

void XmlNodeReader::readLongValue(const SimpleNodeTest& expr, int64_t* value)
{
    trigger->hitMap.insert(
      XmlParserTrigger::ParserTreeMap::value_type(expr.toString(),
        new XmlNodeProcessingInfo(expr, &setLong, value)));
}

void XmlNodeReader::readStringValue(const SimpleNodeTest& expr, string* value)
{
    trigger->hitMap.insert(
      XmlParserTrigger::ParserTreeMap::value_type(expr.toString(),
        new XmlNodeProcessingInfo(expr, &setString, value)));
}

void XmlNodeReader::readUintValue(const SimpleNodeTest& expr, uint32_t* value)
{
    trigger->hitMap.insert(
      XmlParserTrigger::ParserTreeMap::value_type(expr.toString(),
        new XmlNodeProcessingInfo(expr, &setUint, value)));
}

void XmlNodeReader::readUlongValue(const SimpleNodeTest& expr, uint64_t* value)
{
    trigger->hitMap.insert(
      XmlParserTrigger::ParserTreeMap::value_type(expr.toString(),
        new XmlNodeProcessingInfo(expr, &setUlong, value)));
}

XmlNodeReader* XmlNodeReader::createElementReader(const SimpleNodeTest& expr, XmlNodeReader* reader)
{
    setElementReader(expr, reader);
    ownedReaders.push_back(reader);
    return reader;
}


#define BUFFER_SIZE 4*1024

struct XmlParserLevel {
    std::stringstream * textBuffer;
    XmlParserTrigger * trigger;
    bool deep;
    bool composite;

    void clear() {
        textBuffer->str(std::string());
        textBuffer->clear();
    };
};

struct ParserState {
private:
    std::vector<XmlParserLevel> nodeStack;
    int currentStackPosition;
public:
    ParserState() : currentStackPosition(-1) {};
  
    XmlParserLevel * push() {
        currentStackPosition++;

        if ((int) nodeStack.size() <= currentStackPosition) {
            nodeStack.push_back(XmlParserLevel());
            nodeStack.back().textBuffer = new std::stringstream();
        } else {
            nodeStack.at(currentStackPosition).clear();
        }

        return &(nodeStack.at(currentStackPosition));
    }

    XmlParserLevel * top() {
        return &(nodeStack.at(currentStackPosition));
    };

    XmlParserLevel * pop() {
        return &(nodeStack.at(currentStackPosition--));
    };    
};

class ExpatWrapperPP {
    XML_Parser p;
    char buffer[BUFFER_SIZE];
    ParserState pstate;
public:
    static void elementStart(void * _p, const char *el, const char **attr);
    static void elementEnd(void * _p, const char *el);
    static void dataHandler(void * p, const char * data, int len);

    ExpatWrapperPP(XmlParserTrigger * trigger);
    ~ExpatWrapperPP();

    void execute(std::istream* stream);
};

ExpatWrapperPP::ExpatWrapperPP(XmlParserTrigger * trigger)
{
    XmlParserLevel * topLevel = pstate.push();

    topLevel->deep = false;
    topLevel->composite = false;
    topLevel->trigger = trigger;

    p = XML_ParserCreateNS(NULL, SEPARATOR);

    if (!p) {
        throw EXMLParserException("Couldn't allocate memory for parser");
    }

    XML_SetUserData(p, &pstate);
    XML_SetReturnNSTriplet(p, 0);
    XML_SetElementHandler(p, elementStart, elementEnd);
    XML_SetCharacterDataHandler(p, dataHandler);
}

ExpatWrapperPP::~ExpatWrapperPP()
{
    XML_ParserFree(p);
}

void ExpatWrapperPP::execute(istream* stream)
{
  int len;
    int isFinal;

    /*
      * We have to cast since Expat uses 'int' instead of size_t,
      * it's safe since BUFFSIZE < INT_MAX
      */

    do {
        stream->read(buffer, BUFFER_SIZE);
        len = (int) stream->gcount();

        if (len == 0) { break; }

        isFinal = stream->eof();
        if (XML_Parse(p, buffer, len, isFinal) != XML_STATUS_OK) {
          snprintf(errorBuffer, ERROR_BUFFER_SIZE,
                         "XML parse error on line %lu: %s",
                         XML_GetCurrentLineNumber(p), XML_ErrorString(XML_GetErrorCode(p)));
          throw EXMLParserException(errorBuffer);
        }
    } while (!isFinal);
}


void ExpatWrapperPP::elementStart(void* _p, const char* el, const char** attr)
{
    ParserState * pstate = ((ParserState *) _p);
    XmlParserLevel * currentLevel = pstate->top();
    XmlParserLevel * childLevel = pstate->push();

/*
    if (currentLevel->textBuffer.tellp() > 0) {
        currentLevel->trigger->onText(currentLevel->textBuffer.str(), currentLevel->deep);
        currentLevel->textBuffer = std::stringstream();
        currentLevel->composite = true;
    }
*/

    childLevel->trigger = currentLevel->trigger->onElement(el, currentLevel->deep);
    childLevel->deep = childLevel->trigger == NULL;
    childLevel->composite = false;

    if (childLevel->deep) {
        childLevel->trigger = currentLevel->trigger;
    };

    while (attr[0] != NULL) {
        childLevel->trigger->onValue(attr[0], attr[1], true, childLevel->deep);
        attr += 2;
    }
}

void ExpatWrapperPP::elementEnd(void* _p, const char* el)
{
    ParserState * pstate = ((ParserState *) _p);
    XmlParserLevel * currentLevel = pstate->pop();
    XmlParserLevel * parentLevel = pstate->top();

    if (currentLevel->textBuffer->tellp() > 0) {
        std::string value = currentLevel->textBuffer->str();
/*
        currentLevel->trigger->onText(value, currentLevel->deep);
*/
//        currentLevel->textBuffer = std::stringstream();

        if (!currentLevel->composite) {
            parentLevel->trigger->onValue(el, value, false, parentLevel->deep);
        }
    }
}

void ExpatWrapperPP::dataHandler(void* p, const char* data, int len)
{
    XmlParserLevel * currentLevel = ((ParserState *) p)->top();
    currentLevel->textBuffer->write(data, len);
}

void XmlReader::readStream(istream* _stream)
{
    ExpatWrapperPP pp(getDocNodeReader()->getTrigger());
    pp.execute(_stream);
}
