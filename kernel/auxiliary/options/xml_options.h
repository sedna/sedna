/*
 * File:  xml_config.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _XML_CONFIG_H_
#define _XML_CONFIG_H_

#include <stdint.h>

#include <string>
#include <stack>
#include <sstream>
#include <vector>
#include <stdexcept>

class EXMLException : public std::runtime_error {
public:
    explicit EXMLException(const std::string & what) : runtime_error(what) {};
};

class EXMLParserException : public EXMLException {
public:
    explicit EXMLParserException(const std::string & message) : EXMLException(message) {};
};

class EValueConversionException : public EXMLException {
public:
    explicit EValueConversionException(const std::string & message) : EXMLException(message) {};
};

class EXMLBuilderException : public EXMLException {
public:
    explicit EXMLBuilderException(const std::string & message) : EXMLException(message) {};
};

class XMLBuilder {
    std::stack<std::string> elementStack;

    std::stringstream * ownStream;
    std::ostream * out;

    bool hasLeafElement;
    bool openElement;

    void closeElement();
    void writeEscapedXML(const std::string & data, bool escapeQuotes);
public:
    XMLBuilder();
    XMLBuilder(std::ostream * _out);

    ~XMLBuilder() { delete ownStream; };

    std::string str() { return ownStream->str(); }

    XMLBuilder & addTrustedText(const std::string & value);

    XMLBuilder & addAttribute(const std::string & qname, const std::string & value);
    XMLBuilder & addElement(const std::string & qname, const std::string & value, bool fast = true);
    XMLBuilder & addText(const std::string & value, bool fast = true);

    XMLBuilder & beginElement(const std::string &qname);
    XMLBuilder & leafElement(const std::string &qname);
    XMLBuilder & endElement();

    void close();
};

class SimpleNodeTest {
public:
    const char * uri;
    const char * name;
    bool attribute;
    bool deep;

    bool matches(const char * _name, bool _attribute, bool _deep) const;
    
    SimpleNodeTest(const char * _name, bool _attribute = false, bool _deep = false)
      : uri(NULL), name(_name), attribute(_attribute), deep(_deep) { };

    SimpleNodeTest(const char * _uri, const char * _name, bool _attribute = false, bool _deep = false)
      : uri(_uri), name(_name), attribute(_attribute), deep(_deep) { };
      
    inline std::string toString() const {
        return std::string((attribute ? "@" : "")) +
          (uri == NULL ? "" : (std::string(uri) + ">")) + name;
    };
};

class XmlParserTrigger;

typedef void (*TypedValueHit)(const SimpleNodeTest & test, const std::string &, void *);

class XmlNodeReader {
  private:
    std::vector<XmlNodeReader *> ownedReaders;
    XmlParserTrigger * trigger;
  public:
    XmlNodeReader();
    virtual ~XmlNodeReader();

    XmlParserTrigger * getTrigger() { return trigger; }

    void readIntValue(const SimpleNodeTest& expr, int * value);
    void readUintValue(const SimpleNodeTest& expr, uint32_t * value);
    void readLongValue(const SimpleNodeTest& expr, int64_t * value);
    void readUlongValue(const SimpleNodeTest& expr, uint64_t * value);
    void readDoubleValue(const SimpleNodeTest& expr, double * value);
    void readStringValue(const SimpleNodeTest& expr, std::string * value);

    /* All other elements will trigger error */
    void setJealousMode(bool _jealousMode);

    XmlNodeReader * createElementReader(const SimpleNodeTest& expr, XmlNodeReader * reader);
    void setElementReader(const SimpleNodeTest& expr, XmlNodeReader * reader);
};

class XmlReader {
private:
    XmlNodeReader rootParser;
public:
    XmlNodeReader * getDocNodeReader() { return &rootParser; };

    void readStream(std::istream * _stream);
};

#endif /* _XML_CONFIG_H_ */
