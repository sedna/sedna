/*
 * File:  bulkload.cpp
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include <math.h>
#include <map>
#include <iostream>
#include <fstream>

#include "bulkload.h"

#include "tr/structures/metadata.h"
#include "expat.h"
#include "common/xptr.h"
#include "tr/mo/mo.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#endif

#include "tr/updates/updates.h"

#include "tr/models/IElementProducer.h"
#include "tr/models/SCElementProducer.h"

#define BUFFER_SIZE (16*1024)
#define MAX_QNAME 1024
#define TEXT_BUFFER_SIZE PAGE_SIZE
#define SEPARATOR    '>'

#define utfsafe_isspace(c) (isascii(c) && isspace(c))

static
bool whitespaceOnly(const text_source_t &x) {
    U_ASSERT(x.type == text_source_t::text_mem);
    size_t len = (size_t) get_text_size(x);

    // FIXME : create standart functions with size
    for (size_t i = 0; i < len; i++) {
        int c = x.u.cstr[i];

        if (!utfsafe_isspace(c)) {
            return false;
        }
    }

    return true;
}

static
text_source_t clearLeftSpaces(const text_source_t &x) {
    U_ASSERT(x.type == text_source_t::text_mem);
    size_t len = (size_t) get_text_size(x);

    // FIXME : create standart functions with size

    text_source_t result = NULL_TEXT; /* NULL_TEXT is text_mem ! */

    for (size_t i = 0; i < len; i++) {
        int c = x.u.cstr[i];

        if (!utfsafe_isspace(c)) {
            result.u.cstr = x.u.cstr + i;
            result._text_size = x._text_size - i;
            return result;
        }
    }

    U_ASSERT(result.type == text_source_t::text_mem);
    return result;
}

static
text_source_t getRightSpaces(const text_source_t &x) {
    U_ASSERT(x.type == text_source_t::text_mem);
    size_t len = (size_t) get_text_size(x);

    text_source_t result = NULL_TEXT; /* NULL_TEXT is text_mem ! */

    for (size_t i = len; i > 0; i--) {
        int c = x.u.cstr[i-1];

        if (!utfsafe_isspace(c)) {
            result.u.cstr = x.u.cstr + i;
            result._text_size = x.u.cstr + x._text_size - result.u.cstr;
            return result;
        }
    }

    // If we came to an end, whole string contains only whitespaces
    U_ASSERT(x.type == text_source_t::text_mem);
    return x;
}

class SafeParser {
    XML_Parser p;
  public:
    SafeParser() : p(NULL) {
        p = XML_ParserCreateNS(NULL, SEPARATOR);

        if (!p) {
            throw USER_ENV_EXCEPTION("Couldn't allocate memory for parser", true);
        }
    }

    ~SafeParser() { XML_ParserFree(p); };

    XML_Parser get() const { return p; }
};

inline static
int maxInt(int a, int b) { return MAX(a, b); }

inline static
int minInt(int a, int b) { return MIN(a, b); }

typedef std::vector<int> NodeCountStatistics;

class BulkLoader {
private:
    char parseBuffer[BUFFER_SIZE];
    IElementProducer * producer;
    std::stack<IElementProducer *> producerStack;
public:
    std::istream * inputStream;
private:
    
    void parseDocument(bool parseSchema) {
        SafeParser xmlParser;

        XML_Parser p = xmlParser.get();

        XML_SetUserData(p, this);
        XML_SetReturnNSTriplet(p,1);
        XML_SetElementHandler(p, elementStart, elementEnd);
        XML_SetStartNamespaceDeclHandler(p, namespaceHandler);
        XML_SetCommentHandler(p, commentHandler);
        XML_SetProcessingInstructionHandler(p, piHandler);
        XML_SetCharacterDataHandler(p, dataHandler);

        if (!parseSchema && (options.preserveCDataSection || options.stripBoundarySpaces)) {
            XML_SetCdataSectionHandler(p, startCDataHandler, endCDataHandler);
        }

        do {
            if (inputStream->read(parseBuffer, BUFFER_SIZE).bad()) {
                throw USER_ENV_EXCEPTION("Error while reading file", true);
            };

            if (XML_Parse(p, parseBuffer, inputStream->gcount(), inputStream->eof()) != XML_STATUS_OK) {
                char errorDetails[256];
                sprintf(errorDetails, "line %d:\n%s\n", (int) XML_GetCurrentLineNumber(p), XML_ErrorString(XML_GetErrorCode(p)));
                throw USER_EXCEPTION2(SE2005, errorDetails);
            }
        } while (!inputStream->eof());
    }

public:
    char textBuffer[TEXT_BUFFER_SIZE];
    size_t textBufferSize;
    bool updateIndexes;
    bool saveWSOption;
    bool loadNewDocument;

    BulkLoadOptions options;

    std::stringstream tailingWhitespaceBuffer;
    NodeCountStatistics nodeStatistics;
    std::vector< nid_hint_t > numbSchemeHints;

    BulkLoader(bool newDocumentOpt) :
        producer(NULL), inputStream(NULL),
        textBufferSize(0), updateIndexes(false), saveWSOption(false), loadNewDocument(newDocumentOpt)
    { /* qnameBuffer[MAX_QNAME - 1] = '\0'; */ };

    ~BulkLoader() { };

    void loadDocument(xptr doc_node, schema_node_cptr schema_node);

    static
    void elementStart(void * _loader, const char *el, const char **attr) {
        BulkLoader * loader = (BulkLoader *) _loader;
        xsd::QName qname = xsd::QName::bulkloadParse(el);
        IElementProducer * producer = loader->producer;

        loader->producerStack.push(producer);
        loader->producer = loader->producer->addElement(qname, xs_untyped);

        while (attr[0] != NULL) {
            loader->producer->addAttribute(xsd::QName::bulkloadParse(attr[0]), text_source_cstr(attr[1]), xs_untypedAtomic);
            attr += 2;
        }
    };

    static
    void elementEnd(void * _loader, const char *el) {
        BulkLoader * loader = (BulkLoader *) _loader;
        loader->producer->close();
        loader->producer = loader->producerStack.top();
        loader->producerStack.pop();
    };

    static
    void namespaceHandler(void * loader, const char * prefix, const char * uri) {
        ((BulkLoader *) loader)->producer->addNS(xmlns_touch(prefix, uri));
    };

    static
    void dataHandler(void * loader, const char * data, int len) {
        ((BulkLoader *) loader)->producer->addText(text_source_mem(data, (size_t) len));
    };

    static
    void commentHandler(void * loader, const char * data) {
        ((BulkLoader *) loader)->producer->addComment(text_source_cstr(data));
    }

    static
    void piHandler(void * loader, const char * name, const char * data) {
        ((BulkLoader *) loader)->producer->addPI(xsd::NCName::check(name, false), text_source_cstr(data));
    }

    static
    void startCDataHandler(void * loader);

    static
    void endCDataHandler(void * loader);
};

static const tuple_cell NULL_TC = tuple_cell::eos();

class SchemaParser : public IElementProducer {
  private:
    friend class DataParser;

    BulkLoader * parent;
    unsigned int level;

    int nextChildNamespaceCount;
    schema_node_cptr schemaNode;

    typedef std::map<xsd::QName, SchemaParser *, xsd::QName::FastCompare> ChildMap;
    typedef std::set<xsd::QName, xsd::QName::FastCompare> ChildSet;

    ChildMap children;
    ChildSet attributes;

    int textCount;
    int commentCount;
    int piCount;
    int count;
  public:
    SchemaParser(BulkLoader * _parent, schema_node_cptr _schemaNode, int) :
        parent(_parent), level(0), nextChildNamespaceCount(0), schemaNode(_schemaNode),
        textCount(0), commentCount(0), piCount(0), count(0)
    {
        parent->nodeStatistics.push_back(0);
    }

    SchemaParser(SchemaParser * _parentParser, schema_node_cptr _schemaNode) :
      parent(_parentParser->parent), level(_parentParser->level + 1), nextChildNamespaceCount(0),
      schemaNode(_schemaNode), textCount(0), commentCount(0), piCount(0), count(0)
    {
        while (parent->nodeStatistics.size() <= (NodeCountStatistics::size_type) level) {
            parent->nodeStatistics.push_back(0);
        }
    }

    virtual ~SchemaParser() {
        for (ChildMap::const_iterator i = children.begin(); i != children.end(); ++i) {
            delete i->second;
        }
    };

    virtual tuple_cell addNS(const xmlns_ptr ns) {
        nextChildNamespaceCount++;
        return NULL_TC;
    };

    virtual IElementProducer* addElement(const xsd::QName& qname, xmlscm_type type) {
        ChildMap::const_iterator iterator = children.find(qname);
        SchemaParser * result;

        if (iterator == children.end()) {
            schema_node_cptr child = parent->loadNewDocument ? XNULL : schemaNode->get_first_child(qname, element);

            if (!child.found()) {
                child = schemaNode->add_child(qname, element);
            }

            result = new SchemaParser(this, child);

            if (nextChildNamespaceCount > 0) {
                if (parent->loadNewDocument || child->get_first_child(NULL_XMLNS, NULL, xml_namespace) == XNULL) {
                    result->schemaNode->add_child(NULL_XMLNS, NULL, xml_namespace);
                }

                count += nextChildNamespaceCount;
                nextChildNamespaceCount = 0;
            }

            children.insert(ChildMap::value_type(qname, result));
        } else {
            result = iterator->second;
        }

        count++;
        return result;
    };

    virtual tuple_cell addAttribute(const xsd::QName& qname, const text_source_t value, xmlscm_type type) {
        if (attributes.count(qname) == 0) {
            if (parent->loadNewDocument || schemaNode->get_first_child(qname, attribute) == XNULL) {
                schemaNode->add_child(qname, attribute);
            }

            attributes.insert(qname);
        }

        count++;
        return NULL_TC;
    }

    virtual tuple_cell addComment(const text_source_t value) {
        if (commentCount == 0 &&
              (parent->loadNewDocument || schemaNode->get_first_child(NULL_XMLNS, NULL, comment) == XNULL))
        {
            schemaNode->add_child(NULL_XMLNS, NULL, comment);
        }

        commentCount++;
        count++;
        return NULL_TC;
    }

    virtual tuple_cell addPI(const xsd::NCName& name, const text_source_t value) {
        if (piCount == 0 &&
              (parent->loadNewDocument || schemaNode->get_first_child(NULL_XMLNS, NULL, pr_ins) == XNULL))
        {
            schemaNode->add_child(NULL_XMLNS, NULL, pr_ins);
        }

        piCount++;
        count++;
        return NULL_TC;
    }

    virtual tuple_cell addText(const text_source_t value) {
        if (textCount > 0 || (parent->options.stripBoundarySpaces && whitespaceOnly(value))) {
            return NULL_TC;
        } else if (parent->loadNewDocument || schemaNode->get_first_child(NULL_XMLNS, NULL, text) == XNULL) {
            schemaNode->add_child(NULL_XMLNS, NULL, text);
        }

        textCount++;
        count++;
        return NULL_TC;
    }

    virtual tuple_cell close() {
        if (parent->nodeStatistics.at(level) < count) {
            parent->nodeStatistics.at(level) = count;
        }

        count = 0;
        return NULL_TC;
    };

    virtual tuple_cell addNode(const tuple_cell& node, bool preserveType) { U_ASSERT(false); return NULL_TC; }
    virtual tuple_cell addAtomic(const tuple_cell& node) { U_ASSERT(false); return NULL_TC; };
    virtual bool hasNode(const tuple_cell& node) { U_ASSERT(false); return false; };
};

class DataParser : public IElementProducer {
  private:
    BulkLoader * parent;
    unsigned int level;
    bool hasText;

    typedef std::vector<xmlns_ptr> NamspaceList;
    NamspaceList nextChildNamespaces;

    schema_node_cptr schemaNode;

    bool stripLeftSpaces;
    typedef std::map<xsd::QName, DataParser *, xsd::QName::FastCompare> ChildMap;

    ChildMap children;

    schema_node_cptr textNode;
    schema_node_cptr commentNode;
    schema_node_cptr piNode;

    xptr left;
    xptr self;

    text_source_t tailingWhitespace;
  public:
    virtual ~DataParser() {
        for (ChildMap::const_iterator i = children.begin(); i != children.end(); ++i) {
            delete i->second;
        }

        /* remove hints */
        schemaNode->lastnode_ind = XNULL;
    };

    DataParser(SchemaParser * schemaParser) :
      parent(schemaParser->parent), level(schemaParser->level), schemaNode(schemaParser->schemaNode),
      left(XNULL), self(XNULL), tailingWhitespace(NULL_TEXT)
    {
        for (SchemaParser::ChildMap::const_iterator i = schemaParser->children.begin(); i != schemaParser->children.end(); ++i) {
            children.insert(ChildMap::value_type(i->first, new DataParser(i->second)));
        }
    };

    inline
    void updateNode(xptr nodei) {
        xptr node = checkp(indirectionDereferenceCP(nodei));
        schema_node_cptr scm = getSchemaNode(node);
        scm.modify();
        scm->lastnode_ind = nodei;

#ifdef SE_ENABLE_FTSEARCH
        if (parent->updateIndexes) {
            update_insert_sequence(node, scm);
        }
#endif
    }

    void addTextNode(const text_source_t value) {
        if (left != XNULL) {
            insert_text(indirectionDereferenceCP(left), XNULL, XNULL, value);
        } else {
            insert_text(XNULL, XNULL, indirectionDereferenceCP(self), value);
        }

        left = get_last_mo_inderection();
        updateNode(left);

        hasText = true;
    }

    void processText(bool stripTrailingSpaces, bool saveTrailingWhitespaces, bool breaksWhitespaces) {
        U_ASSERT(self != XNULL);

        // TODO: Currently, only BUFFER_SIZE whitespaces are truncated. This should be conditional.
        // No more clearing heading whitespaces in this text. We should strip

        if (parent->textBufferSize > 0) {
            text_source_t value = text_source_mem(parent->textBuffer, parent->textBufferSize);

            tailingWhitespace._text_size = 0;
            if (parent->options.stripBoundarySpaces) {
                if (breaksWhitespaces && !stripLeftSpaces && !hasText) {
                    if (whitespaceOnly(value)) {
                        parent->textBufferSize = 0;
                        return;
                    }
                }

                // Clear heading whitespaces
                if (stripLeftSpaces) {
                    value = clearLeftSpaces(value);

                    if (value._text_size > 0) {
                        stripLeftSpaces = false;
                    } else {
                        stripLeftSpaces = !breaksWhitespaces;
                        parent->textBufferSize = 0;
                        cdataflag_hint = cdata_inherit;
                        return;
                    }
                }

                if (stripTrailingSpaces) {
                    tailingWhitespace = getRightSpaces(value);
                    value._text_size -= tailingWhitespace._text_size;
                }
            }

            if (value._text_size > 0) {
                if (parent->options.stripBoundarySpaces) {
                    if (!parent->tailingWhitespaceBuffer.str().empty()) {
                        parent->tailingWhitespaceBuffer.flush();
                        const std::string& str = parent->tailingWhitespaceBuffer.str();
                        addTextNode(text_source_mem(str.c_str(), str.length()));
                        parent->tailingWhitespaceBuffer.str("");
                        parent->tailingWhitespaceBuffer.clear();
                    }
                }

                addTextNode(value);
                cdataflag_hint = cdata_inherit;
            }

            parent->textBufferSize = 0;

            if (saveTrailingWhitespaces && tailingWhitespace._text_size > 0) {
                memmove(parent->textBuffer, tailingWhitespace.u.cstr, (size_t) tailingWhitespace._text_size);
                parent->textBufferSize = tailingWhitespace._text_size;
                tailingWhitespace = NULL_TEXT;
            }
        }

        if (breaksWhitespaces) {
            stripLeftSpaces = false;
        }
    };

    virtual tuple_cell addNS(const xmlns_ptr ns) {
        nextChildNamespaces.push_back(ns);
        return NULL_TC;
    }

    void init(xptr selfXptr) {
        self = selfXptr;
        left = XNULL;
        stripLeftSpaces = true;
        hasText = false;

        /* Numbering scheme hint */
        sizehnt = &(parent->numbSchemeHints.at(level));

        U_ASSERT(parent->textBufferSize == 0);
    }

    void actualAddNamespace(const xmlns_ptr ns) {
        if (left != XNULL) {
            insert_namespace(indirectionDereferenceCP(left), XNULL, XNULL, ns);
        } else {
            insert_namespace(XNULL, XNULL, indirectionDereferenceCP(self), ns);
        }

        left = get_last_mo_inderection();
        updateNode(left);
    }

    virtual IElementProducer* addElement(const xsd::QName& qname, xmlscm_type type) {
        processText(false, false, true);

        ChildMap::const_iterator iterator = children.find(qname);
        U_ASSERT(iterator != children.end());
        DataParser * result = iterator->second;

        if (left != XNULL) {
            insert_element(indirectionDereferenceCP(left), XNULL, XNULL, qname, type);
        } else {
            insert_element(XNULL, XNULL, indirectionDereferenceCP(self), qname, type);
        }

        left = get_last_mo_inderection();
        updateNode(left);

        result->init(left);

        for (NamspaceList::const_iterator i = nextChildNamespaces.begin(); i != nextChildNamespaces.end(); ++i) {
            result->actualAddNamespace(*i);;
        }

        nextChildNamespaces.clear();

        return result;
    }

    virtual tuple_cell addAttribute(const xsd::QName& qname, const text_source_t value, xmlscm_type type) {
        if (left != XNULL) {
            insert_attribute(indirectionDereferenceCP(left), XNULL, XNULL, qname, type, value);
        } else {
            insert_attribute(XNULL, XNULL, indirectionDereferenceCP(self), qname, type, value);
        }

        left = get_last_mo_inderection();
        updateNode(left);

        return NULL_TC;
    }

    virtual tuple_cell addComment(const text_source_t value) {
        processText(false, false, true);

        if (left != XNULL) {
            insert_comment(indirectionDereferenceCP(left), XNULL, XNULL, value);
        } else {
            insert_comment(XNULL, XNULL, indirectionDereferenceCP(self), value);
        }

        left = get_last_mo_inderection();
        updateNode(left);

        return NULL_TC;
    }

    virtual tuple_cell addPI(const xsd::NCName& name, const text_source_t value) {
        processText(false, false, true);

        if (left != XNULL) {
            insert_pi(indirectionDereferenceCP(left), XNULL, XNULL, name, value);
        } else {
            insert_pi(XNULL, XNULL, indirectionDereferenceCP(self), name, value);
        }

        left = get_last_mo_inderection();
        updateNode(left);

        return NULL_TC;
    }

    virtual tuple_cell addText(const text_source_t value) {
        U_ASSERT(value.type == text_source_t::text_mem);

        if (SZ(value._text_size) > TEXT_BUFFER_SIZE) {
            U_ASSERT(false); /* This cannot be true while parsebuffer is smaller then textbuffer */
            throw SYSTEM_EXCEPTION("Text buffer exceeds parse buffer. Normaly it should not have happend, so report it as a bug.");
            return NULL_TC;
        }

        const size_t rest = (TEXT_BUFFER_SIZE - parent->textBufferSize);

        if (SZ(value._text_size) > rest) {
            if (rest > 0) {
                memcpy(parent->textBuffer + parent->textBufferSize, value.u.cstr, rest);
                parent->textBufferSize += rest;
            }

            processText(true, true, false);

            if (value._text_size > (TEXT_BUFFER_SIZE - parent->textBufferSize)) {
                U_ASSERT(false);
                parent->tailingWhitespaceBuffer.write(parent->textBuffer, parent->textBufferSize);
                parent->textBufferSize = 0;
                // FIXME: This works well, but actually, it is currently processed invalid
//                processText(false);
            };

            U_ASSERT(value._text_size <= (TEXT_BUFFER_SIZE - parent->textBufferSize));

            memcpy(parent->textBuffer + parent->textBufferSize, value.u.cstr + rest, value._text_size - rest);
            parent->textBufferSize += (value._text_size - rest);
        } else {
            memcpy(parent->textBuffer + parent->textBufferSize, value.u.cstr, value._text_size);
            parent->textBufferSize += value._text_size;
        }

        return NULL_TC;
    }

    virtual tuple_cell close() {
        processText(true, false, true);
        tailingWhitespace = NULL_TEXT;

        if (parent->options.stripBoundarySpaces) {
            parent->tailingWhitespaceBuffer.str("");
            parent->tailingWhitespaceBuffer.clear();
        }

        sizehnt = NULL;

        U_ASSERT(nextChildNamespaces.empty());
        U_ASSERT(parent->textBufferSize == 0);

        return NULL_TC;
    };

    virtual tuple_cell addNode(const tuple_cell& node, bool preserveType) { U_ASSERT(false); return NULL_TC; }
    virtual tuple_cell addAtomic(const tuple_cell& node) { U_ASSERT(false); return NULL_TC; };
    virtual bool hasNode(const tuple_cell& node) { U_ASSERT(false); return false; };
};

void BulkLoader::loadDocument(xptr doc_node, schema_node_cptr schema_node)
{
    nid_set_proportion(fnumber()); // Nobody knows WTF.

    scoped_ptr<SchemaParser> schemaParser(new SchemaParser(this, schema_node, 0));
    producer = schemaParser.get();

    inputStream->clear();
    inputStream->seekg(0);
    parseDocument(true);

    /* TODO: process node count statistics */
    for (NodeCountStatistics::const_iterator i = nodeStatistics.begin(); i != nodeStatistics.end(); ++i) {
        nid_hint_t nidHint = {};
        int count = MAX(*i, 1);

        nidHint.size = maxInt((int) ceil(log10(1. * count) / log10((double) MAX_LETTER)), 1);
        nidHint.increment = minInt(DEF_LETTER, ((int) pow((double) MAX_LETTER, nidHint.size)) / (2 + count));

        numbSchemeHints.push_back(nidHint);
    }

    scoped_ptr<DataParser> dataParser(new DataParser(schemaParser.get()));
    dataParser->init(doc_node);

    schemaParser = NULL; /* This will delete schema parser */

    U_ASSERT(producerStack.empty());
    producer = dataParser.get();

    inputStream->clear();
    inputStream->seekg(0);
    parseDocument(false);
}


BulkLoadFrontend::BulkLoadFrontend() : inputStream(NULL)
{
    options.preserveCDataSection = false;
    options.stripBoundarySpaces = true;
}

BulkLoadFrontend::~BulkLoadFrontend()
{
}

void BulkLoadFrontend::setSourceStream(std::istream* _stream)
{
    inputStream = _stream;
}

Node BulkLoadFrontend::loadDocument(const char* documentName)
{
    BulkLoader loader(true);

    loader.options = options;
    loader.updateIndexes = false;
    loader.inputStream = inputStream;

    xptr docNode = insert_document(documentName);

    loader.loadDocument(getIndirectionSafeCP(docNode), getSchemaNode(checkp(docNode)));

    return docNode;
}

Node BulkLoadFrontend::loadCollectionDocument(const char* collectionName, const char* documentName)
{
    BulkLoader loader(false);

    loader.options = options;
    loader.updateIndexes = true;
    loader.inputStream = inputStream;

    xptr docNode = insert_document_into_collection(collectionName, documentName);

#ifdef SE_ENABLE_FTSEARCH
    update_insert_sequence(docNode, getSchemaNode(checkp(docNode)));
#endif

    loader.loadDocument(getIndirectionSafeCP(docNode), getSchemaNode(checkp(docNode)));

#ifdef SE_ENABLE_FTSEARCH
    execute_modifications();
#endif

    return docNode;
}

void BulkLoader::startCDataHandler(void* loader)
{
    DataParser * parser = dynamic_cast<DataParser *>(((BulkLoader *) loader)->producer);
    if (parser != NULL) {
        parser->processText(false, false, true);
    }

    if (((BulkLoader *) loader)->options.preserveCDataSection) {
        cdataflag_hint = cdata_section | cdata_infect;
    }

    ((BulkLoader *) loader)->saveWSOption = ((BulkLoader *) loader)->options.stripBoundarySpaces;
    ((BulkLoader *) loader)->options.stripBoundarySpaces = false;
}


void BulkLoader::endCDataHandler(void* loader)
{
    DataParser * parser = dynamic_cast<DataParser *>(((BulkLoader *) loader)->producer);
    if (parser != NULL) {
        parser->processText(false, false, true);
    }

    ((BulkLoader *) loader)->options.stripBoundarySpaces = ((BulkLoader *) loader)->saveWSOption;

    cdataflag_hint = cdata_inherit;
}
