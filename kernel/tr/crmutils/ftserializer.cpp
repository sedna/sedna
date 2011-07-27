/*
 * File: ftserializer.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/crmutils/ftserializer.h"
#include "tr/strings/strings.h"


class OutputStreamWriter : public se_ostream
{
  private:
    op_str_buf* outbuf;
  public:
    OutputStreamWriter (op_str_buf* aoutbuf) : outbuf(aoutbuf) {};
    virtual ~OutputStreamWriter() {}

    virtual se_ostream& operator<<(se_ostream& (*pf)(se_ostream&)) { U_ASSERT(false); return (*this); };

    virtual se_ostream& operator<<(const char *s) { (*outbuf) << s; return (*this); };
    virtual se_ostream& operator<<(char c) { (*outbuf) << c; return (*this); };
    virtual se_ostream& operator<<(bool n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(short n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(unsigned short n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(int n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(unsigned int n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(long n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(unsigned long n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(float n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(double n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(long double n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(void * n) { U_ASSERT(false); return (*this); };
    virtual se_ostream& operator<<(xptr n) { U_ASSERT(false); return (*this); };
#if defined(_MSC_VER) || !defined(SEDNA_X64)
    virtual se_ostream& operator<<(int64_t n) { U_ASSERT(false); return (*this); };
#endif
    virtual se_ostream& put(char c) { return (*this) << c; };
    virtual se_ostream& write(const char *s, int n) { outbuf->append(s, n); return (*this); };
    virtual se_ostream& flush(bool force = false) { U_ASSERT(false); return (*this); };
    virtual void endline() { U_ASSERT(false);};
    virtual void error(const char*) { U_ASSERT(false);};
    virtual se_ostream* get_debug_ostream() { U_ASSERT(false); return this; };
    virtual void set_debug_info_type(se_debug_info_type type) { U_ASSERT(false); };
};






/*
  Fulltext serialization options:

    xmlOutput
      Use default XML output. Fulltext serializer implements its own serialization
     methods, so the result may be different from the one, produced by default XML
     serializer.

    ignoreTagNames
      DTSearch needs some DirTy (or why do you think it had been called like that)
     hacks to work well, so it will be the greatest day, when this options will
     be obsolete and removed. The idea behind this one is to conceal some certain
     tags (like "meta") from DTSearch for it tries processing it in special way,
     and we do not need it.

    insertDelimiters
      Output space between tags. Useful for string-value serialization.

*/

static const char * FTXML_docTagStart = "?xml version=\"1.0\" standalone=\"yes\" encoding=\"utf-8\"";

static FTSerializer * sharedSerizlier;

FTSerializer* FTSerializer::getSharedInstance() { return sharedSerizlier; }
void FTSerializer::initSharedInstance() { sharedSerizlier = new FTSerializer(); }
void FTSerializer::disposeSharedInstance() { delete sharedSerizlier; }

void FTSerializer::printNodeToBuffer(xptr node, op_str_buf* outbuf, ft_index_type ast, ft_custom_tree_t* a_custom_tree, const char* aOpenTag, const char* aCloseTag)
{
    custom_tree = a_custom_tree;
    setOpenTag(aOpenTag);
    setCloseTag(aCloseTag);
    setSerializationType(ast);
    setOptions(NULL);

    OutputStreamWriter crmout(outbuf);
    setOutput(&crmout);
    initialize();
    printNode(node);
}

// We call actual constructor here, because we need actual implementation of serializer
FTSerializer::FTSerializer() : XMLSerializer(), serialization_type(ft_customized_value), xmlPITagNameOpen(NULL), xmlPITagNameClose(NULL) { }

FTSerializer::~FTSerializer() {
    if (xmlPITagNameOpen != NULL) { free(xmlPITagNameOpen); }
    if (xmlPITagNameClose != NULL) { free(xmlPITagNameClose); }
}

inline static
char * strconcatx(const char *s1, const char * s2) {
    size_t l1 = strlen(s1), l2 = strlen(s2);
    char * result = (char*) malloc(l1 + l2 + 1);
    memcpy(result, s1, l1);
    memcpy(result + l1, s2, l2 + 1);
    return result;
}

void FTSerializer::setOpenTag(const char* tag)
{
    openTagSeq = tag;

    if (xmlPITagNameOpen != NULL) { free(xmlPITagNameOpen); }
    docPISeqOpen = xmlPITagNameOpen = strconcatx(tag, FTXML_docTagStart);
}


void FTSerializer::setCloseTag(const char* tag)
{
    closeTagSeq = tag;

    if (xmlPITagNameClose != NULL) { free(xmlPITagNameClose); }
    docPISeqClose = xmlPITagNameClose = strconcatx("?", tag);
}

void FTSerializer::setSerializationType(ft_index_type st)
{
    serialization_type = st;
    xmlOutput = serialization_type == ft_xml || serialization_type == ft_xml_hl;
    insertDelimiters = serialization_type == ft_delimited_value;
    ignoreTagNames = serialization_type == ft_xml_hl;
}

void FTSerializer::printAtomic(const tuple_cell &t)
{
    throw USER_EXCEPTION2(SE2302, "Atomics are not serializable in fulltext");
}

void FTSerializer::initialize()
{
    options = NULL;
    elementContext = NULL;
    indentElements = false;
    indentNext = false;
    separatorNeeded = false;
    indentSequence = false;
    useCharmapFlag = 0;

    stringFilter.add_str("'","&apos;", pat_attribute);
}

void FTSerializer::printElementName(IXDMNode * elementInterface)
{
    if (ignoreTagNames) {
        (* crmout) << "x";
    } else {
        XMLPrintQName(elementInterface->getQName(), crmout);
    }
}

void FTSerializer::printElement(IXDMNode * elementInterface)
{
    ft_index_type save_serialization_type = serialization_type;

    if (custom_tree != NULL) {
        ft_custom_tree_t::sedna_rbtree_entry * sc = custom_tree->get(elementInterface->getQName().getLocalName(), elementInterface->getQName().getXmlNs());
        if (sc != NULL) {
            setSerializationType(sc->obj->cm);
        } else if (serialization_type == ft_customized_value) {
            setSerializationType(ft_xml);
        }
    }

    if (xmlOutput) {
        XMLSerializer::printElement(elementInterface);
    } else {
        if (insertDelimiters) { (*crmout) << " "; }
        printElementString(elementInterface);
    }

    setSerializationType(save_serialization_type);
}

void FTSerializer::printElementString(IXDMNode * elementInterface)
{
    IXDMNodeList * children = elementInterface->getAllChildren();

    if (!children->end()) do {
        switch (children->getNode()->getNodeKind()) {
          case xml_namespace : break;
          case pr_ins : case text : case comment :
            printString(children->getNode()->getValue(), pat_text); break;
          case attribute :
            printString(children->getNode()->getValue(), pat_attribute); break;
          case element :
            printElement(children->getNode()); break;
          default :
            throw USER_EXCEPTION2(SE2302, "Unserializable fulltext sequence");
        }
    } while (children->next());
}

void FTSerializer::printText(t_item type, const text_source_t value)
{
    if (xmlOutput) {
        XMLSerializer::printText(type, value);
    } else {
        printString(value, pat_text);
    }
}

void FTSerializer::printString(const text_source_t text, int sclass)
{
    TextBufferReader reader(text);

    while (reader.read()) {
        stringFilter.parse(reader.buffer, reader.size, write_func, crmout, sclass);
    }

    stringFilter.flush(write_func, crmout);
}

void FTSerializer::printDocument(const text_source_t docname, IXDMNode* content)
{
    IXDMNodeList * children = content->getAllChildren();

    while (!children->end()) {
        printNode(children->getNode());
        children->next();
    }
}
