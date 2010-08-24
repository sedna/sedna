/*
 * File: serialization.cpp
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

class XMLOutput {
public:
    virtual void startDocument() = 0;
    virtual void endDocument() = 0;
    virtual void startElement(const schema_node_cptr kind) = 0;
    virtual void endElement() = 0;
    virtual void attribute(const schema_node_cptr kind, const text_source_t value) = 0;
    virtual void text(const text_source_t value , void * markup) = 0;
    virtual void comment(const text_source_t value) = 0;
    virtual void pi(const text_source_t value, int target_offset) = 0;
    virtual void xmlnamespace(const xmlns_ptr ns) = 0;
};

class StdXMLOutput : public XMLOutput {
private:
    se_ostream& crmout;
    bool tagStarted;
    bool hasContent;
    std::string tagName;
    const char * finishing;

    void finishTag();
public:
    virtual void startDocument();
    virtual void endDocument();
    virtual void startElement(const schema_node_cptr kind);
    virtual void endElement();
    virtual void attribute(const schema_node_cptr kind, const text_source_t value);
    virtual void text(const text_source_t value , void * markup);
    virtual void comment(const text_source_t value);
    virtual void pi(const text_source_t value, int target_offset);
    virtual void xmlnamespace(const xmlns_ptr ns);
};

class SXMLOutput : public XMLOutput {
public:
    virtual void startDocument();
    virtual void endDocument();
    virtual void startElement(const schema_node_cptr kind);
    virtual void endElement();
    virtual void attribute(const schema_node_cptr kind, const text_source_t value);
    virtual void text(const text_source_t value , void * markup);
    virtual void comment(const text_source_t value);
    virtual void pi(const text_source_t value, int target_offset);
    virtual void xmlnamespace(const xmlns_ptr ns);
};

class Serializer {
    XMLOutput * output;
public:
    void serialize(xptr node);
};



static const char * XML_docTagStart = "<?xml version=\"1.0\" standalone=\"yes\"";
static const char * XML_docTagEnd = "?>";
static const char * XML_elTagEnd = ">";


void StdXMLOutput::startDocument() {
    crmout << XML_docTagStart;
    tagStarted = true;
    finishing = XML_docTagEnd;
}

void StdXMLOutput::finishTag() {
    if (tagStarted) {
        crmout << finishing;
        tagStarted = false;
    }
}

void StdXMLOutput::endDocument() {
    finishTag();
}

static std::string getTagName(const schema_node_cptr kind) {
    const xmlns_ptr * ns = kind->get_xmlns();
    std::string tagName;
    if (ns != NULL_XMLNS && ns->has_prefix()) {
        tagName = ns->get_prefix();
    } else {
        char * tagNameBuffer = (char *) malloc(strlen(ns->get_prefix()) + strlen(kind->get_name()) + 2);
        tagName = sprintf(tagNameBuffer, '%s:%s', ns->get_prefix(), kind->get_name());
        free(tagNameBuffer);
    }
    return tagName;
}

void StdXMLOutput::startElement(const schema_node_cptr kind) {
    tagName = getTagName(kind);
    tagStarted = true;
    finishing = XML_elTagEnd;
    crmout << '<' << tagName.c_str();
}

void StdXMLOutput::endElement() {
    if (!tagStarted) {
        crmout << "</" << tagName.c_str() << '>';
    } else {
        crmout << "/>";
    }
}

void StdXMLOutput::attribute(const schema_node_cptr kind, const text_source_t value) {

//    crmout << " " << getTagName(kind) << "=" << ;
}

void StdXMLOutput::text(const text_source_t value , void * markup);
void StdXMLOutput::comment(const text_source_t value);
void StdXMLOutput::pi(const text_source_t value, int target_offset);
void StdXMLOutput::xmlnamespace(const xmlns_ptr ns);


void Serializer::serialize(xptr node) {
    CHECK_TIMER_FLAG;

    CHECKP(node);

    switch(getNodeType(node)) {
        case virtual_root:
            // exception
            break;
        case document: {
            output->startDocument();
            xptr child=getFirstChild(node);
            while (child != XNULL) {
                this->serialize(node);
                child = nodeGetRightSibling(checkp(child));
            }
            output->endDocument();
        } break;

        case element: {
            output->startElement(getSchemaNode(node));
            xptr child=getFirstChild(node);
            while (child != XNULL) {
                this->serialize(node);
                child = nodeGetRightSibling(checkp(child));
            }
            output->endElement();
        } break;
        case xml_namespace: { output->xmlnamespace(NSNode(node).getNamespaceLocal()); } break;
        case attribute: { output->attribute(getSchemaNode(node), text_source_node(node)); } break;
        case text: { output->text(text_source_node(node), NULL); } break;
        case comment: { output->comment(text_source_node(node), NULL); } break;
        case pr_ins: { output->pi(text_source_node(node), PINode(node).getPITargetSize()); } break;
    }
}
