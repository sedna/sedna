/*
 * File: ftserializer.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef FTSERIALIZER_H_
#define FTSERIALIZER_H_

#include "tr/crmutils/serialization.h"
#include "tr/crmutils/xmlserializer.h"
#include "tr/ft/ft_types.h"

class op_str_buf;

class FTSerializer : public XMLSerializer {
  private:
    ft_index_type serialization_type;
    ft_custom_tree_t * custom_tree;

    bool xmlOutput;
    bool insertDelimiters;
    bool ignoreTagNames;

    const char * tagNameSurrogate;
    char * xmlPITagNameOpen;
    char * xmlPITagNameClose;

    void printElementString(IXDMNode * element);
    void printString(const text_source_t text, int pclass);
  public:
    FTSerializer();
    ~FTSerializer();

    static FTSerializer * getSharedInstance();

    void printNodeToBuffer(xptr node, op_str_buf * outbuf, ft_index_type ast, const char * aOpenTag = "<", const char * aCloseTag = ">");

    virtual void printDocument(const text_source_t docname, IXDMNode * content);
    virtual void printText(t_item type, const text_source_t value);
    virtual void printAtomic(const tuple_cell &t);
    virtual void printElement(IXDMNode * element);
    virtual void printElementName(IXDMNode * element);

    virtual bool supports(enum se_output_method method) { return false; };
    virtual void initialize();

    void setSerializationType(ft_index_type st);
    void setOpenTag(const char * tag);
    void setCloseTag(const char * tag);
};

#endif /* FTSERIALIZER_H_ */
