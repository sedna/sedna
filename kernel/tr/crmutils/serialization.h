/*
 * File: serialization.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SERIALIZATION_H_
#define SERIALIZATION_H_

#include "common/base.h"
#include "common/sedna.h"

#include "tr/executor/base/tuple.h"


/*
enum se_output_method  {se_output_method_xml};
enum se_output_indent  {se_output_indent_yes, se_output_indent_no};
*/

class StrMatcher;

enum se_output_method {
    se_output_method_xml  = 0,
    se_output_method_sxml = 1,
    se_output_method_json = 0x10
};

struct GlobalSerializationOptions {
  /* Global options */
    StrMatcher * stm;
    dynamic_context * cxt;

  /* XML specific options */
    bool preserveNamespaces;
    const char * indentSequence;
    bool indent;
    bool cdataSectionElements;
};

class Serializer {
  public:
    virtual ~Serializer();

    virtual void serialize(tuple &t) = 0;

    virtual bool supports(enum se_output_method method) = 0;
    virtual void initialize() = 0;

    virtual void setOutputStream(se_ostream & out) = 0;
};

Serializer * createSerializer(enum se_output_method method, GlobalSerializationOptions * options);

#endif /* SERIALIZATION_H_ */
