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
  /* XQuery method */
    enum se_output_method xquery_output_method;

  /* XML specific options */
    bool preserveNamespaces;
    const char * indentSequence;
    bool indent;
    std::set<std::string> * cdataSectionElements;
};

class Serializer {
  protected:
    GlobalSerializationOptions * options;
    StrMatcher * stm;
    se_ostream * crmout;
  public:
    inline Serializer() : options(NULL), stm(NULL), crmout(NULL) {};
    virtual ~Serializer() {};

    virtual void serialize(tuple &t) = 0;

    virtual bool supports(enum se_output_method method) = 0;
    virtual void initialize() = 0;

    inline void setOutput(se_ostream * output) { crmout = output; };
    inline void setFilter(StrMatcher * filter) { stm = filter; };
    inline void setOptions(GlobalSerializationOptions * aOptions) { options = aOptions; }

    inline void prepare(se_ostream * output, StrMatcher * filter, GlobalSerializationOptions * aOptions) {
        setOutput(output); setFilter(filter); setOptions(aOptions); initialize();
    }

    static Serializer * createSerializer(enum se_output_method method);
};

#endif /* SERIALIZATION_H_ */
