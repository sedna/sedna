/*
 * File: global_options.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _GLOBAL_OPTIONS_H_
#define _GLOBAL_OPTIONS_H_

#include "tr/structures/xmlns.h"
#include "tr/executor/base/xsd.h"

#include <set>
#include <map>
#include <string>

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
    bool useCharmap;
    const char * indentSequence;
    bool indent;

    typedef std::set< xsd::QName > NameSet;
    NameSet cdataSectionElements;

    typedef std::set< std::pair<std::string, std::string> > Stringmap;
    Stringmap charmap;

    bool separateTuples;
};

#endif /* _GLOBAL_OPTIONS_H_ */
