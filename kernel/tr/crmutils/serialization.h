/*
 * File: serialization.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SERIALIZATION_H_
#define SERIALIZATION_H_

#include "common/base.h"
#include "common/sedna.h"

#include "tr/executor/base/tuple.h"
#include "tr/crmutils/str_matcher.h"

/* Serialization stuff */
enum se_output_method  {se_output_method_xml};
enum se_output_indent  {se_output_indent_yes, se_output_indent_no};

class Serializer {
public:
    virtual void serialize(xptr node) = 0;
    virtual void serializeTuple(tuple * t) = 0;
};

struct serialization_params
{
    se_output_method output_method;
    se_output_indent output_indent;

    // string matcher (symbol substitution in result output)
    StrMatcher stm;
};

#endif /* SERIALIZATION_H_ */
