/*
 * File: serialization.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SERIALIZATION_H_
#define SERIALIZATION_H_

#include "common/base.h"
#include "common/sedna.h"

#include "tr/executor/base/tuple.h"
#include "tr/crmutils/global_options.h"

class Serializer {
  protected:
    GlobalSerializationOptions * options;
    se_ostream * crmout;
  public:
    inline Serializer() : options(NULL), crmout(NULL) {};
    virtual ~Serializer() {};

    virtual void serialize(tuple &t) = 0;

    virtual bool supports(enum se_output_method method) = 0;
    virtual void initialize() = 0;

    inline void setOutput(se_ostream * output) { crmout = output; };
    inline void setOptions(GlobalSerializationOptions * aOptions) { options = aOptions; }

    inline void prepare(se_ostream * output, GlobalSerializationOptions * aOptions) {
        setOutput(output); setOptions(aOptions); initialize();
    }

    static Serializer * createSerializer(enum se_output_method method);
};

/* Print physical operations stack in debug mode */
void print_pp_stack(se_ostream* dostr);

#endif /* SERIALIZATION_H_ */
