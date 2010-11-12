/*
 * File: serialization.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/crmutils/serialization.h"
#include "tr/crmutils/xmlserializer.h"
#include "tr/crmutils/global_options.h"

#include "tr/executor/base/PPBase.h"

void print_pp_stack(se_ostream* dostr)
{
    const char* indent = "  ";
    std::ostringstream message;
    message << "<stack xmlns='" << SEDNA_NAMESPACE_URI << "'>" << std::endl;

    while(!executor_globals::pp_stack.empty()) {
        const operation_info& oi = executor_globals::pp_stack.back();
        executor_globals::pp_stack.pop_back();
        message << indent << "<operation name='" << oi.name;
        if(oi.query_line != 0)
            message << "' line='" << oi.query_line;
        if(oi.query_col != 0)
            message << "' column='" << oi.query_col;
        message << "' calls='" << oi.profile->calls;
        message << "'/>" << std::endl;
    }

    message << "</stack>" << std::endl;

    dostr->set_debug_info_type(se_QueryDebug);
    (*dostr) << message.str().c_str();
    dostr->flush();
}


Serializer * Serializer::createSerializer(enum se_output_method method)
{
    switch (method) {
      case se_output_method_xml:
        return new XMLSerializer();
        break;
      case se_output_method_sxml:
        return new SXMLSerializer();
        break;
      default:
        throw USER_EXCEPTION(SE2301);
    }
}
