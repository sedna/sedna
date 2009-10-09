/*
 * File:  XQuerytoLR.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/XQuerytoLR.h"
#include "tr/xqp/XQueryDriver.h"

#include "tr/tr_utils.h"
#include "common/errdbg/d_printf.h"
#include "tr/strings/utf8.h"

EXTERN_DECLARE_TIME_VARS

static std::string encoding_processing(const char *query)
{
    unsigned int query_len = strlen(query);

    if (query_len > 2 &&
        (unsigned char)query[0] == 0xef &&
        (unsigned char)query[1] == 0xbb &&
        (unsigned char)query[2] == 0xbf) {
        query += 3;
        query_len -= 3;
    }

    if (utf8_valid(query, query_len) >= 0)
        throw USER_EXCEPTION(SE4082);

    return std::string(query, query_len);
}


StringVector parse_batch(QueryType type, StringVector batch, std::string *module_name)
{
    StringVector array, batch_utf;
    sedna::XQueryDriver drv;

    GET_TIME(&t1_parser);

    try
    {
        // check for BOM and valid UTF-8; batch is a copy
        for (unsigned int i = 0; i < batch.size(); i++)
            batch_utf.push_back(encoding_processing(batch[i].c_str()));

        if (type == TL_XQuery)
        {
            // parse query and create ast-tree; any errors will be thrown as exceptions
            for (unsigned int i = 0; i < batch_utf.size(); i++)
                drv.parse(batch_utf[i].c_str());

            // do semantic analysis; any errors will be thrown as exceptions
            drv.doSemanticAnalysis();

            // do lreturn optimizations
            drv.doLReturnAnalysis();

            *module_name = drv.getParsedModuleName();

            if (*module_name == "")
                array = drv.getLRRepresentation();
            else
                array = drv.getIRRepresentation();
        }
        else // not TL_XQuery
        {
            array = batch_utf;
        }

        GET_TIME(&t2_parser);

        ADD_TIME(t_total_parser, t1_parser, t2_parser);

        return array;
    }
    catch (SednaUserException &e)
    {
        GET_TIME(&t2_parser);
        ADD_TIME(t_total_parser, t1_parser, t2_parser);

        throw;
    }
}

StringVector parse_batch(QueryType type, const char *batch1, std::string *module_name)
{
    StringVector sv;

    sv.push_back(batch1);

    return parse_batch(type, sv, module_name);
}

