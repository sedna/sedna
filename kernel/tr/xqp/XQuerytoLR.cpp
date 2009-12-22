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

    // check for UTF-8 BOM and get rid of it
    if (query_len > 2 &&
        (unsigned char)query[0] == 0xef &&
        (unsigned char)query[1] == 0xbb &&
        (unsigned char)query[2] == 0xbf)
    {
        query += 3;
        query_len -= 3;
    }

    if (utf8_valid(query, query_len) >= 0)
        throw USER_EXCEPTION(SE4082);

    return std::string(query, query_len);
}


void parse_batch(sedna::XQueryDriver *drv, QueryType type, StringVector batch, std::string *module_name)
{
    U_ASSERT(drv);

    StringVector array, batch_utf;

    try
    {
        GET_TIME(&t1_parser);

        // check for BOM and valid UTF-8; batch is a copy
        for (unsigned int i = 0; i < batch.size(); i++)
            batch_utf.push_back(encoding_processing(batch[i].c_str()));

        switch (type)
        {
            case TL_XQuery:
            case TL_XQueryMod:
            case TL_XQueryAST:
                // parse query and create ast-tree; any errors will be thrown as exceptions
                for (unsigned int i = 0; i < batch_utf.size(); i++)
                    drv->parse(batch_utf[i].c_str());

                // do semantic analysis; any errors will be thrown as exceptions
                if (type != TL_XQueryAST)
                {
                    drv->doSemanticAnalysis();

                    // do lreturn optimizations
                    if (type != TL_XQueryMod)
                        drv->doLReturnAnalysis();
                }

                *module_name = drv->getParsedModuleName();

                GET_TIME(&t2_parser);
                ADD_TIME(t_total_parser, t1_parser, t2_parser);
                break;

            case TL_ASTInitial:
                // parse query and create ast-tree; any errors will be thrown as exceptions
                for (unsigned int i = 0; i < batch_utf.size(); i++)
                    drv->parseAST(batch_utf[i].c_str());

                // do semantic analysis; any errors will be thrown as exceptions
                drv->doSemanticAnalysis();

                // do lreturn optimizations
                drv->doLReturnAnalysis();

                *module_name = drv->getParsedModuleName();
                break;

            case TL_ASTQEPReady:
                // parse query and create ast-tree; any errors will be thrown as exceptions
                for (unsigned int i = 0; i < batch_utf.size(); i++)
                    drv->parseAST(batch_utf[i].c_str());

                // don't need to run any analysis here sine it's QEP-ready
                *module_name = "";
                break;

            default:
                throw USER_EXCEPTION2(SE4002, "unknown query type: only XQuery queries are supported now");
        }
    }
    catch (SednaUserException &e)
    {
        if (type == TL_XQuery)
        {
            GET_TIME(&t2_parser);
            ADD_TIME(t_total_parser, t1_parser, t2_parser);
        }

        throw;
    }
}

void parse_batch(sedna::XQueryDriver *drv, QueryType type, const char *batch1, std::string *module_name)
{
    U_ASSERT(drv);

    StringVector sv;

    sv.push_back(batch1);

    parse_batch(drv, type, sv, module_name);
}

void parse_batch_context(sedna::XQueryDriver *drv, const char *query, QueryType type, static_context *sx)
{
    U_ASSERT(drv);

    // parse query and create ast-tree with context predefined; any errors will be thrown as exceptions
    if (type == TL_XQuery)
    {
        drv->parseXQInContext(query, sx);
    }
    else
    {
        drv->parseASTInContext(query, sx);
    }
}
