/*
 * File:  XQuerytoLR.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/XQuerytoLR.h"
#include "tr/xqp/XQueryDriver.h"
#include "tr/xqp/visitor/LRVisitor.h"
#include "tr/xqp/ast/AST.h"

#include "tr/tr_utils.h"
#include "common/errdbg/d_printf.h"
#include "tr/strings/utf8.h"

EXTERN_DECLARE_TIME_VARS

static char* encoding_processing(const char *query)
{
    int query_len = strlen(query);
    if (query_len > 2 &&
        (unsigned char)query[0] == 0xef &&
        (unsigned char)query[1] == 0xbb &&
        (unsigned char)query[2] == 0xbf) {
        query += 3;
        query_len -= 3;
    }

    if (utf8_valid(query, query_len) >= 0)
        throw USER_EXCEPTION(SE4082);

    char* x = (char*)malloc(strlen(query) + 1);
    strcpy(x, query);
    return x;
}

StringVector parse_batch(QueryType type, const char *batch1)
{

    char* batch = NULL;
    StringVector array;
    sedna::XQueryDriver drv;
    LRVisitor lrv;
    ASTNodesVector::iterator it;
    ASTScript *scr;
    std::string err_msg;

    GET_TIME(&t1_parser);

    try
    {
        // check for BOM and valid UTF-8; batch is a copy
        batch = encoding_processing(batch1);

        if (type == TL_XQuery)
        {
            // parse query and create ast-tree
            drv.parse(batch);

            // get result; if error throw an exception
            if (drv.getErrorCode() != -1)
            {
                err_msg = drv.getErrorMsg();
                if (err_msg != "")
                    throw USER_EXCEPTION2(drv.getErrorCode(), err_msg.c_str());
                else
                    throw USER_EXCEPTION(drv.getErrorCode());
            }

            scr = drv.getTree();

            for (it = scr->modules->begin(); it != scr->modules->end(); it++)
            {
                // create lr for module ast
                (*it)->accept(lrv);

                // save result
                array.push_back(lrv.getResult());

                // reuse visitor by resetting its state
                lrv.resetVisitor();
            }
        }
        else // not TL_XQuery
        {
            array.push_back(std::string(batch));
        }

        GET_TIME(&t2_parser);

        ADD_TIME(t_total_parser, t1_parser, t2_parser);

        free(batch);

        return array;
    }
    catch (SednaUserException &e)
    {
        GET_TIME(&t2_parser);
        ADD_TIME(t_total_parser, t1_parser, t2_parser);

        free(batch);

        throw;
    }
}
