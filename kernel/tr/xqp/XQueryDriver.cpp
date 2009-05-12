/*
 * File:  XQueryDriver.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "XQueryDriver.h"

#include <stdlib.h>
#include <string.h>
#include <sstream>

namespace sedna
{
    XQueryDriver::~XQueryDriver()
    {
        ErrorInfo *ner, *er;

        delete lexer;
        delete parser;

        // delete errors stack
        if (errors)
        {
            er = errors;
            while (er)
            {
                ner = er->next;

                if (er->error_msg)
                    free(er->error_msg);

                free(er);

                er = ner;
            }
        }

        delete tree;
    }

    bool XQueryDriver::parse(const char *query)
    {
        std::istringstream query_stream(query);

        if (tree)
        {
            delete tree;
            tree = NULL;
        }

        lexer = new XQueryLexer(*this, &query_stream);
        parser = new XQueryParser(*this);

        return parser->parse();
    }

    void XQueryDriver::error(const sedna::XQueryParser::location_type &loc, int code, const char *msg)
    {
        ErrorInfo *err = (ErrorInfo *)malloc(sizeof(ErrorInfo));

        if (msg)
        {
            err->error_msg = (char *)malloc(strlen(msg) + 64);
            sprintf(err->error_msg, "%d:%d-%d:%d %s", loc.begin.line, loc.begin.column, loc.end.line, loc.end.column, msg);
        }
        else
        {
            err->error_msg = NULL;
        }

        err->error_code = code;
        err->next = NULL;

        if (!errors)
        {
            errors = err_tail = err;
        }
        else
        {
            err_tail->next = err;
            err_tail = err;
        }
    }

    int XQueryDriver::getErrorCode() const
    {
        if (errors)
            return errors->error_code;
        else
            return -1;
    }

    std::string XQueryDriver::getErrorMsg() const
    {
        int first_code;
        ErrorInfo *err;
        std::string err_msg;

        if (!errors) return "";

        first_code = errors->error_code;
        err = errors;

        while (err && err->error_code == first_code && err->error_msg)
        {
            err_msg += err->error_msg;
            err_msg += "\n         ";

            err = err->next;
        }

        return err_msg;
    }
}
