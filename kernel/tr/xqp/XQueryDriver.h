#ifndef XQUERY_DRIVER_H
#define XQUERY_DRIVER_H

#include <ostream>
#include <string>

#include "ast/ASTScript.h"

#include "XQueryLexer.h"
#include "XQueryParser.hpp"

namespace sedna
{
    struct ErrorInfo
    {
        int error_code;
        char *error_msg;

        struct ErrorInfo *next;
    };

    class XQueryDriver
    {
        friend class sedna::XQueryParser;

        private:
            XQueryLexer *lexer;
            XQueryParser *parser;

            ASTScript *tree;

            ErrorInfo *errors, *err_tail;

        public:

            XQueryDriver() : lexer(NULL), parser(NULL), tree(NULL), errors(NULL) {}
            ~XQueryDriver();

            ASTScript *getTree() const
            {
                return tree;
            }

            void setTree(ASTScript *new_tree)
            {
                if (tree)
                    delete tree;

                tree = new_tree;
            }

            void error(const sedna::XQueryParser::location_type &loc, int code, const char *msg);

            bool parse(const char *query);

            int getErrorCode() const;
            std::string getErrorMsg() const;
    };
}

#endif
