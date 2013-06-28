/*
 * File: PPDigest.cpp
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include <algorithm>
#include <string>

#include "common/sedna.h"

#include "tr/executor/xqops/PPDigest.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/crypto/crypto.h"

PPDigest::PPDigest(dynamic_context *_cxt_,
                   operation_info _info_,
                   const char* _digest_name_,
                   PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPDigest"),
                                     digest_name(_digest_name_),
                                     child(_child_)
{
}

PPDigest::PPDigest(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _name_child_,
                   PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPDigest"),
                                     digest_name(NULL),
                                     name_child(_name_child_),
                                     child(_child_)
{
}

PPDigest::~PPDigest()
{
    delete child.op;
    child.op = NULL;

    if (name_child.op) {
        delete name_child.op;
        name_child.op = NULL;
    }
}

void PPDigest::do_open ()
{
    child.op->open();
    if (name_child.op) {
        name_child.op->open();
    }
    first_time = true;
}


void PPDigest::do_reopen()
{
    child.op->reopen();
    if (name_child.op) {
        name_child.op->reopen();
    }
    first_time = true;
}

void PPDigest::do_close()
{
    child.op->close();
    if (name_child.op) {
        name_child.op->close();
    }
}

const char* PPDigest::get_function_name()
{
    if (digest_name) {
        return digest_name;
    } else {
        return "hash";
    }
}

static std::string trimAndLower(const std::string& str)
{
    const std::string& whitespace = " \t";
    const size_t strBegin = str.find_first_not_of(whitespace);
    if (strBegin == std::string::npos)
        return "";

    const size_t strEnd = str.find_last_not_of(whitespace);
    const size_t strRange = strEnd - strBegin + 1;

    std::string res = str.substr(strBegin, strRange);
    std::transform(res.begin(), res.end(), res.begin(), ::tolower);
    return res;
}

void PPDigest::do_next(xqp_tuple &t)
{
    if (first_time)
    {
        child.op->next(t);

        if (!t.is_eos())
        {
            first_time = false;
            tuple_cell in_str = atomize(child.get(t));

            if(!is_string_type(in_str.get_atomic_type())) {
                throw XQUERY_EXCEPTION2(XPTY0004, (std::string("Invalid type of the argument in {"CRYPTO_MODULE_NAMESPACE"}::") +
                                                  get_function_name() +
                                                  " (xs_string/derived/promotable is expected).").c_str());
            }

            child.op->next(t);
            if (!t.is_eos()) {
                throw XQUERY_EXCEPTION2(XPTY0004, (std::string("Invalid arity of the argument. Argument contains more than one item in {"CRYPTO_MODULE_NAMESPACE"}::") +
                                                  get_function_name()).c_str());
            }

            tuple_cell tc;
            const char* res_digest_name = digest_name;
            if (digest_name == NULL) {
                tc = get_name_from_PPOpIn(name_child, "hash function",
                        (std::string("{"CRYPTO_MODULE_NAMESPACE"}::") + get_function_name()).c_str(), false, false);
                res_digest_name = tc.get_str_mem();
            }

            scoped_ptr<Digest> digest = Digest::create(res_digest_name);
            if (digest.isnull()) {
                std::string normalizedName = trimAndLower(std::string(res_digest_name));
                digest = Digest::create(normalizedName.c_str());
                if (digest.isnull()) {
                    throw XQUERY_EXCEPTION2(SE2044, std::string(res_digest_name).c_str());
                }
            }
            t.copy(digest->get(&in_str));
        }
    } else {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDigest::do_copy(dynamic_context *_cxt_)
{
    PPDigest *res = name_child.op ?
        new PPDigest(_cxt_, info, name_child, child) :
        new PPDigest(_cxt_, info, digest_name, child);
    res->child.op = child.op->copy(_cxt_);
    if (name_child.op) {
        res->name_child.op->copy(_cxt_);
    }
    return res;
}

void PPDigest::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    if (name_child.op) {
        name_child.op->accept(v);
    }
    v.pop();
}
