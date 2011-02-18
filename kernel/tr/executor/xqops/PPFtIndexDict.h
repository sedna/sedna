/*
 * File:  PPFtIndexDict.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFTINDEXDICT_H
#define _PPFTINDEXDICT_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"

//don't want to inculude ftc_cache.h here
class ftc_scan_words_result;

class PPFtIndexDict : public PPIterator
{
protected:
    PPOpIn idx_name, options;

	bool first_time;
	ftc_scan_words_result *ftc_res;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFtIndexDict(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _idx_name_);

    PPFtIndexDict(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _idx_name_,
                  PPOpIn _options_);

    virtual ~PPFtIndexDict();
};

#endif
