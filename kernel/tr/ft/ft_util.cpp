/*
 * File:  ft_util.cpp
 * Copyright (C) 2011 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/ft_util.h"

#include "tr/executor/base/PPBase.h"

FtHighlighter::FtHighlighter(const char *options, bool hl_fragment, PPOpIn* _seq_) : seq(_seq_)
#ifdef SE_ENABLE_DTSEARCH
											,sj(NULL)
#endif
{
	//TODO: parse options
	//FIXME: move stuff that throws exceptions

#ifdef SE_ENABLE_DTSEARCH
	//now just assume that options contain index name, TODO: change it
	if (options != NULL)
	{
		sj=se_new SednaSearchJob(true, hl_fragment);
		ftc_index_t ftc_idx;
		ft_index_cell_cptr ft_idx = find_ft_index(options, &ftc_idx);
		if (!ft_idx.found())
			throw USER_EXCEPTION(SE1061);
		sj->set_index(&*ft_idx);

		use_index = true;
	}
	else
	{
		sj=se_new SednaSearchJob(seq, ft_xml_hl, NULL, true, hl_fragment);
		use_index = false;
	}

#else
	//TODO
	throw USER_EXCEPTION(SE1002);
#endif
}

void FtHighlighter::set_request(tuple_cell &tc)
{
#ifdef SE_ENABLE_DTSEARCH
	sj->set_request(tc);
#else
	//TODO
	throw USER_EXCEPTION(SE1002);
#endif
}

void FtHighlighter::get_next_result(tuple &t)
{
#ifdef SE_ENABLE_DTSEARCH
	if (use_index)
	{
		while (true)
		{
			seq->op->next(t);
			if (t.is_eos())
				break;
			tuple_cell tc = t.cells[0];
			sj->set_file_cond_for_node(tc);

			sj->get_next_result(t);
			if (!t.is_eos())
			{
				tuple tmp(1);
				sj->get_next_result(tmp);
				if (!tmp.is_eos())
				{
					delete sj;
					sj = NULL;
					throw XQUERY_EXCEPTION2(SE1071, "problem with dtsearch"); //FIXME: change exception code
				}
				break;
			}
		}
	}
	else
		sj->get_next_result(t);

#else
	//TODO
	throw USER_EXCEPTION(SE1002);
#endif
}
