/*
 * File:  ft_util.cpp
 * Copyright (C) 2011 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/ft_util.h"
#include "tr/executor/base/PPBase.h"
#include "tr/ft/ft_cache.h"
#include "tr/ft/query/ft_query.h"
#include "tr/strings/opt_parser.h"
#include "tr/ft/ft_index.h"
#include "tr/structures/nodeinterface.h"
#include "tr/crmutils/ftserializer.h"

FtHighlighter::FtHighlighter(bool _hl_fragment_, PPOpIn* _seq_) : seq(_seq_), hl_fragment(_hl_fragment_),
										impl(ft_ind_undefined), stemming(NULL),ftst(ftst_default),use_index(NULL),
											ftqp(NULL), ftc_ind(NULL)
#ifdef SE_ENABLE_DTSEARCH
											,sj(NULL)
#endif
{
}

FtHighlighter::~FtHighlighter()
{
	if (stemming != NULL)
	{
		free(stemming);
		stemming = NULL;
	}
	if (use_index != NULL)
	{
		free(use_index);
		use_index = NULL;
	}
	if (ftqp != NULL)
	{
		delete ftqp;
		ftqp = NULL;
	}
	if (ftc_ind != NULL)
	{
		delete ftc_ind;
		ftc_ind = NULL;
	}
#ifdef SE_ENABLE_DTSEARCH
	if (sj != NULL)
	{
		delete sj;
		sj = NULL;
	}
#endif
}

void FtHighlighter::set_options(const char *options)
{
	OptionsParser op;
	op.set_str(options);

	while (op.next_opt())
	{
		if (!strcmp(op.opt_name(), "native"))
		{
			if (op.opt_value()[0] || impl != ft_ind_undefined)
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
			impl = ft_ind_native;
		}
		else if (!strcmp(op.opt_name(), "dtsearch"))
		{
			if (op.opt_value()[0] || impl != ft_ind_undefined)
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
			impl = ft_ind_dtsearch;
		}
		else if (!strcmp(op.opt_name(), "stemming"))
		{
			if (stemming != NULL)
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
			stemming = strdup(op.opt_value());
		}
		else if (!strcmp(op.opt_name(), "stemtype"))
		{
			if (!strcmp(op.opt_value(), "both"))
				ftst = ftst_both;
			else
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
		}
		else if (!strcmp(op.opt_name(), "use_index"))
		{
			if (use_index != NULL)
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
			use_index = strdup(op.opt_value());
		}
		else
			throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
	}
}

void FtHighlighter::set_request(tuple_cell &tc)
{
	//FIXME: that's weird but more backward compatible
#ifdef SE_ENABLE_DTSEARCH
	if (impl == ft_ind_undefined)
		impl = ft_ind_dtsearch;
#endif
	if (impl == ft_ind_undefined)
		impl = ft_ind_native;
	switch (impl)
	{
	case ft_ind_native:
	{
		if (use_index != NULL)
			throw USER_EXCEPTION2(SE1002, "use_index is not supported for ft_ind_native");

		U_ASSERT(ftc_ind == NULL);
		ftc_ind = new FtcTempIndex(ftst);

		ftc_ind->set_stemming(stemming);

		ftqp = new FtQueryProcessor(ftc_ind->get());
		str_cursor *query_cur = get_text_cursor(text_source_tuple_cell(tc));
		ftqp->set_query(query_cur);
		delete query_cur;

		break;
	}
#ifdef SE_ENABLE_DTSEARCH
	case ft_ind_dtsearch:
	{
		if (use_index != NULL)
		{
			sj=se_new SednaSearchJob(true, hl_fragment);
			ftc_index_t ftc_idx;
			ft_index_cell_cptr ft_idx = find_ft_index(use_index, &ftc_idx);
			if (!ft_idx.found())
				throw USER_EXCEPTION(SE1061);
			sj->set_index(&*ft_idx);
		}
		else
		{
			sj=se_new SednaSearchJob(seq, ft_xml_hl, NULL, true, hl_fragment);
		}

		sj->set_request(tc);

		break;
	}
#endif
default:
	throw USER_EXCEPTION2(SE3022, "unknown index implementation");
	}
}

void FtHighlighter::get_next_result(tuple &t)
{
	switch (impl)
	{
	case ft_ind_native:
	{
		if (use_index != NULL)
			throw USER_EXCEPTION2(SE1002, "use_index is not supported for ft_ind_native");

		while (true)
		{
			seq->op->next(t);
			if (t.is_eos())
				break;
			tuple_cell tc = t.cells[0];
			if (!tc.is_node())
				throw USER_EXCEPTION2(SE3022, "fthighlight needs a node"); //FIXME
			Node nd = tc.get_node();
			xptr acc = nd.getIndirection();

			op_str_buf text_buf; //FIXME: move to class fields?
			text_buf.clear();
			//FIXME: allow to customize serialization?
			FTSerializer::getSharedInstance()->printNodeToBuffer(nd.getPtr(), &text_buf, ft_xml, NULL);

			ft_index_update(ft_insert, acc, &text_buf, ftc_ind->get());

			ftqp->open();
			xptr res;
			FtWordIndexList *wl;
			ftqp->get_next_result_hl(&res, &wl);

			if (res == XNULL)
			{
				//node didn't match query, try next one
				ftqp->close();
				ftc_ind->clear();
				continue;
			}

			if (res != acc)
				throw XQUERY_EXCEPTION(SE1003);

			{
				stmt_str_buf res_buf;
				ft_highlight(&text_buf, &res_buf, wl->get_inds(), wl->get_inds_count(), hl_fragment);
				t.copy(res_buf.get_tuple_cell());
			}

			ftqp->get_next_result_hl(&res, &wl);
			if (res != XNULL)
				throw XQUERY_EXCEPTION(SE1003);
			ftqp->close();
			ftc_ind->clear();

			break;
		}

		break;
	}
#ifdef SE_ENABLE_DTSEARCH
	case ft_ind_dtsearch:
	{
		if (use_index != NULL)
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
		break;
	}
#endif
default:
	throw USER_EXCEPTION2(SE3022, "unknown index implementation");
	}
}
