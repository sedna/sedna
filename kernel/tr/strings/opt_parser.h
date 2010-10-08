/*
 * File:  opt_parser.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _OPT_PARSER_H
#define _OPT_PARSER_H

#include "tr/strings/strings.h"
#include "tr/executor/base/tuple.h"

//parses options in the form "Name[=Value]{,Name[=Value]}" Name - consinsts of letters, Value - anything except ','; all whitespace is stripped (i.e. "A a=1 1" equiv to "Aa=11")
//FIXME: currenty "Name=" is equivalent to "Name"
class OptionsParser
{
protected:
	static const int opt_name_buf_size = 32;
	char opt_name_buf[opt_name_buf_size];
	static const int opt_value_buf_size = 32;
	char opt_value_buf[opt_value_buf_size];
	unicode_cp_iterator *ucpi;
	int next_char;

	void reset()
	{
		if (ucpi != NULL)
		{
			delete ucpi;
			ucpi = NULL;
		}
		opt_name_buf[0] = 0;
		opt_value_buf[0] = 0;
		next_char = unicode_cp_iterator::EOS;
	}

	void get_next_nonws_char()
	{
		U_ASSERT(ucpi != NULL);
		do
		{
			next_char = ucpi->get_next_char();
		} while (next_char != unicode_cp_iterator::EOS && iswspace(next_char));
		if (next_char == unicode_cp_iterator::EOS)
		{
			delete ucpi;
			ucpi = NULL;
		}
	}

public:
	OptionsParser() : ucpi(NULL)
	{
	}

	void set_tc(const tuple_cell *tc)
	{
		reset();
		ucpi = charset_handler->get_unicode_cp_iterator(tc);
	}

	~OptionsParser()
	{
		reset();
	}

	bool next_opt()
	{
		while (ucpi != NULL && (next_char == unicode_cp_iterator::EOS || next_char == ','))
			get_next_nonws_char();
		if (next_char == unicode_cp_iterator::EOS)
		{
			U_ASSERT(ucpi == NULL);
			opt_name_buf[0] = 0;
			opt_value_buf[0] = 0;
			return false;
		}
		int name_pos = 0;
		int value_pos = 0;

		while (next_char != unicode_cp_iterator::EOS && next_char != ',' && next_char != '=')
		{
			CharsetHandler_utf8::utf8_putch(next_char, opt_name_buf, &name_pos, opt_name_buf_size-1);
			get_next_nonws_char();
		}
		if (next_char == '=')
		{
			get_next_nonws_char();
			while (next_char != unicode_cp_iterator::EOS && next_char != ',')
			{
				CharsetHandler_utf8::utf8_putch(next_char, opt_value_buf, &value_pos, opt_value_buf_size-1);
				get_next_nonws_char();
			}
		}
		U_ASSERT(name_pos < opt_name_buf_size);
		U_ASSERT(value_pos < opt_value_buf_size);
		opt_name_buf[name_pos] = 0;
		opt_value_buf[value_pos] = 0;
        return true;
	}
	const char *opt_name()
	{
		return opt_name_buf;
	}
	const char *opt_value()
	{
		return opt_value_buf;
	}
	//empty or omiited value results in true
	bool opt_value_as_bool()
	{
		//FIXME: throw exception for invalid args?
		//FIXME: 00 is true
		if (!opt_value_buf[0])
			return true;
		if ( (opt_value_buf[0] == '0' && !opt_value_buf[1])
			|| (toupper(opt_value_buf[0]) == 'N' && toupper(opt_value_buf[1]) == 'O' && !opt_value_buf[2])
			|| (toupper(opt_value_buf[0]) == 'O' && toupper(opt_value_buf[1]) == 'F' && toupper(opt_value_buf[2]) == 'F' && !opt_value_buf[3])
			)
			return false;
		return true;
	}
};


#endif
