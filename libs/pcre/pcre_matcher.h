#ifndef _PCRE_MATCHER_H
#define _PCRE_MATCHER_H

#include <stdexcept>
#include <string>
#include "pcre_pattern.h"
#include "pcre_matcher_base.h"

template <typename CharIterator, typename iter_off_t>
class PcreMatcher : private PcreMatcherBase<CharIterator, iter_off_t>
{
private:
	int					m_capturecount;
	int					m_ofscount;
	int					m_groups;
	const pcre_extra	*m_re_extra;
	CharIterator		*m_offsets;


	void init() {
		m_re_extra = NULL;
		m_offsets = NULL;
	}
	//reset and free all allocated RE data
	void free_re_data() {
		if (m_re_extra != NULL)
		{
			//XXX - free is not good
			free((void*)m_re_extra);
			m_re_extra = NULL;
		}
	}

	void free_offsets() {
		//XXX - use allocator
		if (m_offsets != NULL)
			delete[] m_offsets;
	}

	//reset match data
	void reset() {
		m_groups = -1;
	}

	void load_re(const pcre * re) {
		free_offsets();
		free_re_data();
		PcreMatcherBase<CharIterator,iter_off_t>::set_re(re);

		int rc = pcre_fullinfo(PcreMatcherBase<CharIterator,iter_off_t>::m_re, m_re_extra, PCRE_INFO_CAPTURECOUNT, &m_capturecount);
		if (rc == 0)
			m_ofscount = (m_capturecount+2)*3; //XXX - +2 probably should be +1
		else
			throw PcreException(std::string("failed to get RE info: ") + PcreException::errname(rc));

		reset();
	}

	//format replacement string using current matched substring
	template <typename OIterator>
	void format(OIterator &out, const char *fmt_start, const char *fmt_end)
	{
		while (fmt_start < fmt_end)
		{
			if (*fmt_start == '$')
			{
				//only $0..$9 are allowed
				++fmt_start;
				if (fmt_start >= fmt_end)
					throw PcreBadFormatException();
				int num = (int)*fmt_start - (int)'0';
				++fmt_start;
				if (num < 0 || num > 9)
					throw PcreBadFormatException();
				if (num >= m_groups ||m_offsets[num<<1] == PcreMatcherBase<CharIterator, iter_off_t>::InvalidOffset)
					continue; //invalid substrings are substituted by zero-length strings

				CharIterator sub_start = m_offsets[(num<<1)];
				int end_ind = (num<<1)+1;
				while (sub_start < m_offsets[end_ind])
				{
					*out = *sub_start;
					++out; ++sub_start;
				}
			}
			else if (*fmt_start == '\\')
			{
				fmt_start++;
				if (fmt_start >= fmt_end)
					throw PcreBadFormatException();

				const char ch = *fmt_start;
				fmt_start++;
				switch (ch)
				{
				case '\\':
				case '$':
					*out = ch;
					++out;
					break;
				default:
					throw PcreBadFormatException();
				}
			}
			else
			{
				*out = *fmt_start;
				++out; ++fmt_start;
			}
		}
	}
public:

	PcreMatcher(const PcrePattern &re) : PcreMatcherBase<CharIterator,iter_off_t>() {
		init();
		load_re(re.m_re);
	}

	~PcreMatcher() {
		free_offsets();
		free_re_data();
	}

	bool matches(const CharIterator &subject_start, const CharIterator &subject_end, 
				const CharIterator &match_start, int options = 0) {
		reset();
		if (m_offsets == NULL)
			m_offsets = new CharIterator[m_ofscount];

		int rc = exec(m_re_extra, subject_start, subject_end, match_start, options, m_offsets, m_ofscount);
		if (rc == PCRE_ERROR_NOMATCH)
			return false; //no need to do anything since we made reset() 
		else if (rc < 0)
			throw PcreException(std::string("match failed: ") + PcreException::errname(rc));

		//matched
		if (rc == 0)
			rc = (m_ofscount/3); //XXX - this should never happen

		m_groups = rc;
		return true;
	}

	int groupCount() const {return m_groups;}
	CharIterator start(int group) const {
		if (group < 0 || group >= m_groups)
			throw PcreException("invalid group number");
		return substring_start(m_offsets, group);
	}
	CharIterator end(int group) const {
		if (group < 0 || group >= m_groups)
			throw PcreException("invalid group number");
		return substring_end(m_offsets, group);
	}

	template <typename OIterator>
	void replaceAll(OIterator &out, const CharIterator &subject_start, const CharIterator &subject_end, 
				const CharIterator &match_start, const char *fmt, int options = 0)
	{
		CharIterator p = match_start;
		const char *fmt_end = fmt + strlen(fmt);
		//TODO - add something like this, but perform check once anyway if no PCRE_NO_UTF8_CHECK is given
		//if ((options & PCRE_UTF8) != 0)
		//	options |= PCRE_NO_UTF8_CHECK; 
		while (matches(subject_start, subject_end, p, options))
		{
			if (m_offsets[0] == m_offsets[1])
			{
				//TODO - add option to allow empty string matching
				throw PcreEmptyStringMatchedException();
			}
			while (p < m_offsets[0])
			{
				*out = *p;
				++out; ++p;
			}

			format(out, fmt, fmt_end);

			p = m_offsets[1];
		}
		while (p < subject_end)
		{
			*out = *p;
			++out; ++p;
		}
	}
};

#endif //_PCRE_MATCHER_H
