#ifndef _PCRE_PATTERN_H
#define _PCRE_PATTERN_H

#include "pcre.h"


class PcreException : public std::runtime_error
{
public:
	PcreException(const std::string &msg) : std::runtime_error(msg) {}

	static inline const char * errname(int rc) {
		switch (rc) {
#define errcase(x) case x: return #x;
		errcase(PCRE_ERROR_NOMATCH)		
		errcase(PCRE_ERROR_NULL)
		errcase(PCRE_ERROR_BADOPTION)
		errcase(PCRE_ERROR_BADMAGIC)
		errcase(PCRE_ERROR_UNKNOWN_NODE)
		errcase(PCRE_ERROR_NOMEMORY)
		errcase(PCRE_ERROR_NOSUBSTRING)
		errcase(PCRE_ERROR_MATCHLIMIT)
		errcase(PCRE_ERROR_CALLOUT)
		errcase(PCRE_ERROR_BADUTF8)
		errcase(PCRE_ERROR_BADUTF8_OFFSET)
		errcase(PCRE_ERROR_PARTIAL)
		errcase(PCRE_ERROR_BADPARTIAL)
		errcase(PCRE_ERROR_INTERNAL)
		errcase(PCRE_ERROR_BADCOUNT)
		default:
			return "unknown error " + rc;

#undef errcase
		}
	}
};

class PcreCompileException : public PcreException
{
public:
	int error_offset;

	PcreCompileException(const char *errorptr, int ofs) : PcreException(errorptr), error_offset(ofs) {}
};

class PcreEmptyStringMatchedException : public PcreException
{
public:
	PcreEmptyStringMatchedException() : PcreException("Regular expression matched an empty string") {}
};


class PcrePattern
{
public:
	//this field is public because we can't make PcrePattern a friend of PcreMatcher<> (coz' it's template class)
	pcre *m_re;

	PcrePattern(const char *pattern, int options = 0, const unsigned char *tables = NULL) {
		const char *errorptr;
		int erroroffset;
		m_re = pcre_compile(pattern, options, &errorptr, &erroroffset, tables);
		if (m_re == NULL)
			throw PcreCompileException(errorptr, erroroffset);
	}

	~PcrePattern() {
		//XXX
		if (m_re)
			pcre_free(m_re);
	}
};

#endif //_PCRE_PATTERN_H
