/*
 * File:  ft_norm.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/ft_norm.h"


//TODO: remove this
//neeeded for pcre includes
#define PCRE_STATIC
#define SUPPORT_UTF8
#define SUPPORT_UCP

#include "pcre/pcre.h"


bool ft_norm_char(int *ch)
{
	int c_cl, c_type, c_case;
	c_cl = ucp_findchar(*ch, &c_type, &c_case);

	if (c_type == ucp_Mn)
	{
		*ch = -1;
		return true;
	}

	if (c_case != 0 && (c_type == ucp_Lt || c_type == ucp_Lu))
		*ch = c_case;

	return c_cl == ucp_L || c_cl == ucp_N;
}



FtStemmer::FtStemmer()
{
}

const char *FtStemmer::stem_word(const char *word, int size)
{
	return (const char *)sb_stemmer_stem(sb, (const sb_symbol *)word, size);
}

FtStemmer::~FtStemmer()
{
	if (sb != NULL)
	{
		sb_stemmer_delete(sb);
		sb = NULL;
	}
}

FtStemmer *FtStemmer::get(const char *lang)
{
	FtStemmer *s = new FtStemmer();
	s->sb = sb_stemmer_new(lang, "UTF_8");
	if (s->sb == NULL)
	{
		delete s;
		return NULL;
	}
	return s;
}

void FtStemmer::release(FtStemmer *stemmer)
{
	delete stemmer;
}
