/*
 * File:  ft_norm.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_NORM_H
#define _FT_NORM_H

#include "tr/ft/libstemmer/libstemmer.h"

//returns true if char could be a part of some word
bool ft_norm_char(int *ch);

class FtStemmer
{
private:
	struct sb_stemmer *sb;
	FtStemmer();
	~FtStemmer();
public:
	const char *stem_word(const char *word, int size);

	static FtStemmer *get(const char *lang);
	static void release(FtStemmer *stemmer);
};

#endif
