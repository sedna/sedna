/*
 * File:  ft_types.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef FT_TYPES_H_
#define FT_TYPES_H_

#include <vector>

#include "common/sedna.h"
#include "common/xptr.h"
#include "common/commutil.h"

#include "tr/structures/rbtree.h"
#include "tr/structures/schema.h"
#include "tr/executor/base/xsd.h"

typedef uint64_t ft_acc_uint_t; //accesor xptrs stored as unsigned integers
#define FT_XPTR_TO_UINT(x) (x.to_logical_int())
#define FT_UINT_TO_XPTR(x) (logical_int_to_xptr(x))
//#define FT_XPTR_TO_UINT(x) (x.to_uint64())
//#define FT_UINT_TO_XPTR(x) (uint64_to_xptr(x))
#define FT_ACC_UINT_NULL (FT_XPTR_TO_UINT(XNULL))

typedef int ft_word_ind_t;

//this length is in bytes, not characters
//words with length more than this are truncated
#define FT_MAX_WORD_LENGTH 150

/*
  types of words in the index:
    word    - some word, index is the number of this word
	tag*    - opening tag, index is the index of the word that is (or could be) right after this tag
	tag/    - closing tag, index is the index of the word that is (or could be) right after this tag
 */
#define FT_TAG_OPEN_MARKER     '*'
#define FT_TAG_CLOSE_MARKER    '/'

#define FT_NOSTEM_MARKER       '='

typedef float ft_float;

enum ft_index_type
{
    ft_xml,
    ft_xml_hl,
    ft_string_value,
    ft_delimited_value,
    ft_customized_value
};

enum ft_index_impl
{
    ft_ind_dtsearch,
    ft_ind_native,
	ft_ind_undefined
};

enum ft_stem_type
{
	ftst_default,
	ftst_both
};

struct ft_custom_cell
{
    xmlns_ptr ns_local;
    char* local;
    ft_index_type cm;

    inline xmlns_ptr get_xmlns() const {
        return ns_local;
    }

    inline xsd::QName getQName() const { return xsd::QName::createUnchecked(ns_local, local); }

    inline ft_custom_cell() : ns_local(NULL_XMLNS) {};

    inline ft_custom_cell(xsd::QName qname, ft_index_type _cm) :
            ns_local(qname.getXmlNs()), local(NULL), cm(_cm)
    {
        local = new char[strlen(qname.getLocalName()) + 1];
        strcpy(local, qname.getLocalName());
    };

    inline bool less( ft_custom_cell *p1)
    {
        int val= strcmpex(this->local,p1->local);
        if (val<0) return true;
        if (val>0) return false;
        return ((ptrdiff_t) get_xmlns() < (ptrdiff_t) p1->get_xmlns());
    }

    inline bool equals( ft_custom_cell *p1)
    {
        return (strcmpex(this->local,p1->local)==0 && ((ptrdiff_t) get_xmlns() == (ptrdiff_t) p1->get_xmlns()));
    }

    inline bool less(const void* p1, const void* p2)
    {
        int val= strcmpex(local,(char*)p1);
        if (val<0) return true;
        if (val>0) return false;
        return ((ptrdiff_t) get_xmlns() < (ptrdiff_t) p2);
    }

    inline bool equals(const void* p1, const void* p2)
    {
        return (strcmpex(this->local,(char*)p1)==0 && (ptrdiff_t) get_xmlns() == (ptrdiff_t) p2);
    }
};

typedef sedna_rbtree< ft_custom_cell > ft_custom_tree_t;
typedef std::pair<xsd::QName, ft_index_type> ft_index_pair_t;
typedef std::vector< ft_index_pair_t > ft_index_template_t;

class FtWordsScanner
{
public:
	//return current word or NULL if no more words
	virtual const char *cur_word() = 0;
	virtual void next_word() = 0;
	virtual ~FtWordsScanner() {}
};

class FtScanner
{
protected:
	ft_acc_uint_t cur_acc_i;
public:
	ft_acc_uint_t get_cur_acc_i() const {
		return cur_acc_i;
	}
	bool at_end() const {
		return (cur_acc_i == FT_ACC_UINT_NULL);
	}
};

#endif /* FT_TYPES_H_ */
