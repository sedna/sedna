/*
 * File:  PPOrderBy_serializer.h
 * Copyright (C) 2011 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPOrderBy_serializer_h__
#define __PPOrderBy_serializer_h__

#include "tr/executor/base/PPBase.h"
#include "common/sedna.h"
#include "common/commutil.h"
#include "tr/executor/base/tuple.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/seq_common.h"
#include "tr/tr_globals.h"
#include "tr/executor/base/ITupleSerializer.h"

//FIXME: Don't forget to add Exception in serializer in case of tuple, which size exceeds block size

#define ORB_STRING_PREFIX_SIZE  29

#define ORB_SERIALIZED_STRING_SIZE (sizeof(bool) + ORB_STRING_PREFIX_SIZE + sizeof(char))
//All xs:string based types have ORB_SERIALIZED_STRING_SIZE
//bytes after serialization:
//sizeof(bool) + prefix size + sizeof(char)
//[true ] + [prefix] + ['\0'] or
//[false] + [prefix] + ['\0']
//'true' means that the whole string was serialized (size <= prefix size).
//'false' means we are able to get the whole string using not
//serialize tuple cell with this string.

#define ORB_SERIALIZED_SIZE(t)  (xmlscm_type_size(t) == 0 ? ORB_SERIALIZED_STRING_SIZE : xmlscm_type_size(t))

//Serializer used in order by operation
struct orb_modifier {
    enum orb_empty_status {
        ORB_EMPTY_GREATEST,
        ORB_EMPTY_LEAST,
    };

    enum orb_sort_order {
        ORB_ASCENDING,
        ORB_DESCENDING,
    };

    orb_sort_order   order;
    orb_empty_status status;
    CollationHandler* collation;

    inline std::string to_string() const
    {
        std::string res;
        if (order == ORB_ASCENDING) res += "ascending";
        else res += "descending";
        if (status == ORB_EMPTY_GREATEST) res += " empty greatest";
        else res += " empty least";
        return res;
    }
};

struct orb_common_type
{
    int size;                                       //Size of type in bytes.
    xmlscm_type xtype;                              //One of the atomic built-in types.
    bool initialized;                               //'true' if xtype is defined, else must be 'false'
};

typedef std::vector<orb_modifier>       arr_of_orb_modifier;
typedef std::vector<orb_common_type>    arr_of_common_type;


class TupleSerializer : public ITupleSerializer
{
private:
    int bit_set_offset; //Offset of bit set in serialized representation
    arr_of_common_type *header;                     //Array of common types structures.
    arr_of_orb_modifier *modifiers;
    int64_t *init_pos;  //Position of tuple in initial sequence

    sequence *sort; //Pointer to the initial sequence that must be sorted; Used in string comparing
    bool stable;    //Indicates if stable sort is requested

public:
    TupleSerializer(int _bit_set_offset_,
                    arr_of_orb_modifier *_modifiers_,
                    arr_of_common_type *_header_,
                    bool _stable_,
                    sequence *_sort_,
                    int64_t *pos
                   );

    //Inherited virtual interface methods:
    virtual size_t serialize(const xqp_tuple& t, void* buf);
    virtual void deserialize(xqp_tuple &t, void *buf, size_t size);
    virtual int compare(void *buf1, size_t size1, void *buf2, size_t size2);
};

#endif /*__PPOrderBy_serializer_h__*/
