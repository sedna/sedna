#ifndef __ITupleSerializer_h__
#define __ITupleSerializer_h__

#include "common/sedna.h"
#include "common/commutil.h"
#include "tr/executor/base/tuple.h"
#include "tr/tr_globals.h"

class ITupleSerializer
{
public:
    virtual size_t serialize(const tuple &t, void *buf) = 0;
    virtual void deserialize(tuple &t, void *buf, size_t size) = 0;
    virtual int compare(void *buf1, size_t size1, void *buf2, size_t size2) = 0;
    //Returns -1 if first tuple less, 0 if they are equal and 1 otherwise
};

#endif /*__ITupleSerializer_h__*/
