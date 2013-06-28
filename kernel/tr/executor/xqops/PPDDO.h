/*
* File:  PPDDO.h
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef _PPDDO_H
#define _PPDDO_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/sorted_sequence.h"

class PPDDO: public PPIterator
{
private:
    static char* temp_buffer;
    static int buf_lgth;

    int pos;
    sorted_sequence *s;
    xptr ret_val;
    bool atomic_mode;
    PPOpIn child;

    static int get_size_ser(xptr& v1);
    static xptr get_ptr_ser(xptr& v1,int sz);
    static void copy_data_ser_to_buffer(xptr v1,shft shift,int sz);
    static void copy_to_buffer(const void* addr, shft size);
    static void copy_to_buffer(const void* addr, shft shift,shft size);
    static void copy_from_buffer(xptr addr, shft shift,shft size);
    
    inline static void copy_to_buffer(xptr addr, shft size) {
        CHECKP(addr);
        copy_to_buffer(XADDR(addr),size);
    }
    inline static void copy_data_ser_to_buffer(xptr v1,int sz) {
        copy_data_ser_to_buffer(v1, 0, sz);
    }
    inline static void copy_to_buffer(xptr addr, shft shift,shft size) {
        CHECKP(addr);
        copy_to_buffer(XADDR(addr),shift,size);
    }

    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPDDO(dynamic_context *_cxt_,
        operation_info _info_,
        PPOpIn _child_);
    virtual ~PPDDO();

    static int compare_less (xptr v1,xptr v2, const void * Udata);
    static int get_size (xqp_tuple& t, const void * Udata);
    static void serialize (xqp_tuple& t,xptr v1, const void * Udata);
    static void serialize_2_blks (xqp_tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
    static void deserialize (xqp_tuple &t, xptr& v1, const void * Udata);
    static void deserialize_2_blks (xqp_tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
};

#endif /* _PPDDO_H */
