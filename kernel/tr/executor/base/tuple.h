/*
 * File:  tuple.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _TUPLE_H
#define _TUPLE_H

#include <string>

#include "common/sedna.h"

#include "common/counted_ptr.h"
#include "tr/executor/base/xs_decimal_t.h"
#include "tr/executor/base/XMLDateTime.h"
#include "tr/pstr/pstr.h"

#include "tr/structures/nodes.h"
#include "tr/crmutils/node_utils.h"
#include "tr/mo/indirection.h"

class sequence;

/// Array of int pairs
typedef std::pair<int, int>			int_pair;
typedef std::vector<int_pair>		arr_of_int_pairs;


typedef counted_ptr<char> str_counted_ptr;


/// Used in fn:concat to determine which type of sting to create as result.
/// Possibly, additional review is needed how we manage in-memory strings.
#define MAX_MEM_STR_SIZE	            100

/// Used in casting and serialazation to create lexical atomic tuple cell
/// representation (through tr_globals::mem_str_buf).
/// Size should be the same as maximum lexical representaion of any fixed size 
/// atomic value. In general, it means the same value as used in the lexical 
/// analyzer (XQueryDLGLexer::bufsize). 
#define MAX_ATOMIC_LEX_REPR_SIZE        2000


/// Possible types of tuple cell
#define tc_eos                         (__uint32)(0x80000000) // cell stores end of sequence
#define tc_xptr                        (__uint32)(0x40000000) // cell stores xptr
#define tc_safenode                    (__uint32)(0x43000000) // cell stores safenode
#define tc_node                        (__uint32)(0x42000000) // cell stores node
#define tc_unsafenode                  (__uint32)(0x41000000) // cell stores unsafenode
#define tc_light_atomic_fix_size       (__uint32)(0x10000000) // cell stores light atomic value (feats in dynamic memory)
#define tc_light_atomic_var_size       (__uint32)(0x20000000) // cell stores light atomic value (feats in dynamic memory)
#define tc_heavy_atomic_estr           (__uint32)(0x04000000) // cell stores heavy atomic value in e_strings (that is stored in VMM memory)
#define tc_heavy_atomic_pstr_short     (__uint32)(0x08000000) // cell stores heavy atomic value in short pstrings (that is stored in VMM memory)
#define tc_heavy_atomic_pstr_long      (__uint32)(0x0C000000) // cell stores heavy atomic value in long pstrings (that is stored in VMM memory)


/// Masks for 't' field of tuple cell
#define TC_EOS_MASK                    (__uint32)(0x80000000)
#define TC_NODE_MASK                   (__uint32)(0x40000000)
#define TC_ATOMIC_MASK                 (__uint32)(0x3C000000)
#define TC_LIGHT_ATOMIC_MASK           (__uint32)(0x30000000)
#define TC_HEAVY_ATOMIC_MASK           (__uint32)(0x0C000000)
#define TC_LIGHT_ATOMIC_VAR_SIZE_MASK  (__uint32)(0x20000000)

#define TC_TYPE_MASK                   (__uint32)(0xFF000000)
#define TC_XTYPE_MASK                  (__uint32)(0x00FFFFFF)
#define TC_SUBTYPE_MASK                (__uint32)(0x03000000)

#define TC_UNSAFENODE                  (__uint32)(0x41000000)
#define TC_TMPSAFENODE                 (__uint32)(0x42000000)
#define TC_SAFENODE                    (__uint32)(0x43000000)

/// Tuple cell data ('data' field of tuple_cell)
typedef struct {
    __uint64 x, y;
} tcdata;



/**
Tuple cell is a part of tuple, which is a structure passed between QEP operations.

Tuple Cell Structure
~~~~~~~~~~~~~~~~~~~~
Tuple cell contains two fields -- 't' and 'data'.
't' indicates type information of tuple cell (what it stores and of which type),
'data' stores some information depending on 't'.

Lets look at 't' as an array of 32-bits.

00000000000000000000000000000000
|____||________________________|

First 6 bits are used for encoding tuple cell type (see below). Other 26 could be
used for encoding type of the stored entity (atomic value for now) according to
XML Schema. Today 16 bits are used for representing values of type xmlscm_type.


First 6 bits of 't'
~~~~~~~~~~~~~~~~~~~
bits:    X                  X               X       X                      XX


                                entity in tuple cell
                                     //    \\
          __________________________//      \\________________
         //                 //                               \\
        eos                xptr                             atomic
                       //   ||   \\                ________//    \\________
                     safe   ||  tmp-safe          //                      \\
                          unsafe           light atomic               heavy atomic
                                           //      \\                //   ||     \\
                                          //        \\              //    ||      \\
                                        variable   fixed         estr  pstr_short  pstr_long
                                         size       size

100000 -- eos
010000 -- xptr
001000 -- light atomic of variable size
000100 -- light atomic of fixed size
000001 -- heavy atomic (estr)
000010 -- heavy atomic (pstr_short)
000011 -- heavy atomic (pstr_long)

Special subtypes for "node" in 7-8 bits:

01000011 -- always indirection (safe)
01000010 -- indirection only for temporary (tmp-safe)
01000001 -- always direct node (unsafe)
01000000 -- "untyped" node (any xptr)

'eos' stands for END OF SEQUENCE.

Light atomic value is an atomic value (according to XPath/XQuery Data Model) that feats in
dynamic memory. It could be of two types -- values of small fixed size (less or equal to
16 bytes) that are stored in 'data' field and values of variable size that are stored in
main memory outside tuple cell (tuple cell stores a pointer to it). Variable size light
atomic value are null-terminated values.

Heavy atomic value is an atomic value (according to XPath/XQuery Data Model) that is stored
in VMM memory.


'data' Field According to Type of Tuple Cell
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. eos
   'data' stores nothing.

2. node
   'data' stores
        xptr p;

3. light atomic of variable size
   'data' stores
        str_counted_ptr s;

4. light atomic of fixed size
   'data' stores
        __int64
    or
        xs_decimal_t
    or
        float
    or
        double
    or
        bool
    or
        xs_packed_datetime
    or
        xs_packed_duration
    depending on xmlscm_type of atomic value.

5. heavy atomic (estr, pstr_short, pstr_long)
   'data' stores
        xptr p;
        __int64 size;
*/

struct tuple_cell;

extern tuple_cell EMPTY_STRING_TC;

struct tuple_cell
{
private:
    __uint32 t;
    tcdata data;

    void assign(const tuple_cell& tc)
    {
        t = tc.t;
        if (tc.t & TC_LIGHT_ATOMIC_VAR_SIZE_MASK)
        {
            data.x = data.y = (__int64)0;
            *(str_counted_ptr*)(&data) = *(str_counted_ptr*)(&(tc.data));
        }
        else
            data = tc.data;
    }
    void release()
    {
        if (t & TC_LIGHT_ATOMIC_VAR_SIZE_MASK)
            ((str_counted_ptr*)(&data))->~str_counted_ptr();
    }

    void set_size(__int64 _size_) { *(__int64*)((char*)(&data) + sizeof(xptr)) = _size_; }

public:
    ////////////////////////////////////////////////////////////////////////////
    /// SET AND GET FUNCTIONS (SOME OF THEM ARE VERY DANGEROUS)
    ////////////////////////////////////////////////////////////////////////////
    bool is_eos()          const { return t & TC_EOS_MASK; }
    bool is_atomic()       const { return t & TC_ATOMIC_MASK; }
    bool is_light_atomic() const { return t & TC_LIGHT_ATOMIC_MASK; }
    bool is_heavy_atomic() const { return t & TC_HEAVY_ATOMIC_MASK; }

    /* node types */

    bool is_xptr()         const { return t & TC_NODE_MASK; }

    /* safenode type means that node is stored with indirection always */
    bool is_safenode()     const { return (t & TC_TYPE_MASK) == TC_SAFENODE; }
    /* node type means that node is stored with indirection only for temporary nodes */
    bool is_node()         const { return (t & TC_TYPE_MASK) == TC_TMPSAFENODE; }
    /* unsafenode type means that node xptr is stored directly (equals xptr type) */
    bool is_unsafenode()  const { return (t & TC_TYPE_MASK) == TC_UNSAFENODE; }

    bool is_anynode()     const { return (t & TC_TYPE_MASK) && (t & TC_SUBTYPE_MASK); }

    __uint32           get_type()        const { return t & TC_TYPE_MASK; }
    xmlscm_type        get_atomic_type() const { return t & TC_XTYPE_MASK; }

    xptr               get_xptr()        const { return *(xptr*)(&data); }
    xptr               get_node()        const { xptr a = get_xptr(); return isTmpBlock(a) ? indirectionDereferenceCP(a) : a; }
    xptr               get_safenode()    const { return indirectionDereferenceCP(get_xptr()); }
    xptr               get_unsafenode()  const { return get_xptr(); }

    xptr               get_node_smart()  const {
      switch (t & TC_TYPE_MASK) {
        case TC_SAFENODE : return get_safenode();
        case TC_TMPSAFENODE : return get_node();
        case TC_UNSAFENODE : return get_unsafenode();
      }
    }


    /// fixed size atomic values
    __int64            get_xs_integer()  const { return *(__int64*           )(&data); }
    xs_decimal_t       get_xs_decimal()  const { return *(xs_decimal_t*      )(&data); }
    float              get_xs_float()    const { return *(float*             )(&data); }
    double             get_xs_double()   const { return *(double*            )(&data); }
    bool               get_xs_boolean()  const { return *(bool*              )(&data); }
    xs_packed_datetime get_xs_dateTime() const { return *(xs_packed_datetime*)(&data); }
    xs_packed_duration get_xs_duration() const { return *(xs_packed_duration*)(&data); }

    sequence*          get_sequence_ptr()const { return *(sequence**         )(&data); }
    void*              get_binary_data() const { return  (void*              )(&data); }


    /// variable size atomic values
    char*              get_str_mem()     const { return ((str_counted_ptr*)(&data))->get(); }
    xptr               get_str_vmm()     const { return *(xptr*)(&data); }
    __int64            get_strlen_mem()  const { return (__int64)strlen(get_str_mem()); }
    __int64            get_strlen_vmm()  const { return *(__int64*)((char*)(&data) + sizeof(xptr)); }
    __int64            get_strlen()      const { return is_light_atomic() ? get_strlen_mem() : get_strlen_vmm(); }

    str_counted_ptr    get_str_ptr()     const { return *(str_counted_ptr*)(&data); }

    ////////////////////////////////////////////////////////////////////////////
    /// CONSTRUCTORS FOR TUPLE_CELL
    ////////////////////////////////////////////////////////////////////////////
    // destructor
    ~tuple_cell()
    {
        release();
    }
    // default constructor
    tuple_cell() : t(tc_eos)
    {
        data.x = data.y = (__uint64)0;
    }
    // type field constructor
    tuple_cell(__uint32 _t_) : t(_t_)
    {
        data.x = data.y = (__uint64)0;
    }
    // all fields constructor
    tuple_cell(__uint32 _t_, const tcdata &_data_) : t(_t_), data(_data_) {}
    // copy constructor
    tuple_cell(const tuple_cell& tc)
    {
        assign(tc);
    }
    // assign operator
    tuple_cell& operator=(const tuple_cell& tc)
    {
        release();
        assign(tc);
        return *this;
    }
    // for nodes

  private :
    explicit tuple_cell(const int subtype, const xptr &_p_)
    {
        t = subtype;
        *(xptr*)(&(data)) = _p_;
    }
  public :

    // for xs:integer atomics
    tuple_cell(__int64 _data_)
    {
        t = tc_light_atomic_fix_size | xs_integer;
		*(__int64*)(&(data)) = _data_;
    }
    // for xs_decimal atomics
    tuple_cell(xs_decimal_t _data_)
    {
        t = tc_light_atomic_fix_size | xs_decimal;
        *(xs_decimal_t*)(&(data)) = _data_;
    }
    // for xs_float atomics
    tuple_cell(float _data_)
    {
        t = tc_light_atomic_fix_size | xs_float;
        *(float*)(&(data)) = _data_;
    }
    // for xs_double atomics
    tuple_cell(double _data_)
    {
        t = tc_light_atomic_fix_size | xs_double;
        *(double*)(&(data)) = _data_;
    }
    // for xs_boolean atomics
    tuple_cell(bool _data_)
    {
        t = tc_light_atomic_fix_size | xs_boolean;
        *(bool*)(&(data)) = _data_;
    }
    // for xs_date atomics
    tuple_cell(xs_packed_datetime _data_, xmlscm_type _type_)
    {
        t = tc_light_atomic_fix_size | _type_;
        *(xs_packed_datetime*)(&(data)) = _data_;
    }
    // for xs_duration atomics
    tuple_cell(xs_packed_duration _data_, xmlscm_type _type_)
    {
        t = tc_light_atomic_fix_size | _type_;
        *(xs_packed_duration*)(&(data)) = _data_;
    }
    // for se_sequence atomics
    tuple_cell(sequence* _sequence_ptr_)
    {
        t = tc_light_atomic_fix_size | se_sequence;
        data.x = data.y = (__int64)0;
        *((sequence**)(&data)) = _sequence_ptr_;
    }
    // for variable size light atomics (without deep copy)
    tuple_cell(xmlscm_type _xtype_, char *_str_)
    {
        t = tc_light_atomic_var_size | _xtype_;
        data.x = data.y = (__int64)0;
        *(str_counted_ptr*)(&data) = str_counted_ptr(_str_);
    }
    // for variable size light atomics (with deep copy)
    tuple_cell(xmlscm_type _xtype_, const char *_str_, bool)
    {
        char *tmp = se_new char[strlen(_str_) + 1];
        strcpy(tmp, _str_);
        t = tc_light_atomic_var_size | _xtype_;
        data.x = data.y = (__int64)0;
        *(str_counted_ptr*)(&data) = str_counted_ptr(tmp);
    }
    // for heavy atomics
    tuple_cell(__uint32 _t_, xmlscm_type _xtype_, const xptr &_p_, __int64 _size_)
    {
        t = _t_ | _xtype_;
		*(xptr*)(&(data)) = _p_;
        *(__int64*)((char*)(&data) + sizeof(xptr)) = _size_;
    }


    ////////////////////////////////////////////////////////////////////////////
    /// FACTORIES FOR TUPLE_CELL
    ////////////////////////////////////////////////////////////////////////////
    static tuple_cell eos()
    {
        return tuple_cell();
    }

    static tuple_cell from_xptr(const xptr &_p_)
    {
        return tuple_cell(tc_xptr, _p_);
    }

    static tuple_cell safenode(const xptr &_p_)
    {
        return tuple_cell(tc_safenode, getIndirectionSafeCP(_p_));
    }

    static tuple_cell node(const xptr &_p_)
    {
        U_ASSERT(_p_ != XNULL);
        return tuple_cell(tc_node, isTmpBlock(_p_) ? getIndirectionSafeCP(_p_) : _p_);
    }

    static tuple_cell node_indir(const xptr &_p_)
    {
        U_ASSERT(_p_ != XNULL);
        return tuple_cell(tc_node, isTmpBlock(_p_) ? _p_ : indirectionDereferenceCP(_p_));
    }

    static tuple_cell unsafenode(const xptr &_p_)
    {
        return tuple_cell(tc_unsafenode, _p_);
    }

    static tuple_cell atomic(__int64 _data_)
    {
        return tuple_cell(_data_);
    }

    static tuple_cell atomic(xs_decimal_t _data_)
    {
        return tuple_cell(_data_);
    }

    static tuple_cell atomic(float _data_)
    {
        return tuple_cell(_data_);
    }

    static tuple_cell atomic(double _data_)
    {
        return tuple_cell(_data_);
    }

    static tuple_cell atomic(bool _data_)
    {
        return tuple_cell(_data_);
    }

    static tuple_cell atomic(xs_packed_datetime _data_, xmlscm_type _type_)
    {
        return tuple_cell(_data_, _type_);
    }

    static tuple_cell atomic(xs_packed_duration _data_, xmlscm_type _type_)
    {
        return tuple_cell(_data_, _type_);
    }

    static tuple_cell atomic(xmlscm_type _xtype_, char *_str_)
    {
        return tuple_cell(_xtype_, _str_);
    }

    static tuple_cell atomic_deep(xmlscm_type _xtype_, const char *_str_)
    {
        return tuple_cell(_xtype_, _str_, true);
    }

    static tuple_cell atomic(__uint32 _t_, xmlscm_type _xtype_, __int64 _size_, const xptr &_p_)
    {
        return tuple_cell(_t_, _xtype_, _p_, _size_);
    }

    static tuple_cell atomic_pstr(xmlscm_type _xtype_, __int64 _size_, const xptr &_p_)
    {
        __uint32 _t_ = _size_ > PSTRMAXSIZE ? tc_heavy_atomic_pstr_long : tc_heavy_atomic_pstr_short;
        return tuple_cell(_t_, _xtype_, _p_, _size_);
    }

    static tuple_cell atomic_estr(xmlscm_type _xtype_, __int64 _size_, const xptr &_p_)
    {
        return tuple_cell(tc_heavy_atomic_estr, _xtype_, _p_, _size_);
    }

    inline static tuple_cell atomic_text(xptr node)
    {
        CHECKP(node);
        if (isTextEmpty(T_DSC(node))) return EMPTY_STRING_TC;
        return tuple_cell::atomic_pstr(xs_string, getTextSize(T_DSC(node)), getTextPtr(T_DSC(node)));
    }

    inline static tuple_cell atomic_pi(xptr node)
    {
        CHECKP(node);
        size_t size = (size_t) getTextSize(PI_DSC(node));
        size_t target_size = PI_DSC(node)->target;
        if (size == 0 || target_size == size) return EMPTY_STRING_TC;
        ++target_size;
        U_ASSERT(size < PSTRMAXSIZE);
        return tuple_cell::atomic_pstr(xs_string, size - target_size, getTextPtr(T_DSC(node)) + target_size);
    }

    static tuple_cell atomic_se_separator()
    {
        return tuple_cell(tc_light_atomic_fix_size | se_separator);
    }

    static tuple_cell atomic_se_sequence(sequence* _sequence_)
    {
        return tuple_cell(_sequence_);
    }

    ////////////////////////////////////////////////////////////////////////////
    /// FUNCTIONS THAT CHANGE THE VALUE OF TUPLE_CELL (DANGEROUS!!!)
    ////////////////////////////////////////////////////////////////////////////
    void set_xtype(xmlscm_type _xtype_) { t = (t & TC_TYPE_MASK) | _xtype_; }

    void set_eos()
    {
        release();
        t = tc_eos;
        data.x = (__int64)0;
        data.y = (__int64)0;
    }

    void set_unsafenode(const xptr &_p_)
    {
        release();
        t = tc_unsafenode;
        *(xptr*)(&data) = _p_;
    }

    void set_node(const xptr &_p_)
    {
        release();
        t = tc_node;
        U_ASSERT(_p_ != XNULL);
        *(xptr*)(&data) = isTmpBlock(_p_) ? getIndirectionSafeCP(_p_) : _p_;
    }

    void set_xptr(const xptr &_p_)
    {
        release();
        t = tc_xptr;
        *(xptr*)(&data) = isTmpBlock(_p_) ? getIndirectionSafeCP(_p_) : _p_;
    }

    void set_safenode(const xptr &_p_)
    {
        release();
        t = tc_safenode;
        *(xptr*)(&data) = getIndirectionSafeCP(_p_);
    }

  private :
    void set_xptr_int(const xptr &_p_) { *(xptr*)(&data) = _p_; }
  public:

    void _adjust_serialized_tc(const xptr &txt_ptr)
    {
        if (is_light_atomic())
        {
            __int64 len = get_strlen_mem();
            data.x = data.y = (__int64)0;
            set_size(len);
        }
        set_xptr_int(txt_ptr);
    }

    void _adjust_restored_tc(char *_str_)
    {
        data.x = data.y = (__int64)0;
        *(str_counted_ptr*)(&data) = str_counted_ptr(_str_);
    }

    ////////////////////////////////////////////////////////////////////////////
    /// ADDITIONAL FUNCTIONS
    ////////////////////////////////////////////////////////////////////////////
    void print(bool b = true) const;

    /// FIXME: Add good comment to these functions (how to call, memory management)

    static tuple_cell make_sure_light_atomic(const tuple_cell& tc);
    /// copy content of the string to buf (!!! FIXME: not very well designed approach...)
    char* copy_string(char *buf) const;
    char* copy_string(char *buf, __int64 n) const;

    std::string type2string() const;
};



struct tuple
{
    int cells_number;	// number of cells in the array
    tuple_cell *cells;	// array of cells
    bool eos;			// is eos?

    tuple() : cells_number(0), cells(NULL), eos(true) {}
    tuple(int _n_) : cells_number(_n_), eos(false) { cells = se_new tuple_cell[_n_]; }
    tuple(const tuple &t);
    tuple &operator=(const tuple& t);

    void copy(const tuple &t)
    {
        eos = t.eos;
        if (!eos) for (int i = 0; i < cells_number; i++) cells[i] = t.cells[i];
    }
    void copy(const tuple_cell &tc)
    {
        if (cells_number != 1) throw USER_EXCEPTION2(SE1003, "Cannot construct tuple from tuple cell (size mismatch)");
        eos = (tc.get_type() == tc_eos);
        cells[0] = tc;
    }
    void copy(const tuple_cell &tc1, const tuple_cell &tc2)
    {
        eos = false;
        if (cells_number != 2) throw USER_EXCEPTION2(SE1003, "Cannot construct tuple from tuple cell (size mismatch)");
        cells[0] = tc1;
        cells[1] = tc2;
    }
    void copy(const tuple &t, const tuple_cell &tc)
    {
        eos = false;
        if (t.cells_number + 1 != cells_number)
            throw USER_EXCEPTION2(SE1003, "Cannot construct tuple from tuple and tuple cell (size mismatch)");
        for (int i = 0; i < t.cells_number; i++) cells[i] = t.cells[i];
        cells[cells_number - 1] = tc;
    }
    void copy(const tuple &t, const tuple_cell &tc1, const tuple_cell &tc2)
    {
        eos = false;
        if (t.cells_number + 2 != cells_number)
            throw USER_EXCEPTION2(SE1003, "Cannot construct tuple from tuple and two tuple cells (size mismatch)");
        for (int i = 0; i < t.cells_number; i++) cells[i] = t.cells[i];
        cells[cells_number - 2] = tc1;
        cells[cells_number - 1] = tc2;
    }
    void copy(const tuple &s1, const tuple &s2, const arr_of_int_pairs &order)
    {
        eos = false;
        if (s1.cells_number + s2.cells_number != cells_number)
            throw USER_EXCEPTION2(SE1003, "Cannot construct tuple from two tuples (size mismatch)");
        const tuple* arr[2] = {&s1, &s2};
        for (int i = 0; i < cells_number; i++) cells[i] = arr[order[i].first - 1]->cells[order[i].second - 1];
    }
    ~tuple() { clear(); }
    void clear()
    {
		if (cells != NULL) delete [] cells;
    }
    bool is_eos() const { return eos; }
    void set_eos() { eos = true; }

    void print() const;
};


#endif
