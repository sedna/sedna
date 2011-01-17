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

#include "tr/structures/nodeinterface.h"
#include "tr/mo/indirection.h"

class sequence;

/// Array of int pairs
typedef std::pair<int, int>			int_pair;
typedef std::vector<int_pair>		arr_of_int_pairs;

typedef counted_ptr<char, de_delete_array<char> > str_counted_ptr;
//typedef counted_ptr<protal_node> portalnode_ptr;

/// Used in fn:concat to determine which type of sting to create as result.
/// Possibly, additional review is needed how we manage in-memory strings.
#define MAX_MEM_STR_SIZE	            100

/// Used in casting and serialazation to create lexical atomic tuple cell
/// representation (through executor_globals::mem_str_buf).
/// Size should be the same as maximum lexical representaion of any fixed size
/// atomic value. In general, it means the same value as used in the lexical
/// analyzer.

#define MAX_ATOMIC_LEX_REPR_SIZE        2000


/// Possible types of tuple cell
#define tc_eos                         (uint32_t)(0x80000000) // cell stores end of sequence
#define tc_portal                      (uint32_t)(0x40000000) // cell stores virtual element pointer
#define tc_light_atomic_fix_size       (uint32_t)(0x10000000) // cell stores light atomic value (feats in dynamic memory)
#define tc_light_atomic_var_size       (uint32_t)(0x20000000) // cell stores light atomic value (feats in dynamic memory)
#define tc_heavy_atomic_estr           (uint32_t)(0x04000000) // cell stores heavy atomic value in e_strings (that is stored in VMM memory)
#define tc_heavy_atomic_pstr_short     (uint32_t)(0x08000000) // cell stores heavy atomic value in short pstrings (that is stored in VMM memory)
#define tc_heavy_atomic_pstr_long      (uint32_t)(0x0C000000) // cell stores heavy atomic value in long pstrings (that is stored in VMM memory)
#define tc_safenode                    (uint32_t)(0x03000000) // cell stores safenode
#define tc_node                        (uint32_t)(0x02000000) // cell stores node
#define tc_unsafenode                  (uint32_t)(0x01000000) // cell stores unsafenode


/// Masks for 't' field of tuple cell
#define TC_EOS_MASK                    (uint32_t)(0x80000000)
#define TC_NODE_MASK                   (uint32_t)(0x03000000)
#define TC_ATOMIC_MASK                 (uint32_t)(0x3C000000)
#define TC_PORTAL_MASK                 (uint32_t)(0x40000000)
#define TC_LIGHT_ATOMIC_MASK           (uint32_t)(0x30000000)
#define TC_HEAVY_ATOMIC_MASK           (uint32_t)(0x0C000000)
#define TC_LIGHT_ATOMIC_VAR_SIZE_MASK  (uint32_t)(0x20000000)

#define TC_TYPE_MASK                   (uint32_t)(0xFF000000)
#define TC_XTYPE_MASK                  (uint32_t)(0x00FFFFFF)

/// Tuple cell data ('data' field of tuple_cell)
typedef struct {
    uint64_t x, y;
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
          __________________________//      \\_________________________________________
         //                 //                               \\                        \\
        eos                xptr                             atomic                  portal node
                       //   ||   \\                ________//    \\________
                     safe   ||  tmp-safe          //                      \\
                          unsafe           light atomic               heavy atomic
                                           //      \\                //   ||     \\
                                          //        \\              //    ||      \\
                                        variable   fixed         estr  pstr_short  pstr_long
                                         size       size

100000 -- eos
001000 -- light atomic of variable size
000100 -- light atomic of fixed size
000001 -- heavy atomic (estr)
000010 -- heavy atomic (pstr_short)
000011 -- heavy atomic (pstr_long)
00000011 -- node always indirection (safe)
00000010 -- node indirection only for temporary (tmp-safe)
00000001 -- node always direct node (unsafe)

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
        int64_t
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
        int64_t size;
*/

struct tuple_cell;

extern tuple_cell EMPTY_STRING_TC;

struct sequence_ptr {
    sequence * p;
    int pos;

    sequence_ptr(sequence * ap, int apos) : p(ap), pos(apos) {};
    sequence_ptr(const sequence_ptr & p) : p(p.p), pos(p.pos) { };
//    sequence_ptr& operator= (const sequence_ptr& s) { if (this != &s) { p = s.p; pos = s.pos; } return *this; }
};

// allocator for xs_QName_create; 'void' to avoid changing alloc_func in every signature
inline void *tuple_char_alloc(size_t size)
{
    char *res = new char[size];
    return res;
}

struct tuple_cell
{
private:
    uint32_t t;
    tcdata data;

    void assign(const tuple_cell& tc)
    {
        t = tc.t;
        if (tc.t & TC_LIGHT_ATOMIC_VAR_SIZE_MASK) {
            data.x = data.y = (int64_t)0;
            *(str_counted_ptr*)(&data) = *(str_counted_ptr*)(&(tc.data));
        } else {
            data = tc.data;
        }
    }
    void release()
    {
        if (t & TC_LIGHT_ATOMIC_VAR_SIZE_MASK) {
            ((str_counted_ptr*)(&data))->~str_counted_ptr();
            data.x = data.y = (int64_t) 0;
        }
    }

    void set_size(int64_t _size_) { *(int64_t*)((char*)(&data) + sizeof(xptr)) = _size_; }

public:
    ////////////////////////////////////////////////////////////////////////////
    /// SET AND GET FUNCTIONS (SOME OF THEM ARE VERY DANGEROUS)
    ////////////////////////////////////////////////////////////////////////////
    bool is_eos()          const { return (t & TC_EOS_MASK) != 0; }
    bool is_atomic()       const { return (t & TC_ATOMIC_MASK) != 0; }
    bool is_light_atomic() const { return (t & TC_LIGHT_ATOMIC_MASK) != 0; }
    bool is_heavy_atomic() const { return (t & TC_HEAVY_ATOMIC_MASK) != 0; }
    bool is_portal()       const { return (t & TC_PORTAL_MASK) != 0; }

    bool is_fixed_size_atomic() const { return is_fixed_size_type(get_atomic_type()); };

    /* node types */
    bool is_node()         const { return (t & TC_NODE_MASK) != 0; }
    /* safenode type means that node is stored with indirection always */
    bool is_safenode()     const { return (t & TC_TYPE_MASK) == tc_safenode; }
    /* node type means that node is stored with indirection only for temporary nodes */
    bool is_smartnode()    const { return (t & TC_TYPE_MASK) == tc_node; }
    /* unsafenode type means that node xptr is stored directly (equals xptr type) */
    bool is_unsafenode()   const { return (t & TC_TYPE_MASK) == tc_unsafenode; }

    uint32_t    get_type()        const { return t & TC_TYPE_MASK; }
    xmlscm_type get_atomic_type() const { return t & TC_XTYPE_MASK; }

    sequence_ptr get_portal() const { return *(sequence_ptr*)(&data); };

    xptr get_xptr() const { return *(xptr*)(&data); }

    xptr get_smartnode() const { xptr a = get_xptr(); return isTmpBlock(a) ? indirectionDereferenceCP(a) : a; }
    xptr get_smartnode_indir() const { xptr a = get_xptr(); return isTmpBlock(a) ? a : getIndirectionSafeCP(checkp(a)); }
    xptr get_safenode() const { return indirectionDereferenceCP(get_xptr()); }
    xptr get_unsafenode() const { return get_xptr(); }

    xptr get_node() const {
      switch (t & TC_TYPE_MASK) {
        case tc_safenode : return get_safenode();
        case tc_node : return get_smartnode();
        case tc_unsafenode : return get_unsafenode();
        default: return XNULL;
      }
    }

    xptr get_node_inderection() const {
      switch (t & TC_TYPE_MASK) {
        case tc_safenode : return get_xptr();
        case tc_node : return get_smartnode_indir();
        case tc_unsafenode : return getIndirectionSafeCP(checkp(get_xptr()));
        default: return XNULL;
      }
    }

    bool is_atomic_type(xmlscm_type tt) const { return is_atomic() && (this->t & TC_XTYPE_MASK) == (uint32_t) tt; }

    bool is_boolean_true() const { return is_atomic_type(xs_boolean) && get_xs_boolean(); };

    /// fixed size atomic values
    int64_t            get_xs_integer()  const { return *(int64_t*           )(&data); }
    xs_decimal_t       get_xs_decimal()  const { return *(xs_decimal_t*      )(&data); }
    float              get_xs_float()    const { return *(float*             )(&data); }
    double             get_xs_double()   const { return *(double*            )(&data); }
    bool               get_xs_boolean()  const { return *(bool*              )(&data); }
    xs_packed_datetime get_xs_dateTime() const { return *(xs_packed_datetime*)(&data); }
    xs_packed_duration get_xs_duration() const { return *(xs_packed_duration*)(&data); }

    sequence*          get_sequence_ptr()const {
        U_ASSERT(is_light_atomic() && (t & TC_XTYPE_MASK) == se_sequence);
        return *(sequence**)(&data);
    }

    int get_index() const { return ((sequence_ptr*)(&data))->pos; }
    void* get_binary_data() const { return  (void*              )(&data); }

    /// variable size atomic values
    char*              get_str_mem()     const { return ((str_counted_ptr*)(&data))->get(); }
    xptr               get_str_vmm()     const { return *(xptr*)(&data); }
    size_t             get_strlen_mem()  const { return strlen(get_str_mem()); }
    int64_t            get_strlen_vmm()  const { return *(int64_t*)((char*)(&data) + sizeof(xptr)); }
    int64_t            get_strlen()      const { return is_light_atomic() ? get_strlen_mem() : get_strlen_vmm(); }

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
        data.x = data.y = (uint64_t)0;
    }
    // type field constructor
    explicit tuple_cell(uint32_t _t_) : t(_t_)
    {
        data.x = data.y = (uint64_t)0;
    }
    // all fields constructor
    explicit tuple_cell(uint32_t _t_, const tcdata &_data_) : t(_t_), data(_data_) {}
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
    explicit tuple_cell(const int atype, const xptr &_p_)
    {
        t = atype;
        *(xptr*)(&(data)) = _p_;
    }

    // for sequence element
    explicit tuple_cell(const int atype, const sequence_ptr _p_)
    {
        t = atype;
        data.x = data.y = (int64_t)0;
        *(sequence_ptr*)(&data) = _p_;
    }
  public :
    // for xs:integer atomics
    explicit tuple_cell(int64_t _data_)
    {
        t = tc_light_atomic_fix_size | xs_integer;
		*(int64_t*)(&(data)) = _data_;
    }
    // for xs_decimal atomics
    explicit tuple_cell(xs_decimal_t _data_)
    {
        t = tc_light_atomic_fix_size | xs_decimal;
        *(xs_decimal_t*)(&(data)) = _data_;
    }
    // for xs_float atomics
    explicit tuple_cell(float _data_)
    {
        t = tc_light_atomic_fix_size | xs_float;
        *(float*)(&(data)) = _data_;
    }
    // for xs_double atomics
    explicit tuple_cell(double _data_)
    {
        t = tc_light_atomic_fix_size | xs_double;
        *(double*)(&(data)) = _data_;
    }
    // for xs_boolean atomics
    explicit tuple_cell(bool _data_)
    {
        t = tc_light_atomic_fix_size | xs_boolean;
        *(bool*)(&(data)) = _data_;
    }
    // for xs_date atomics
    explicit tuple_cell(xs_packed_datetime _data_, xmlscm_type _type_)
    {
        t = tc_light_atomic_fix_size | _type_;
        *(xs_packed_datetime*)(&(data)) = _data_;
    }
    // for xs_duration atomics
    explicit tuple_cell(xs_packed_duration _data_, xmlscm_type _type_)
    {
        t = tc_light_atomic_fix_size | _type_;
        *(xs_packed_duration*)(&(data)) = _data_;
    }
    // for se_sequence atomics
    explicit tuple_cell(sequence* _sequence_ptr_)
    {
        t = tc_light_atomic_fix_size | se_sequence;
        data.x = data.y = (int64_t)0;
        *((sequence**)(&data)) = _sequence_ptr_;
    }
    // for variable size light atomics (without deep copy)
    explicit tuple_cell(xmlscm_type _xtype_, char *_str_)
    {
        t = tc_light_atomic_var_size | _xtype_;
        data.x = data.y = (int64_t)0;
        *(str_counted_ptr*)(&data) = str_counted_ptr(_str_);
    }

    // for variable size light atomics (with deep copy)
    explicit tuple_cell(xmlscm_type _xtype_, const char *_str_, bool)
    {
        char *tmp = se_new char[strlen(_str_) + 1];
        strcpy(tmp, _str_);
        t = tc_light_atomic_var_size | _xtype_;
        data.x = data.y = (int64_t)0;
        *(str_counted_ptr*)(&data) = str_counted_ptr(tmp);
    }
    // for heavy atomics
    explicit tuple_cell(uint32_t _t_, xmlscm_type _xtype_, const xptr &_p_, int64_t _size_)
    {
        t = _t_ | _xtype_;
		*(xptr*)(&(data)) = _p_;
        *(int64_t*)((char*)(&data) + sizeof(xptr)) = _size_;
    }


    ////////////////////////////////////////////////////////////////////////////
    /// FACTORIES FOR TUPLE_CELL
    ////////////////////////////////////////////////////////////////////////////
    static tuple_cell eos()
    {
        return tuple_cell();
    }

    static tuple_cell safenode(const xptr &_p_)
    {
        return tuple_cell(tc_safenode, getIndirectionSafeCP(_p_));
    }

    static tuple_cell safenode_indir(const xptr &_p_)
    {
        return tuple_cell(tc_safenode, _p_);
    }

    static tuple_cell node(const xptr &_p_)
    {
        U_ASSERT(_p_ != XNULL);
        return tuple_cell(tc_node, isTmpBlock(_p_) ? getIndirectionSafeCP(_p_) : _p_);
    }

    static tuple_cell node(const Node &node)
    {
        return tuple_cell::node(node.getPtr());
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

    static tuple_cell atomic(int64_t _data_)
    {
        return tuple_cell(_data_);
    }

    static tuple_cell atomic_portal(xptr _data_)
    {
        return tuple_cell(tc_portal | se_xptr, _data_);
    }

    static tuple_cell atomic_portal(sequence * _sequence_, int index)
    {
        return tuple_cell(tc_portal | se_sequence_element, sequence_ptr(_sequence_, index));
    }

    static tuple_cell atomic_se_sequence_element(sequence * _sequence_, int index)
    {
        return tuple_cell(tc_light_atomic_fix_size | se_sequence_element, sequence_ptr(_sequence_, index));
    }

    static tuple_cell atomic_xptr(xptr _data_)
    {
        return tuple_cell(tc_light_atomic_fix_size | se_xptr, _data_);
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

    static tuple_cell atomic(uint32_t _t_, xmlscm_type _xtype_, int64_t _size_, const xptr &_p_)
    {
        return tuple_cell(_t_, _xtype_, _p_, _size_);
    }

    static tuple_cell atomic_pstr(xmlscm_type _xtype_, int64_t _size_, const xptr &_p_)
    {
        uint32_t _t_ = _size_ > PSTRMAXSIZE ? tc_heavy_atomic_pstr_long : tc_heavy_atomic_pstr_short;
        return tuple_cell(_t_, _xtype_, _p_, _size_);
    }

    static tuple_cell atomic_estr(xmlscm_type _xtype_, int64_t _size_, const xptr &_p_)
    {
        return tuple_cell(tc_heavy_atomic_estr, _xtype_, _p_, _size_);
    }

    static tuple_cell atomic_text(CommonTextNode node)
    {
        if (node.isEmpty()) return EMPTY_STRING_TC;
        return tuple_cell::atomic_pstr(xs_string, (size_t) node.getTextSize(), node.getTextPointer());
    }

    static tuple_cell atomic_pi(PINode node)
    {
        node.checkp();
        if (node.isEmptyPI()) return EMPTY_STRING_TC;
        return tuple_cell::atomic_pstr(xs_string, node.getPIDataSize(), node.getPIData());
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
        data.x = (int64_t)0;
        data.y = (int64_t)0;
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
        t = tc_node;
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
            int64_t len = get_strlen_mem();
            data.x = data.y = (int64_t)0;
            set_size(len);
        }
        set_xptr_int(txt_ptr);
    }

    void _adjust_restored_tc(char *_str_)
    {
        data.x = data.y = (int64_t)0;
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
    char* copy_string(char *buf, size_t n) const;

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

    explicit tuple(const tuple_cell tc) : cells_number(1), eos(false) {
        cells = se_new tuple_cell[1];
        this->copy(tc);
    }

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

static const tuple_cell EMPTY_TUPLE_CELL;

#endif /* _TUPLE_H */
