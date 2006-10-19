/*
 * File:  tuple.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _TUPLE_H
#define _TUPLE_H

#include "sedna.h"

#include "counted_ptr.h"
#include "nodes.h"
#include "xs_decimal_t.h"
#include "pstr.h"
#include "XMLDateTime.h"

/// Array of int pairs
typedef std::pair<int, int>			int_pair;
typedef std::vector<int_pair>		arr_of_int_pairs;


typedef counted_ptr<char> str_counted_ptr;


/*
// type of a cell.
enum tuple_cell_type {
     tc_eos,			        // cell stores end of sequence
     tc_node,			        // cell stores node
     tc_light_atomic,	        // cell stores light atomic value (feats in dynamic memory)
     tc_heavy_atomic_estr,	    // cell stores heavy atomic value in e_strings (that is stored in VMM memory)
     tc_heavy_atomic_pstr_short,// cell stores heavy atomic value in short pstrings (that is stored in VMM memory)
     tc_heavy_atomic_pstr_long  // cell stores heavy atomic value in long pstrings (that is stored in VMM memory)
                     };
*/


#define tc_eos                      (__uint32)(0x80000000) // cell stores end of sequence
#define tc_node                     (__uint32)(0x40000000) // cell stores node
#define tc_light_atomic_fix_size    (__uint32)(0x10000000) // cell stores light atomic value (feats in dynamic memory)
#define tc_light_atomic_var_size    (__uint32)(0x20000000) // cell stores light atomic value (feats in dynamic memory)
#define tc_heavy_atomic_estr        (__uint32)(0x04000000) // cell stores heavy atomic value in e_strings (that is stored in VMM memory)
#define tc_heavy_atomic_pstr_short  (__uint32)(0x08000000) // cell stores heavy atomic value in short pstrings (that is stored in VMM memory)
#define tc_heavy_atomic_pstr_long   (__uint32)(0x0C000000) // cell stores heavy atomic value in long pstrings (that is stored in VMM memory)


#define TC_TYPE_MASK                (__uint32)(0xFC000000)
#define TC_XTYPE_MASK               (__uint32)(0x03FFFFFF)
/*
struct tcdata
{
private:
    __uint32 a, b, c;
public:
  tcdata(): a(0), b(0), c(0)
  {}
  tcdata(int value): a(value), b(0), c(0)
  {}
};
*/
typedef struct {
    __uint64 x, y;
} tcdata;
//typedef __uint8[16] tcdata;


#define MAX_MEM_STR_SIZE	100

#define TC_EOS_MASK                    (__uint32)(0x80000000)
#define TC_NODE_MASK                   (__uint32)(0x40000000)
#define TC_ATOMIC_MASK                 (__uint32)(0x3C000000)
#define TC_LIGHT_ATOMIC_MASK           (__uint32)(0x30000000)
#define TC_HEAVY_ATOMIC_MASK           (__uint32)(0x0C000000)
#define TC_LIGHT_ATOMIC_VAR_SIZE_MASK  (__uint32)(0x20000000)

/*
1. tc_heavy_atomic_estr, tc_heavy_atomic_pstr_short, tc_heavy_atomic_pstr_long
xptr p;
int size;

2. tc_light_atomic_var_size
str_counted_ptr str_ptr;

3. tc_light_atomic_fix_size
tcdata data; // converted to specific type

4. tc_node
xptr p;
*/

struct tuple_cell
{
private:
    __uint32 t;
    tcdata data;

/*
    // tuple cell stores lots of things according to type
    // tc_eos:			just eos
    // tc_node:			data stores xptr to the node
    // tc_light_atomic:	xtype stores type of the atomic value. If xtype is 
    //                  'string' then str_ptr points to string in Windows' 
    //                  managed memory, else 'data' field is used for storing 
    //                  data.
    // tc_heavy_atomic: xtype stores type of the atomic value. 'data' stores 
    //                  xptr to the text, 'size' stores size of the text
    tuple_cell_type type;	// type of the tuple_cell
    tcdata data;			// stores lots of things in concordance with type. 
    xmlscm_type xtype;     	// type of atomic value (if atomic value is stored)
    str_counted_ptr str_ptr;
    int size;
*/

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

    void set_size(int _size_) { *(int*)((char*)(&data) + sizeof(xptr)) = _size_; }

public:
    ////////////////////////////////////////////////////////////////////////////
    /// SET AND GET FUNCTIONS (SOME OF THEM ARE VERY DANGEROUS)
    ////////////////////////////////////////////////////////////////////////////
    bool is_eos()          const { return t & TC_EOS_MASK; }
    bool is_node()         const { return t & TC_NODE_MASK; }
    bool is_atomic()       const { return t & TC_ATOMIC_MASK; }
    bool is_light_atomic() const { return t & TC_LIGHT_ATOMIC_MASK; }
    bool is_heavy_atomic() const { return t & TC_HEAVY_ATOMIC_MASK; }


    __uint32 get_type()    const { return t & TC_TYPE_MASK; }
    xmlscm_type get_atomic_type() const { return t & TC_XTYPE_MASK; }

    xptr get_node() const { return *(xptr*)(&data); }

    __int64            get_xs_integer()  const { return *(__int64*           )(&data); } 
    xs_decimal_t       get_xs_decimal()  const { return *(xs_decimal_t*      )(&data); }
    float              get_xs_float()    const { return *(float*             )(&data); }
    double             get_xs_double()   const { return *(double*            )(&data); }
    bool               get_xs_boolean()  const { return *(bool*              )(&data); }
    xs_packed_datetime get_xs_dateTime() const { return *(xs_packed_datetime*)(&data); }
    xs_packed_duration get_xs_duration() const { return *(xs_packed_duration*)(&data); }
//    xs_QName           get_xs_QName()    const { return *(xs_QName*          )(&data); }

/* !!! DELETE LATER */
/*
    char*   get_xs_untypedAtomic_mem  () const { return get_str_mem(); }
    xptr    get_xs_untypedAtomic_vmm  () const { return get_str_vmm(); }
    char*   get_xs_anyURI_mem         () const { return get_str_mem(); }
    xptr    get_xs_anyURI_vmm         () const { return get_str_vmm(); }
    char*   get_xs_QName_mem          () const { return get_str_mem(); }
    xptr    get_xs_QName_vmm          () const { return get_str_vmm(); }
    char*   get_xs_string_mem         () const { return get_str_mem(); }
    xptr    get_xs_string_vmm         () const { return get_str_vmm(); }
*/

    char*   get_str_mem           () const { return ((str_counted_ptr*)(&data))->get(); }
    xptr    get_str_vmm           () const { return *(xptr*)(&data); }

    int     get_strlen_mem        () const { return strlen(get_str_mem()); }
    int     get_strlen_vmm        () const { return *(int*)((char*)(&data) + sizeof(xptr)); }
    int     get_strlen            () const { return is_light_atomic() ? get_strlen_mem() : get_strlen_vmm(); }

    str_counted_ptr get_str_ptr   () const { return *(str_counted_ptr*)(&data); }

    bool is_string_type           () const { return ::is_string_type(get_atomic_type()); }
    bool is_numeric_type          () const { return ::is_numeric_type(get_atomic_type()); }
    bool is_fixed_size_type       () const { return ::is_fixed_size_type(get_atomic_type()); }

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
    tuple_cell(const xptr &_p_) 
    {
        t = tc_node;
		*(xptr*)(&(data)) = _p_;
    }
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
        char *tmp = new char[strlen(_str_) + 1];
        strcpy(tmp, _str_);
        t = tc_light_atomic_var_size | _xtype_;
        data.x = data.y = (__int64)0;
        *(str_counted_ptr*)(&data) = str_counted_ptr(tmp);
    }
    // for heavy atomics
    tuple_cell(__uint32 _t_, xmlscm_type _xtype_, const xptr &_p_, int _size_)
    {
        t = _t_ | _xtype_;
		*(xptr*)(&(data)) = _p_;
        *(int*)((char*)(&data) + sizeof(xptr)) = _size_;
    }


    ////////////////////////////////////////////////////////////////////////////
    /// FACTORIES FOR TUPLE_CELL
    ////////////////////////////////////////////////////////////////////////////
    static tuple_cell eos()
    {
        return tuple_cell();
    }

    static tuple_cell node(const xptr &_p_)
    {
        return tuple_cell(_p_);
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

    static tuple_cell atomic(__uint32 _t_, xmlscm_type _xtype_, int _size_, const xptr &_p_)
    {
        return tuple_cell(_t_, _xtype_, _p_, _size_);
    }

    static tuple_cell atomic_pstr(xmlscm_type _xtype_, int _size_, const xptr &_p_)
    {
        __uint32 _t_ = _size_ > PSTRMAXSIZE ? tc_heavy_atomic_pstr_long : tc_heavy_atomic_pstr_short;
        return tuple_cell(_t_, _xtype_, _p_, _size_);
    }
	
    static tuple_cell atomic_estr(xmlscm_type _xtype_, int _size_, const xptr &_p_)
    {
        return tuple_cell(tc_heavy_atomic_estr, _xtype_, _p_, _size_);
    }

    static tuple_cell atomic_xs_QName_deep(const char *_prefix_, const char *_name_)
    {
        char *tmp = NULL;
        if (_prefix_)
        {
            int prefix_len = strlen(_prefix_);
            int name_len = strlen(_name_);
            tmp = new char[prefix_len + name_len + 2];
            strcpy(tmp, _prefix_);
            tmp[prefix_len] = ':';
            strcpy(tmp + prefix_len + 1, _name_);
        }
        else
        {
            tmp = new char[strlen(_name_) + 1];
            strcpy(tmp, _name_);
        }
        return tuple_cell(xs_QName, tmp, true);
    }

    static tuple_cell atomic_se_separator()
    {
        return tuple_cell(tc_light_atomic_fix_size | se_separator);
    }

    ////////////////////////////////////////////////////////////////////////////
    /// FUNCTIONS THAT CHANGE THE VALUE OF TUPLE_CELL (DANGEROUS!!!)
    ////////////////////////////////////////////////////////////////////////////
    void set_xtype(xmlscm_type _xtype_) { t = (t & TC_TYPE_MASK) | _xtype_; }
    // !!! DO WE NEED THIS FUNCTIONS???
    void set_eos() 
    { 
        release();
        t = tc_eos;
        data.x = (__int64)0;
        data.y = (__int64)0;
    }
    void set_node(const xptr &_p_) 
    { 
        release();
        t = tc_node; 
        *(xptr*)(&data) = _p_;
    }
    void set_xptr(const xptr &_p_) { *(xptr*)(&data) = _p_; }

/* !!!!!!!!!!!!!!!!!!
    /// set string atomic value w/o deep copy
    void set_atomic(xmlscm_type _xtype_, char *_str_)
    {
        release();
        t = tc_light_atomic_var_size | _xtype_;
        data.x = data.y = (__int64)0;
        *(str_counted_ptr*)(&data) = str_counted_ptr(_str_);
    }
*/
/* !!!!!!!!!!!!!!!!!!!
    /// set counted string atomic value w/o deep copy
    void set_atomic(xmlscm_type _xtype_, str_counted_ptr _str_)
    {
        type = tc_light_atomic;
        data = (tcdata)0;
        xtype = _xtype_;
        str_ptr = _str_;
        size = 0;
    }
*/

/* !!!!!!!!!!!!!!!
    /// set string atomic value with deep copy
    void set_atomic_deep(xmlscm_type _xtype_, const char *_str_)
    {
        char *tmp = new char[strlen(_str_) + 1];
        strcpy(tmp, _str_);

        type = tc_light_atomic;
        data = (tcdata)0;
        xtype = _xtype_;
        str_ptr = str_counted_ptr(tmp);
        size = 0;
    }
*/
    void _adjust_serialized_tc(const xptr &txt_ptr)
    {
        if (is_light_atomic())
        {
            int len = get_strlen_mem();
            data.x = data.y = (__int64)0;
            set_size(len);
        }
        set_xptr(txt_ptr);
    }

    void _adjust_restored_tc(char *_str_)
    {
        data.x = data.y = (__int64)0;
        *(str_counted_ptr*)(&data) = str_counted_ptr(_str_);
    }
/*
	/// never use this function unless you know what you are doing
	void _reset_str_ptr()
	{
		//!!!!!!!!!!!!!!!!!!!!!!!!!!!memset(&str_ptr, 0, sizeof(str_ptr));
	}
*/
    ////////////////////////////////////////////////////////////////////////////
    /// ADDITIONAL FUNCTIONS
    ////////////////////////////////////////////////////////////////////////////
    void print(bool b = true) const;

    static tuple_cell make_sure_light_atomic(const tuple_cell& tc);

    /// copy content of the string to buf (!!! not very well designed approach...)
    void copy_string(char *buf);
};


extern tuple_cell EMPTY_STRING_TC;


struct tuple
{ 
    int cells_number;	// number of cells in the array 
    tuple_cell *cells;	// array of cells
    bool eos;			// is eos?

    tuple() : cells_number(0), cells(NULL), eos(true) {}
    tuple(int _n_) : cells_number(_n_), eos(false) { cells = new tuple_cell[_n_]; }
    tuple(const tuple &t);
    tuple &operator=(const tuple& t);

    void copy(const tuple &t)
    {
        eos = t.eos;
        if (!eos) for (int i = 0; i < cells_number; i++) cells[i] = t.cells[i];
    }
    void copy(const tuple_cell &tc)
    {
        eos = false;
        if (cells_number != 1) throw USER_EXCEPTION2(SE1003, "Cannot construct tuple from tuple cell (size mismatch)");
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
