/*
 * File:  tuple.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _TUPLE_H
#define _TUPLE_H

#include "counted_ptr.h"
#include "nodes.h"
#include "decimal.h"
#include "date.h"
#include "pstr.h"

/// Array of int pairs
typedef std::pair<int, int>			int_pair;
typedef std::vector<int_pair>		arr_of_int_pairs;


typedef counted_ptr<char> str_counted_ptr;



// type of a cell.
enum tuple_cell_type {
     tc_eos,			        // cell stores end of sequence
     tc_node,			        // cell stores node
     tc_light_atomic,	        // cell stores light atomic value (feats in dynamic memory)
     tc_heavy_atomic_estr,	    // cell stores heavy atomic value in e_strings (that is stored in VMM memory)
     tc_heavy_atomic_pstr_short,// cell stores heavy atomic value in short pstrings (that is stored in VMM memory)
     tc_heavy_atomic_pstr_long  // cell stores heavy atomic value in long pstrings (that is stored in VMM memory)
                     };

typedef __int64 tcdata;

//#define GET_INTEGER(t)		(*(int*)(&((t).data)))

#define MAX_MEM_STR_SIZE	100


struct tuple_cell
{
private:
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

public:

    void print(bool b = true) const;
    ////////////////////////////////////////////////////////////////////////////
    /// SET AND GET FUNCTIONS (SOME OF THEM ARE VERY DANGEROUS)
    ////////////////////////////////////////////////////////////////////////////
    bool is_eos()    const { return type == tc_eos; }
    bool is_node()   const { return type == tc_node; }
    bool is_atomic() const { return type >= tc_light_atomic; }
    bool is_light_atomic() const { return type == tc_light_atomic; }
    bool is_heavy_atomic() const { return type > tc_light_atomic; }


    tuple_cell_type get_type() const { return type; }
    xmlscm_type get_atomic_type() const { return xtype; }

    xptr get_node() const { return *(xptr*)(&data); }

    int        get_xs_integer() const { return *(int*       )(&data); } 
    decimal    get_xs_decimal() const { return *(decimal*   )(&data); }
    float      get_xs_float()   const { return *(float*     )(&data); }
    double     get_xs_double()  const { return *(double*    )(&data); }
    bool       get_xs_boolean() const { return *(bool*      )(&data); }
    date       get_xs_date()    const { return *(date*      )(&data); }

/* !!! DELETE LATER */
    char*   get_xdt_untypedAtomic_mem () const { return get_str_mem(); }
    xptr    get_xdt_untypedAtomic_vmm () const { return get_str_vmm(); }
    char*   get_xs_anyURI_mem         () const { return get_str_mem(); }
    xptr    get_xs_anyURI_vmm         () const { return get_str_vmm(); }
    char*   get_xs_QName_mem          () const { return get_str_mem(); }
    xptr    get_xs_QName_vmm          () const { return get_str_vmm(); }
    char*   get_xs_string_mem         () const { return get_str_mem(); }
    xptr    get_xs_string_vmm         () const { return get_str_vmm(); }


    char*   get_str_mem           () const { return str_ptr.get(); }
    xptr    get_str_vmm           () const { return *(xptr*)(&data); }

    int     get_strlen_mem        () const { return strlen(str_ptr.get()); }
    int     get_strlen_vmm        () const { return size; }
    int     get_strlen            () const { return is_light_atomic() ? strlen(str_ptr.get()) : size; }

    str_counted_ptr get_str_ptr   () const { return str_ptr; }

    bool is_string_type() const
    { // !!! Other string types are possible in the future
        return (xtype == xdt_untypedAtomic	||
                xtype == xs_anyURI			||
                xtype == xs_QName			||
                xtype == xs_string);
    }
    bool is_numeric_type() const
    {
        return (xtype == xs_integer			||
                xtype == xs_decimal			||
                xtype == xs_float			||
                xtype == xs_double);
    }
    bool is_tuple_cell_of_none_const_size() const
    {  
        return (type == tc_light_atomic && is_string_type());
    }

    ////////////////////////////////////////////////////////////////////////////
    /// CONSTRUCTORS FOR TUPLE_CELL
    ////////////////////////////////////////////////////////////////////////////
    tuple_cell() : type(tc_eos), data((tcdata)0), xtype(xs_anyType), size(0) {}
    tuple_cell(tuple_cell_type _type_,
               tcdata _data_,
               xmlscm_type _xtype_,
               char* _str_,
               int _size_) : type(_type_),
                             data(_data_),
                             xtype(_xtype_),
                             str_ptr(_str_),
                             size(_size_) {}

    // for int atomics
    tuple_cell(int _data_) : type(tc_light_atomic), xtype(xs_integer), size(0)
    {
        *(int*)(&(data)) = _data_;
    }
    // for xs_decimal atomics
    tuple_cell(decimal _data_) : type(tc_light_atomic), xtype(xs_decimal), size(0)
    {
        *(decimal*)(&(data)) = _data_;
    }
    // for xs_float atomics
    tuple_cell(float _data_) : type(tc_light_atomic), xtype(xs_float), size(0)
    {
        *(float*)(&(data)) = _data_;
    }
    // for xs_double atomics
    tuple_cell(double _data_) : type(tc_light_atomic), xtype(xs_double), size(0)
    {
        *(double*)(&(data)) = _data_;
    }
    // for xs_boolean atomics
    tuple_cell(bool _data_) : type(tc_light_atomic), xtype(xs_boolean), size(0)
    {
        *(bool*)(&(data)) = _data_;
    }
    // for xs_date atomics
    tuple_cell(date _data_) : type(tc_light_atomic), xtype(xs_date), size(0)
    {
        *(date*)(&(data)) = _data_;
    }


    ////////////////////////////////////////////////////////////////////////////
    /// FACTORIES FOR TUPLE_CELL
    ////////////////////////////////////////////////////////////////////////////
    static tuple_cell node(const xptr &_p_) 
    { 
        return tuple_cell(tc_node, *((tcdata*)&_p_), xs_anyType, NULL, 0);
    }

    static tuple_cell eos()
    {
        return tuple_cell();
    }

    static tuple_cell atomic(int _data_)
    {
        return tuple_cell(_data_);
    }

    static tuple_cell atomic(decimal _data_)
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

    static tuple_cell atomic(date _data_)
    {
        return tuple_cell(_data_);
    }

    static tuple_cell atomic(xmlscm_type _xtype_, char *_str_)
    {
        return tuple_cell(tc_light_atomic, (tcdata)0, _xtype_, _str_, 0);
    }

    static tuple_cell atomic_deep(xmlscm_type _xtype_, const char *_str_)
    {
        char *tmp = new char[strlen(_str_) + 1];
        strcpy(tmp, _str_);

        return tuple_cell(tc_light_atomic, (tcdata)0, _xtype_, tmp, 0);
    }

    static tuple_cell atomic(tuple_cell_type _tc_type_, xmlscm_type _xtype_, int _size_, const xptr &_p_)
    {
        return tuple_cell(_tc_type_, *((tcdata*)&_p_), _xtype_, NULL, _size_);
    }

    static tuple_cell atomic_pstr(xmlscm_type _xtype_, int _size_, const xptr &_p_)
    {
        return tuple_cell(_size_ > PSTRMAXSIZE ? tc_heavy_atomic_pstr_long : tc_heavy_atomic_pstr_short, 
                          *((tcdata*)&_p_), 
                          _xtype_, 
                          NULL, 
                          _size_);
    }
	
    static tuple_cell atomic_estr(xmlscm_type _xtype_, int _size_, const xptr &_p_)
    {
        return tuple_cell(tc_heavy_atomic_estr, *((tcdata*)&_p_), _xtype_, NULL, _size_);
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

        return tuple_cell(tc_light_atomic, (tcdata)0, xs_QName, tmp, 0);
    }

    static tuple_cell atomic_se_separator()
    {
        return tuple_cell(tc_light_atomic, (tcdata)0, se_separator, NULL, 0);
    }

    ////////////////////////////////////////////////////////////////////////////
    /// FUNCTIONS THAT CHANGE THE VALUE OF TUPLE_CELL (DANGEROUS!!!)
    ////////////////////////////////////////////////////////////////////////////
    void set_xtype(xmlscm_type _xtype_) { xtype = _xtype_; }
    void set_eos() { type = tc_eos; }
    void set_node(const xptr &_p_) { type = tc_node; data = *((tcdata*)&_p_); }
    void set_xptr(const xptr &_p_) { data = *((tcdata*)&_p_); }
    void set_size(int _size_) { size = _size_; }

    /// set string atomic value w/o deep copy
    void set_atomic(xmlscm_type _xtype_, char *_str_)
    {
        type = tc_light_atomic;
        data = (tcdata)0;
        xtype = _xtype_;
        str_ptr = str_counted_ptr(_str_);
        size = 0;
    }

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


    ////////////////////////////////////////////////////////////////////////////
    /// ADDITIONAL FUNCTIONS
    ////////////////////////////////////////////////////////////////////////////
    static tuple_cell make_sure_light_atomic(const tuple_cell& tc);

    /// copy content of the string to buf
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
