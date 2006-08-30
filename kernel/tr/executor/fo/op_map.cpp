/*
 * File:  op_map.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "op_map.h"
#include "boolean_operations.h"
#include "comparison_operations.h"
#include "dateTime_operations.h"
#include "numeric_operations.h"
#include "string_operations.h"



typedef int (*simle_type2index)(xmlscm_type);

int simple_type2bin_op_numeric_index(xmlscm_type xtype);

int simple_type2bin_op_add_index(xmlscm_type xtype);
int simple_type2bin_op_sub_index(xmlscm_type xtype);
int simple_type2bin_op_mul_index(xmlscm_type xtype);
int simple_type2bin_op_div_index(xmlscm_type xtype);
int simple_type2bin_op_idiv_index(xmlscm_type xtype);
int simple_type2bin_op_mod_index(xmlscm_type xtype);
int simple_type2bin_op_eq_index(xmlscm_type xtype);
int simple_type2bin_op_ne_index(xmlscm_type xtype);
int simple_type2bin_op_gt_index(xmlscm_type xtype);
int simple_type2bin_op_lt_index(xmlscm_type xtype);
int simple_type2bin_op_ge_index(xmlscm_type xtype);
int simple_type2bin_op_le_index(xmlscm_type xtype);

extern bin_op_tuple_cell_tuple_cell op_add_tbl[6][6];
extern bin_op_tuple_cell_tuple_cell op_sub_tbl[6][6];
extern bin_op_tuple_cell_tuple_cell op_mul_tbl[3][3];
extern bin_op_tuple_cell_tuple_cell op_div_tbl[3][3];
extern bin_op_tuple_cell_tuple_cell op_idiv_tbl[1][1];
extern bin_op_tuple_cell_tuple_cell op_mod_tbl[1][1];
extern bin_op_tuple_cell_tuple_cell op_eq_tbl[13][13];
extern bin_op_tuple_cell_tuple_cell op_ne_tbl[13][13];
extern bin_op_tuple_cell_tuple_cell op_gt_tbl[7][7];
extern bin_op_tuple_cell_tuple_cell op_lt_tbl[7][7];
extern bin_op_tuple_cell_tuple_cell op_ge_tbl[7][7];
extern bin_op_tuple_cell_tuple_cell op_le_tbl[7][7];

struct xq_binary_op_info_type
{
    bin_op_tuple_cell_tuple_cell fun;

    bin_op_tuple_cell_tuple_cell (*numeric_table)[5];
    simle_type2index numeric_index;

    bin_op_tuple_cell_tuple_cell *map_table;
    int map_table_side_size;
    simle_type2index map_index;
};

struct xq_unary_op_info_type
{
    un_op_tuple_cell fun;
    un_op_tuple_cell *numeric_table;
};

// string compare functions for XQuery B.2 Operator Mapping implementation
tuple_cell op_map_fn_compare_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2, true).get_xs_integer() == 0);
}

tuple_cell op_map_fn_compare_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2, true).get_xs_integer() != 0);
}

tuple_cell op_map_fn_compare_less_than(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2, true).get_xs_integer() < 0);
}

tuple_cell op_map_fn_compare_less_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2, true).get_xs_integer() <= 0);
}

tuple_cell op_map_fn_compare_greater_than(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2, true).get_xs_integer() > 0);
}

tuple_cell op_map_fn_compare_greater_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2, true).get_xs_integer() >= 0);
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// TABLES AND FUNCTIONS FOR NUMERIC OPERATIONS
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

int simple_type2bin_op_numeric_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_untypedAtomic: 
        return 0;
    case xs_integer:
        return 1;
    case xs_decimal: 
        return 2;
    case xs_float: 
        return 3;
    case xs_double: 
        return 4;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_numeric_add_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                               /*xs_integer*/                               /*xs_decimal*/                               /*xs_float*/                               /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_add_xs_untypedAtomic_xs_untypedAtomic, op_numeric_add_xs_untypedAtomic_xs_integer, op_numeric_add_xs_untypedAtomic_xs_decimal, op_numeric_add_xs_untypedAtomic_xs_float, op_numeric_add_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_add_xs_integer_xs_untypedAtomic,        op_numeric_add_xs_integer_xs_integer,        op_numeric_add_xs_integer_xs_decimal,        op_numeric_add_xs_integer_xs_float,        op_numeric_add_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_add_xs_decimal_xs_untypedAtomic,        op_numeric_add_xs_decimal_xs_integer,        op_numeric_add_xs_decimal_xs_decimal,        op_numeric_add_xs_decimal_xs_float,        op_numeric_add_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_add_xs_float_xs_untypedAtomic,          op_numeric_add_xs_float_xs_integer,          op_numeric_add_xs_float_xs_decimal,          op_numeric_add_xs_float_xs_float,          op_numeric_add_xs_float_xs_double},
/*xs_double*/         {op_numeric_add_xs_double_xs_untypedAtomic,         op_numeric_add_xs_double_xs_integer,         op_numeric_add_xs_double_xs_decimal,         op_numeric_add_xs_double_xs_float,         op_numeric_add_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_sub_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                                    /*xs_integer*/                                    /*xs_decimal*/                                    /*xs_float*/                                    /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_subtract_xs_untypedAtomic_xs_untypedAtomic, op_numeric_subtract_xs_untypedAtomic_xs_integer, op_numeric_subtract_xs_untypedAtomic_xs_decimal, op_numeric_subtract_xs_untypedAtomic_xs_float, op_numeric_subtract_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_subtract_xs_integer_xs_untypedAtomic,        op_numeric_subtract_xs_integer_xs_integer,        op_numeric_subtract_xs_integer_xs_decimal,        op_numeric_subtract_xs_integer_xs_float,        op_numeric_subtract_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_subtract_xs_decimal_xs_untypedAtomic,        op_numeric_subtract_xs_decimal_xs_integer,        op_numeric_subtract_xs_decimal_xs_decimal,        op_numeric_subtract_xs_decimal_xs_float,        op_numeric_subtract_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_subtract_xs_float_xs_untypedAtomic,          op_numeric_subtract_xs_float_xs_integer,          op_numeric_subtract_xs_float_xs_decimal,          op_numeric_subtract_xs_float_xs_float,          op_numeric_subtract_xs_float_xs_double},
/*xs_double*/         {op_numeric_subtract_xs_double_xs_untypedAtomic,         op_numeric_subtract_xs_double_xs_integer,         op_numeric_subtract_xs_double_xs_decimal,         op_numeric_subtract_xs_double_xs_float,         op_numeric_subtract_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_mul_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                                    /*xs_integer*/                                    /*xs_decimal*/                                    /*xs_float*/                                    /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_multiply_xs_untypedAtomic_xs_untypedAtomic, op_numeric_multiply_xs_untypedAtomic_xs_integer, op_numeric_multiply_xs_untypedAtomic_xs_decimal, op_numeric_multiply_xs_untypedAtomic_xs_float, op_numeric_multiply_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_multiply_xs_integer_xs_untypedAtomic,        op_numeric_multiply_xs_integer_xs_integer,        op_numeric_multiply_xs_integer_xs_decimal,        op_numeric_multiply_xs_integer_xs_float,        op_numeric_multiply_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_multiply_xs_decimal_xs_untypedAtomic,        op_numeric_multiply_xs_decimal_xs_integer,        op_numeric_multiply_xs_decimal_xs_decimal,        op_numeric_multiply_xs_decimal_xs_float,        op_numeric_multiply_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_multiply_xs_float_xs_untypedAtomic,          op_numeric_multiply_xs_float_xs_integer,          op_numeric_multiply_xs_float_xs_decimal,          op_numeric_multiply_xs_float_xs_float,          op_numeric_multiply_xs_float_xs_double},
/*xs_double*/         {op_numeric_multiply_xs_double_xs_untypedAtomic,         op_numeric_multiply_xs_double_xs_integer,         op_numeric_multiply_xs_double_xs_decimal,         op_numeric_multiply_xs_double_xs_float,         op_numeric_multiply_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_div_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                                  /*xs_integer*/                                  /*xs_decimal*/                                  /*xs_float*/                                  /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_divide_xs_untypedAtomic_xs_untypedAtomic, op_numeric_divide_xs_untypedAtomic_xs_integer, op_numeric_divide_xs_untypedAtomic_xs_decimal, op_numeric_divide_xs_untypedAtomic_xs_float, op_numeric_divide_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_divide_xs_integer_xs_untypedAtomic,        op_numeric_divide_xs_integer_xs_integer,        op_numeric_divide_xs_integer_xs_decimal,        op_numeric_divide_xs_integer_xs_float,        op_numeric_divide_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_divide_xs_decimal_xs_untypedAtomic,        op_numeric_divide_xs_decimal_xs_integer,        op_numeric_divide_xs_decimal_xs_decimal,        op_numeric_divide_xs_decimal_xs_float,        op_numeric_divide_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_divide_xs_float_xs_untypedAtomic,          op_numeric_divide_xs_float_xs_integer,          op_numeric_divide_xs_float_xs_decimal,          op_numeric_divide_xs_float_xs_float,          op_numeric_divide_xs_float_xs_double},
/*xs_double*/         {op_numeric_divide_xs_double_xs_untypedAtomic,         op_numeric_divide_xs_double_xs_integer,         op_numeric_divide_xs_double_xs_decimal,         op_numeric_divide_xs_double_xs_float,         op_numeric_divide_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_idiv_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                                          /*xs_integer*/                                          /*xs_decimal*/                                          /*xs_float*/                                          /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_integer_divide_xs_untypedAtomic_xs_untypedAtomic, op_numeric_integer_divide_xs_untypedAtomic_xs_integer, op_numeric_integer_divide_xs_untypedAtomic_xs_decimal, op_numeric_integer_divide_xs_untypedAtomic_xs_float, op_numeric_integer_divide_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_integer_divide_xs_integer_xs_untypedAtomic,        op_numeric_integer_divide_xs_integer_xs_integer,        op_numeric_integer_divide_xs_integer_xs_decimal,        op_numeric_integer_divide_xs_integer_xs_float,        op_numeric_integer_divide_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_integer_divide_xs_decimal_xs_untypedAtomic,        op_numeric_integer_divide_xs_decimal_xs_integer,        op_numeric_integer_divide_xs_decimal_xs_decimal,        op_numeric_integer_divide_xs_decimal_xs_float,        op_numeric_integer_divide_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_integer_divide_xs_float_xs_untypedAtomic,          op_numeric_integer_divide_xs_float_xs_integer,          op_numeric_integer_divide_xs_float_xs_decimal,          op_numeric_integer_divide_xs_float_xs_float,          op_numeric_integer_divide_xs_float_xs_double},
/*xs_double*/         {op_numeric_integer_divide_xs_double_xs_untypedAtomic,         op_numeric_integer_divide_xs_double_xs_integer,         op_numeric_integer_divide_xs_double_xs_decimal,         op_numeric_integer_divide_xs_double_xs_float,         op_numeric_integer_divide_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_mod_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                               /*xs_integer*/                               /*xs_decimal*/                               /*xs_float*/                               /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_mod_xs_untypedAtomic_xs_untypedAtomic, op_numeric_mod_xs_untypedAtomic_xs_integer, op_numeric_mod_xs_untypedAtomic_xs_decimal, op_numeric_mod_xs_untypedAtomic_xs_float, op_numeric_mod_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_mod_xs_integer_xs_untypedAtomic,        op_numeric_mod_xs_integer_xs_integer,        op_numeric_mod_xs_integer_xs_decimal,        op_numeric_mod_xs_integer_xs_float,        op_numeric_mod_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_mod_xs_decimal_xs_untypedAtomic,        op_numeric_mod_xs_decimal_xs_integer,        op_numeric_mod_xs_decimal_xs_decimal,        op_numeric_mod_xs_decimal_xs_float,        op_numeric_mod_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_mod_xs_float_xs_untypedAtomic,          op_numeric_mod_xs_float_xs_integer,          op_numeric_mod_xs_float_xs_decimal,          op_numeric_mod_xs_float_xs_float,          op_numeric_mod_xs_float_xs_double},
/*xs_double*/         {op_numeric_mod_xs_double_xs_untypedAtomic,         op_numeric_mod_xs_double_xs_integer,         op_numeric_mod_xs_double_xs_decimal,         op_numeric_mod_xs_double_xs_float,         op_numeric_mod_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_eq_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                                 /*xs_integer*/                                 /*xs_decimal*/                                 /*xs_float*/                                 /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_equal_xs_untypedAtomic_xs_untypedAtomic, op_numeric_equal_xs_untypedAtomic_xs_integer, op_numeric_equal_xs_untypedAtomic_xs_decimal, op_numeric_equal_xs_untypedAtomic_xs_float, op_numeric_equal_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_equal_xs_integer_xs_untypedAtomic,        op_numeric_equal_xs_integer_xs_integer,        op_numeric_equal_xs_integer_xs_decimal,        op_numeric_equal_xs_integer_xs_float,        op_numeric_equal_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_equal_xs_decimal_xs_untypedAtomic,        op_numeric_equal_xs_decimal_xs_integer,        op_numeric_equal_xs_decimal_xs_decimal,        op_numeric_equal_xs_decimal_xs_float,        op_numeric_equal_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_equal_xs_float_xs_untypedAtomic,          op_numeric_equal_xs_float_xs_integer,          op_numeric_equal_xs_float_xs_decimal,          op_numeric_equal_xs_float_xs_float,          op_numeric_equal_xs_float_xs_double},
/*xs_double*/         {op_numeric_equal_xs_double_xs_untypedAtomic,         op_numeric_equal_xs_double_xs_integer,         op_numeric_equal_xs_double_xs_decimal,         op_numeric_equal_xs_double_xs_float,         op_numeric_equal_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_lt_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                                     /*xs_integer*/                                     /*xs_decimal*/                                     /*xs_float*/                                     /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_less_than_xs_untypedAtomic_xs_untypedAtomic, op_numeric_less_than_xs_untypedAtomic_xs_integer, op_numeric_less_than_xs_untypedAtomic_xs_decimal, op_numeric_less_than_xs_untypedAtomic_xs_float, op_numeric_less_than_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_less_than_xs_integer_xs_untypedAtomic,        op_numeric_less_than_xs_integer_xs_integer,        op_numeric_less_than_xs_integer_xs_decimal,        op_numeric_less_than_xs_integer_xs_float,        op_numeric_less_than_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_less_than_xs_decimal_xs_untypedAtomic,        op_numeric_less_than_xs_decimal_xs_integer,        op_numeric_less_than_xs_decimal_xs_decimal,        op_numeric_less_than_xs_decimal_xs_float,        op_numeric_less_than_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_less_than_xs_float_xs_untypedAtomic,          op_numeric_less_than_xs_float_xs_integer,          op_numeric_less_than_xs_float_xs_decimal,          op_numeric_less_than_xs_float_xs_float,          op_numeric_less_than_xs_float_xs_double},
/*xs_double*/         {op_numeric_less_than_xs_double_xs_untypedAtomic,         op_numeric_less_than_xs_double_xs_integer,         op_numeric_less_than_xs_double_xs_decimal,         op_numeric_less_than_xs_double_xs_float,         op_numeric_less_than_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_gt_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                                        /*xs_integer*/                                        /*xs_decimal*/                                        /*xs_float*/                                        /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_greater_than_xs_untypedAtomic_xs_untypedAtomic, op_numeric_greater_than_xs_untypedAtomic_xs_integer, op_numeric_greater_than_xs_untypedAtomic_xs_decimal, op_numeric_greater_than_xs_untypedAtomic_xs_float, op_numeric_greater_than_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_greater_than_xs_integer_xs_untypedAtomic,        op_numeric_greater_than_xs_integer_xs_integer,        op_numeric_greater_than_xs_integer_xs_decimal,        op_numeric_greater_than_xs_integer_xs_float,        op_numeric_greater_than_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_greater_than_xs_decimal_xs_untypedAtomic,        op_numeric_greater_than_xs_decimal_xs_integer,        op_numeric_greater_than_xs_decimal_xs_decimal,        op_numeric_greater_than_xs_decimal_xs_float,        op_numeric_greater_than_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_greater_than_xs_float_xs_untypedAtomic,          op_numeric_greater_than_xs_float_xs_integer,          op_numeric_greater_than_xs_float_xs_decimal,          op_numeric_greater_than_xs_float_xs_float,          op_numeric_greater_than_xs_float_xs_double},
/*xs_double*/         {op_numeric_greater_than_xs_double_xs_untypedAtomic,         op_numeric_greater_than_xs_double_xs_integer,         op_numeric_greater_than_xs_double_xs_decimal,         op_numeric_greater_than_xs_double_xs_float,         op_numeric_greater_than_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_ne_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                                     /*xs_integer*/                                     /*xs_decimal*/                                     /*xs_float*/                                     /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_not_equal_xs_untypedAtomic_xs_untypedAtomic, op_numeric_not_equal_xs_untypedAtomic_xs_integer, op_numeric_not_equal_xs_untypedAtomic_xs_decimal, op_numeric_not_equal_xs_untypedAtomic_xs_float, op_numeric_not_equal_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_not_equal_xs_integer_xs_untypedAtomic,        op_numeric_not_equal_xs_integer_xs_integer,        op_numeric_not_equal_xs_integer_xs_decimal,        op_numeric_not_equal_xs_integer_xs_float,        op_numeric_not_equal_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_not_equal_xs_decimal_xs_untypedAtomic,        op_numeric_not_equal_xs_decimal_xs_integer,        op_numeric_not_equal_xs_decimal_xs_decimal,        op_numeric_not_equal_xs_decimal_xs_float,        op_numeric_not_equal_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_not_equal_xs_float_xs_untypedAtomic,          op_numeric_not_equal_xs_float_xs_integer,          op_numeric_not_equal_xs_float_xs_decimal,          op_numeric_not_equal_xs_float_xs_float,          op_numeric_not_equal_xs_float_xs_double},
/*xs_double*/         {op_numeric_not_equal_xs_double_xs_untypedAtomic,         op_numeric_not_equal_xs_double_xs_integer,         op_numeric_not_equal_xs_double_xs_decimal,         op_numeric_not_equal_xs_double_xs_float,         op_numeric_not_equal_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_le_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                                      /*xs_integer*/                                      /*xs_decimal*/                                      /*xs_float*/                                      /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_less_equal_xs_untypedAtomic_xs_untypedAtomic, op_numeric_less_equal_xs_untypedAtomic_xs_integer, op_numeric_less_equal_xs_untypedAtomic_xs_decimal, op_numeric_less_equal_xs_untypedAtomic_xs_float, op_numeric_less_equal_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_less_equal_xs_integer_xs_untypedAtomic,        op_numeric_less_equal_xs_integer_xs_integer,        op_numeric_less_equal_xs_integer_xs_decimal,        op_numeric_less_equal_xs_integer_xs_float,        op_numeric_less_equal_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_less_equal_xs_decimal_xs_untypedAtomic,        op_numeric_less_equal_xs_decimal_xs_integer,        op_numeric_less_equal_xs_decimal_xs_decimal,        op_numeric_less_equal_xs_decimal_xs_float,        op_numeric_less_equal_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_less_equal_xs_float_xs_untypedAtomic,          op_numeric_less_equal_xs_float_xs_integer,          op_numeric_less_equal_xs_float_xs_decimal,          op_numeric_less_equal_xs_float_xs_float,          op_numeric_less_equal_xs_float_xs_double},
/*xs_double*/         {op_numeric_less_equal_xs_double_xs_untypedAtomic,         op_numeric_less_equal_xs_double_xs_integer,         op_numeric_less_equal_xs_double_xs_decimal,         op_numeric_less_equal_xs_double_xs_float,         op_numeric_less_equal_xs_double_xs_double}
};

bin_op_tuple_cell_tuple_cell op_numeric_ge_tbl[5][5] = 
{
                       /*xs_untypedAtomic*/                                         /*xs_integer*/                                         /*xs_decimal*/                                         /*xs_float*/                                         /*xs_double*/
/*xs_untypedAtomic*/ {op_numeric_greater_equal_xs_untypedAtomic_xs_untypedAtomic, op_numeric_greater_equal_xs_untypedAtomic_xs_integer, op_numeric_greater_equal_xs_untypedAtomic_xs_decimal, op_numeric_greater_equal_xs_untypedAtomic_xs_float, op_numeric_greater_equal_xs_untypedAtomic_xs_double},
/*xs_integer*/        {op_numeric_greater_equal_xs_integer_xs_untypedAtomic,        op_numeric_greater_equal_xs_integer_xs_integer,        op_numeric_greater_equal_xs_integer_xs_decimal,        op_numeric_greater_equal_xs_integer_xs_float,        op_numeric_greater_equal_xs_integer_xs_double},
/*xs_decimal*/        {op_numeric_greater_equal_xs_decimal_xs_untypedAtomic,        op_numeric_greater_equal_xs_decimal_xs_integer,        op_numeric_greater_equal_xs_decimal_xs_decimal,        op_numeric_greater_equal_xs_decimal_xs_float,        op_numeric_greater_equal_xs_decimal_xs_double},
/*xs_float*/          {op_numeric_greater_equal_xs_float_xs_untypedAtomic,          op_numeric_greater_equal_xs_float_xs_integer,          op_numeric_greater_equal_xs_float_xs_decimal,          op_numeric_greater_equal_xs_float_xs_float,          op_numeric_greater_equal_xs_float_xs_double},
/*xs_double*/         {op_numeric_greater_equal_xs_double_xs_untypedAtomic,         op_numeric_greater_equal_xs_double_xs_integer,         op_numeric_greater_equal_xs_double_xs_decimal,         op_numeric_greater_equal_xs_double_xs_float,         op_numeric_greater_equal_xs_double_xs_double}
};





///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// IMPLEMENTATION OF (XQuery B.2 Operator Mapping)
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

int simple_type2bin_op_add_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_untypedAtomic: 
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    case xs_date:
        return 1;
    case xs_time:
        return 2;
    case xs_dayTimeDuration:
        return 3;
    case xs_yearMonthDuration:
        return 4;
    case xs_dateTime:
        return 5;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_add_tbl[6][6]=
{
                           /*numeric*/      /*xs:date*/                       /*xs:time*/                     /*xs:dayTimeDuration*/             /*xs:yearMonthDuration*/             /*xs:dateTime*/
/*numeric*/                {op_numeric_add, NULL,                             NULL,                           NULL,                               NULL,                                 NULL},
/*xs:date*/                {NULL,           NULL,                             NULL,                           op_add_dayTimeDuration_to_date,     op_add_yearMonthDuration_to_date,     NULL},
/*xs:time*/                {NULL,           NULL,                             NULL,                           op_add_dayTimeDuration_to_time,     NULL,                                 NULL},
/*xs:dayTimeDuration*/    {NULL,           op_add_date_to_dayTimeDuration,   op_add_time_to_dayTimeDuration, op_add_dayTimeDurations,            NULL,                                 op_add_dateTime_to_dayTimeDuration},
/*xs:yearMonthDuration*/  {NULL,           op_add_date_to_yearMonthDuration, NULL,                           NULL,                               op_add_yearMonthDurations,            op_add_dateTime_to_yearMonthDuration},
/*xs:dateTime*/            {NULL,           NULL,                             NULL,                           op_add_dayTimeDuration_to_dateTime, op_add_yearMonthDuration_to_dateTime, NULL}
};

int simple_type2bin_op_sub_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_untypedAtomic: 
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    case xs_date:
        return 1;
    case xs_time:
        return 2;
    case xs_dayTimeDuration:
        return 3;
    case xs_yearMonthDuration:
        return 4;
    case xs_dateTime:
        return 5;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_sub_tbl[6][6] =
{
                           /*numeric*/           /*xs:date*/          /*xs:time*/           /*xs:dayTimeDuration*/                     /*xs:yearMonthDuration*/                      /*xs:dateTime*/
/*numeric*/                {op_numeric_subtract, NULL,                NULL,                 NULL,                                       NULL,                                          NULL},
/*xs:date*/                {NULL,                op_subtract_dates,   NULL,                 op_subtract_dayTimeDuration_from_date,      op_subtract_yearMonthDuration_from_date,       NULL},
/*xs:time*/                {NULL,                NULL,                op_subtract_times,    op_subtract_dayTimeDuration_from_time,      NULL,                                          NULL},
/*xs:dayTimeDuration*/    {NULL,                NULL,                NULL,                 op_subtract_dayTimeDurations,               NULL,                                          NULL},
/*xs:yearMonthDuration*/  {NULL,                NULL,                NULL,                 NULL,                                       op_subtract_yearMonthDurations,                NULL},
/*xs:dateTime*/            {NULL,                NULL,                NULL,                 op_subtract_dayTimeDuration_from_dateTime,  op_subtract_yearMonthDuration_from_dateTime,   op_subtract_dateTimes}
};

int simple_type2bin_op_mul_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_untypedAtomic: 
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    case xs_dayTimeDuration:
        return 1;
    case xs_yearMonthDuration:
        return 2;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_mul_tbl[3][3] = 
{
                           /*numeric*/                              /*xs:dayTimeDuration*/                 /*xs:yearMonthDuration*/
/*numeric*/                {op_numeric_multiply,                      op_multiply_numeric_by_dayTimeDuration, op_multiply_numeric_by_yearMonthDuration},
/*xs:dayTimeDuration*/    {op_multiply_dayTimeDuration_by_numeric,   NULL,                                   NULL},
/*xs:yearMonthDuration*/  {op_multiply_yearMonthDuration_by_numeric, NULL,                                   NULL}
};


int simple_type2bin_op_div_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_untypedAtomic: 
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    case xs_dayTimeDuration:
        return 1;
    case xs_yearMonthDuration:
        return 2;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_div_tbl[3][3] = 
{
                           /*numeric*/                                /*xs:dayTimeDuration*/                        /*xs:yearMonthDuration*/
/*numeric*/                {op_numeric_divide,                        NULL,                                          NULL},
/*xs:dayTimeDuration*/    {op_divide_dayTimeDuration_by_numeric,     op_divide_dayTimeDuration_by_dayTimeDuration,  NULL},
/*xs:yearMonthDuration*/  {op_divide_yearMonthDuration_by_numeric,   NULL,                                          op_divide_yearMonthDuration_by_yearMonthDuration}
};

int simple_type2bin_op_idiv_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_untypedAtomic: 
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_idiv_tbl[1][1] = {{op_numeric_integer_divide}};

int simple_type2bin_op_mod_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_untypedAtomic: 
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_mod_tbl[1][1] = {{op_numeric_mod}};

int simple_type2bin_op_eq_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    case xs_date:
        return 1;
    case xs_time:
        return 2;
    case xs_duration:
	return 3;
    case xs_dayTimeDuration:
        return 4;
    case xs_yearMonthDuration:
        return 5;
    case xs_dateTime:
       return 6;
    case xs_gYear:
        return 7;
    case xs_gYearMonth:
        return 8;
    case xs_gMonth:
        return 9;
    case xs_gMonthDay:
        return 10;
    case xs_gDay:
        return 11;
    case xs_untypedAtomic:
    case xs_string:
        return 12;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_eq_tbl[13][13] = 
{
                           /*numeric*/        /*xs:date*/         /*xs:time*/         /* xs:duration*/           /*xs:dayTimeDuration*/             /*xs:yearMonthDuration*/             /*xs:dateTime*/                /*xs_gYear*/         /*xs:gYearMonth*/        /*xs_gMonth*/       /*xs:gMonthDay*/       /*xs_gDay*/     /*xs_string*/	
/*numeric*/                {op_numeric_equal, NULL,               NULL,               NULL,                      NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,               NULL,                  NULL,           NULL},
/*xs:date*/                {NULL,             op_date_equal,      NULL,               NULL,                      NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,               NULL,                  NULL,           NULL},
/*xs:time*/                {NULL,             NULL,               op_time_equal,      NULL,                      NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,               NULL,                  NULL,           NULL},
/*xs:duration*/            {NULL,             NULL,               NULL,               op_duration_equal,         NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,               NULL,                  NULL,           NULL},
/*xs:dayTimeDuration*/    {NULL,             NULL,               NULL,               NULL,                      op_dayTimeDuration_equal,           NULL,                                 NULL,                          NULL,                NULL,                    NULL,               NULL,                  NULL,           NULL},
/*xs:yearMonthDuration*/  {NULL,             NULL,               NULL,               NULL,                      NULL,                               op_yearMonthDuration_equal,           NULL,                          NULL,                NULL,                    NULL,               NULL,                  NULL,           NULL},
/*xs:dateTime*/            {NULL,             NULL,               NULL,               NULL,                      NULL,                               NULL,                                 op_dateTime_equal,             NULL,                NULL,                    NULL,               NULL,                  NULL,           NULL},
/*xs:gYear*/               {NULL,             NULL,               NULL,               NULL,                      NULL,                               NULL,                                 NULL,                          op_gYear_equal,      NULL,                    NULL,               NULL,                  NULL,           NULL},
/*xs:gYearMonth*/          {NULL,             NULL,               NULL,               NULL,                      NULL,                               NULL,                                 NULL,                          NULL,                op_gYearMonth_equal,     NULL,               NULL,                  NULL,           NULL},
/*xs:gMonth*/              {NULL,             NULL,               NULL,               NULL,                      NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    op_gMonth_equal,    NULL,                  NULL,           NULL},
/*xs:gMonthDay*/           {NULL,             NULL,               NULL,               NULL,                      NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,               op_gMonthDay_equal,    NULL,           NULL},
/*xs:gDay*/                {NULL,             NULL,               NULL,               NULL,                      NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,               NULL,                  op_gDay_equal,  NULL},
/*xs:string*/              {NULL,             NULL,               NULL,               NULL,                      NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,               NULL,                  NULL,           op_map_fn_compare_equal}
};


int simple_type2bin_op_ne_index(xmlscm_type xtype) 
{
    switch(xtype)
    {
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    case xs_date:
        return 1;
    case xs_time:
        return 2;
    case xs_duration:
	return 3;
    case xs_dayTimeDuration:
        return 4;
    case xs_yearMonthDuration:
        return 5;
    case xs_dateTime:
        return 6;
    case xs_gYear:
        return 7;
    case xs_gYearMonth:
        return 8;
    case xs_gMonth:
        return 9;
    case xs_gMonthDay:
        return 10;
    case xs_gDay:
        return 11;
    case xs_untypedAtomic:
    case xs_string:
        return 12;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_ne_tbl[13][13] = 
{
                           /*numeric*/            /*xs:date*/         /*xs:time*/         /*xs:duration*/           /*xs:dayTimeDuration*/             /*xs:yearMonthDuration*/             /*xs:dateTime*/                /*xs_gYear*/         /*xs:gYearMonth*/        /*xs_gMonth*/         /*xs:gMonthDay*/          /*xs_gDay*/        /*xs_string*/
/*numeric*/                {op_numeric_not_equal, NULL,               NULL,               NULL,                     NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,                 NULL,                     NULL,              NULL},
/*xs:date*/                {NULL,                 op_date_not_equal,  NULL,               NULL,                     NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,                 NULL,                     NULL,              NULL},
/*xs:time*/                {NULL,                 NULL,               op_time_not_equal,  NULL,                     NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,                 NULL,                     NULL,              NULL},
/*xs:duration*/            {NULL,                 NULL,               NULL,               op_duration_not_equal,    NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,                 NULL,                     NULL,              NULL},
/*xs:dayTimeDuration*/    {NULL,                 NULL,               NULL,               NULL,                     op_dayTimeDuration_not_equal,       NULL,                                 NULL,                          NULL,                NULL,                    NULL,                 NULL,                     NULL,              NULL},
/*xs:yearMonthDuration*/  {NULL,                 NULL,               NULL,               NULL,                     NULL,                               op_yearMonthDuration_not_equal,       NULL,                          NULL,                NULL,                    NULL,                 NULL,                     NULL,              NULL},
/*xs:dateTime*/            {NULL,                 NULL,               NULL,               NULL,                     NULL,                               NULL,                                 op_dateTime_not_equal,         NULL,                NULL,                    NULL,                 NULL,                     NULL,              NULL},
/*xs:gYear*/               {NULL,                 NULL,               NULL,               NULL,                     NULL,                               NULL,                                 NULL,                          op_gYear_not_equal,  NULL,                    NULL,                 NULL,                     NULL,              NULL},
/*xs:gYearMonth*/          {NULL,                 NULL,               NULL,               NULL,                     NULL,                               NULL,                                 NULL,                          NULL,                op_gYearMonth_not_equal, NULL,                 NULL,                     NULL,              NULL},
/*xs:gMonth*/              {NULL,                 NULL,               NULL,               NULL,                     NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    op_gMonth_not_equal,  NULL,                     NULL,              NULL},
/*xs:gMonthDay*/           {NULL,                 NULL,               NULL,               NULL,                     NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,                 op_gMonthDay_not_equal,   NULL,              NULL},
/*xs:gDay*/                {NULL,                 NULL,               NULL,               NULL,                     NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,                 NULL,                     op_gDay_not_equal, NULL},
/*xs:string*/              {NULL,                 NULL,               NULL,               NULL,                     NULL,                               NULL,                                 NULL,                          NULL,                NULL,                    NULL,                 NULL,                     NULL,              op_map_fn_compare_not_equal}
};


int simple_type2bin_op_gt_index(xmlscm_type xtype) 
{
    switch(xtype)
    {
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    case xs_date:
        return 1;
    case xs_time:
        return 2;
    case xs_dayTimeDuration:
        return 3;
    case xs_yearMonthDuration:
        return 4;
    case xs_dateTime:
        return 5;
    case xs_string:
    case xs_untypedAtomic: 
        return 6;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_gt_tbl[7][7] = 
{
                           /*numeric*/               /*xs:date*/            /*xs:time*/            /*xs:dayTimeDuration*/             /*xs:yearMonthDuration*/             /*xs:dateTime*/                /*xs_string*/
/*numeric*/                {op_numeric_greater_than, NULL,                  NULL,                  NULL,                               NULL,                                 NULL,                          NULL},
/*xs:date*/                {NULL,                    op_date_greater_than,  NULL,                  NULL,                               NULL,                                 NULL,                          NULL},
/*xs:time*/                {NULL,                    NULL,                  op_time_greater_than,  NULL,                               NULL,                                 NULL,                          NULL},
/*xs:dayTimeDuration*/    {NULL,                    NULL,                  NULL,                  op_dayTimeDuration_greater_than,    NULL,                                 NULL,                          NULL},
/*xs:yearMonthDuration*/  {NULL,                    NULL,                  NULL,                  NULL,                               op_yearMonthDuration_greater_than,    NULL,                          NULL},
/*xs:dateTime*/            {NULL,                    NULL,                  NULL,                  NULL,                               NULL,                                 op_dateTime_greater_than,      NULL},
/*xs:string*/              {NULL,                    NULL,                  NULL,                  NULL,                               NULL,                                 NULL,                          op_map_fn_compare_greater_than}
};

int simple_type2bin_op_lt_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    case xs_date:
        return 1;
    case xs_time:
        return 2;
    case xs_dayTimeDuration:
        return 3;
    case xs_yearMonthDuration:
        return 4;
    case xs_dateTime:
        return 5;
    case xs_string:
    case xs_untypedAtomic: 
        return 6;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_lt_tbl[7][7] = 
{
                           /*numeric*/            /*xs:date*/            /*xs:time*/            /*xs:dayTimeDuration*/             /*xs:yearMonthDuration*/             /*xs:dateTime*/                /*xs_string*/
/*numeric*/                {op_numeric_less_than, NULL,                  NULL,                  NULL,                               NULL,                                 NULL,                          NULL},
/*xs:date*/                {NULL,                 op_date_less_than,     NULL,                  NULL,                               NULL,                                 NULL,                          NULL},
/*xs:time*/                {NULL,                 NULL,                  op_time_less_than,     NULL,                               NULL,                                 NULL,                          NULL},
/*xs:dayTimeDuration*/    {NULL,                 NULL,                  NULL,                  op_dayTimeDuration_less_than,       NULL,                                 NULL,                          NULL},
/*xs:yearMonthDuration*/  {NULL,                 NULL,                  NULL,                  NULL,                               op_yearMonthDuration_less_than,       NULL,                          NULL},
/*xs:dateTime*/            {NULL,                 NULL,                  NULL,                  NULL,                               NULL,                                 op_dateTime_less_than,         NULL},
/*xs:string*/              {NULL,                 NULL,                  NULL,                  NULL,                               NULL,                                 NULL,                          op_map_fn_compare_less_than}
};

int simple_type2bin_op_ge_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    case xs_date:
        return 1;
    case xs_time:
        return 2;
    case xs_dayTimeDuration:
        return 3;
    case xs_yearMonthDuration:
        return 4;
    case xs_dateTime:
        return 5;
    case xs_string:
    case xs_untypedAtomic: 
        return 6;
    default:
        return -1;
    }
}


bin_op_tuple_cell_tuple_cell op_ge_tbl[7][7] =
{
                           /*numeric*/                /*xs:date*/            /*xs:time*/            /*xs:dayTimeDuration*/             /*xs:yearMonthDuration*/             /*xs:dateTime*/                /*xs_string*/
/*numeric*/                {op_numeric_greater_equal, NULL,                  NULL,                  NULL,                               NULL,                                 NULL,                          NULL},
/*xs:date*/                {NULL,                     op_date_greater_equal, NULL,                  NULL,                               NULL,                                 NULL,                          NULL},
/*xs:time*/                {NULL,                     NULL,                  op_time_greater_equal, NULL,                               NULL,                                 NULL,                          NULL},
/*xs:dayTimeDuration*/    {NULL,                     NULL,                  NULL,                  op_dayTimeDuration_greater_equal,   NULL,                                 NULL,                          NULL},
/*xs:yearMonthDuration*/  {NULL,                     NULL,                  NULL,                  NULL,                               op_yearMonthDuration_greater_equal,   NULL,                          NULL},
/*xs:dateTime*/            {NULL,                     NULL,                  NULL,                  NULL,                               NULL,                                 op_dateTime_greater_equal,     NULL},
/*xs:string*/              {NULL,                     NULL,                  NULL,                  NULL,                               NULL,                                 NULL,                          op_map_fn_compare_greater_equal}
};

int simple_type2bin_op_le_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_integer:
    case xs_decimal: 
    case xs_float: 
    case xs_double: 
        return 0;
    case xs_date:
        return 1;
    case xs_time:
        return 2;
    case xs_dayTimeDuration:
        return 3;
    case xs_yearMonthDuration:
        return 4;
    case xs_dateTime:
        return 5;
    case xs_string:
    case xs_untypedAtomic: 
        return 6;
    default:
        return -1;
    }
}

bin_op_tuple_cell_tuple_cell op_le_tbl[7][7] = 
{
                           /*numeric*/             /*xs:date*/            /*xs:time*/            /*xs:dayTimeDuration*/             /*xs:yearMonthDuration*/             /*xs:dateTime*/                /*xs_string*/
/*numeric*/                {op_numeric_less_equal, NULL,                  NULL,                  NULL,                               NULL,                                 NULL,                          NULL},
/*xs:date*/                {NULL,                  op_date_less_equal,    NULL,                  NULL,                               NULL,                                 NULL,                          NULL},
/*xs:time*/                {NULL,                  NULL,                  op_time_less_equal,    NULL,                               NULL,                                 NULL,                          NULL},
/*xs:dayTimeDuration*/    {NULL,                  NULL,                  NULL,                  op_dayTimeDuration_less_equal,      NULL,                                 NULL,                          NULL},
/*xs:yearMonthDuration*/  {NULL,                  NULL,                  NULL,                  NULL,                               op_yearMonthDuration_less_equal,      NULL,                          NULL},
/*xs:dateTime*/            {NULL,                  NULL,                  NULL,                  NULL,                               NULL,                                 op_dateTime_less_equal,        NULL},
/*xs:string*/              {NULL,                  NULL,                  NULL,                  NULL,                               NULL,                                 NULL,                          op_map_fn_compare_less_equal}
};



xq_binary_op_info_type xq_binary_op_info[] =
{
/*xqbop_add*/  {op_add,  op_numeric_add_tbl,  simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_add_tbl,  6,  simple_type2bin_op_add_index},
/*xqbop_sub*/  {op_sub,  op_numeric_sub_tbl,  simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_sub_tbl,  6,  simple_type2bin_op_sub_index},
/*xqbop_mul*/  {op_mul,  op_numeric_mul_tbl,  simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_mul_tbl,  3,  simple_type2bin_op_mul_index},
/*xqbop_div*/  {op_div,  op_numeric_div_tbl,  simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_div_tbl,  3,  simple_type2bin_op_div_index},
/*xqbop_idiv*/ {op_idiv, op_numeric_idiv_tbl, simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_idiv_tbl, 1,  simple_type2bin_op_idiv_index},
/*xqbop_mod*/  {op_mod,  op_numeric_mod_tbl,  simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_mod_tbl,  1,  simple_type2bin_op_mod_index},
/*xqbop_eq*/   {op_eq,   op_numeric_eq_tbl,   simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_eq_tbl,   13, simple_type2bin_op_eq_index},
/*xqbop_ne*/   {op_ne,   op_numeric_ne_tbl,   simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_ne_tbl,   13, simple_type2bin_op_ne_index},
/*xqbop_gt*/   {op_gt,   op_numeric_gt_tbl,   simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_gt_tbl,   7,  simple_type2bin_op_gt_index},
/*xqbop_lt*/   {op_lt,   op_numeric_lt_tbl,   simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_lt_tbl,   7, simple_type2bin_op_lt_index},
/*xqbop_ge*/   {op_ge,   op_numeric_ge_tbl,   simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_ge_tbl,   7, simple_type2bin_op_ge_index},
/*xqbop_le*/   {op_le,   op_numeric_le_tbl,   simple_type2bin_op_numeric_index, (bin_op_tuple_cell_tuple_cell*)op_le_tbl,   7, simple_type2bin_op_le_index}
};



const char* _op_numeric_binary_generic_err_c_str(xq_binary_op_type t)
{
    switch (t)
    {
    case xqbop_add:
        return "Invalid arguments of op:numeric-add";
    case xqbop_sub:
        return "Invalid arguments of op:numeric-subtract";
    case xqbop_mul:
        return "Invalid arguments of op:numeric-multiply";
    case xqbop_div:
        return "Invalid arguments of op:numeric-divide";
    case xqbop_idiv:
        return "Invalid arguments of op:numeric-integer-divide";
    case xqbop_mod:
        return "Invalid arguments of op:numeric-mod";
    case xqbop_eq:
        return "Invalid arguments of op:numeric-equal";
    case xqbop_ne:
        return "Invalid arguments of op:numeric-equal";
    case xqbop_gt:
        return "Invalid arguments of op:numeric-greater-than";
    case xqbop_lt:
        return "Invalid arguments of op:numeric-less-than";
    case xqbop_ge:
        return "Invalid arguments of op:numeric-greater-than";
    case xqbop_le:
        return "Invalid arguments of op:numeric-less-than";
    default:
        return NULL;
    }
}

const char* _op_binary_generic_err_c_str(xq_binary_op_type t)
{
    switch (t)
    {
    case xqbop_add:
        return "Invalid arguments of '+' operation";
    case xqbop_sub:
        return "Invalid arguments of '-' operation";
    case xqbop_mul:
        return "Invalid arguments of '*' operation";
    case xqbop_div:
        return "Invalid arguments of 'div' operation";
    case xqbop_idiv:
        return "Invalid arguments of 'idiv' operation";
    case xqbop_mod:
        return "Invalid arguments of 'mod' operation";
    case xqbop_eq:
        return "Invalid arguments of 'eq' operation";
    case xqbop_ne:
        return "Invalid arguments of 'ne' operation";
    case xqbop_gt:
        return "Invalid arguments of 'gt' operation";
    case xqbop_lt:
        return "Invalid arguments of 'lt' operation";
    case xqbop_ge:
        return "Invalid arguments of 'ge' operation";
    case xqbop_le:
        return "Invalid arguments of 'le' operation";
    default:
        return NULL;
    }
}

tuple_cell _op_numeric_generic(xq_binary_op_type t, const tuple_cell &a1, const tuple_cell &a2)
{
    U_ASSERT(a1.is_atomic());
    U_ASSERT(a2.is_atomic());

    xq_binary_op_info_type &info = xq_binary_op_info[(int)t];

    int i1 = (info.numeric_index)(a1.get_atomic_type());
    int i2 = (info.numeric_index)(a2.get_atomic_type());
    if ((i1 == -1) || (i2 == -1))
        throw USER_EXCEPTION2(XPTY0004, _op_numeric_binary_generic_err_c_str(t));
    else
        return ((info.numeric_table)[i1][i2])(a1, a2);
}

tuple_cell _op_generic(xq_binary_op_type t, const tuple_cell &a1, const tuple_cell &a2)
{
    if (a1.is_eos() || a2.is_eos()) return tuple_cell::eos();

    U_ASSERT(a1.is_atomic());
    U_ASSERT(a2.is_atomic());

    xq_binary_op_info_type &info = xq_binary_op_info[(int)t];

    int i1 = (info.map_index)(a1.get_atomic_type());
    int i2 = (info.map_index)(a2.get_atomic_type());
    bin_op_tuple_cell_tuple_cell op = (info.map_table)[info.map_table_side_size * i1 + i2];

    if ((i1 == -1) || (i2 == -1) || (op == NULL))
	throw USER_EXCEPTION2(XPTY0004, _op_binary_generic_err_c_str(t));

    else
        return op(a1, a2);
}

bin_op_tuple_cell_tuple_cell get_binary_op(xq_binary_op_type t, xmlscm_type t1, xmlscm_type t2)
{
    xq_binary_op_info_type &info = xq_binary_op_info[(int)t];

    if (t1 == xs_anyType || t1 == xs_anySimpleType || t1 == xs_anyAtomicType ||
        t2 == xs_anyType || t2 == xs_anySimpleType || t2 == xs_anyAtomicType)
        return info.fun;

    int i1 = (info.map_index)(t1);
    int i2 = (info.map_index)(t2);
    if (i1 == -1 || i2 == -1)
        throw USER_EXCEPTION2(SE1003, "Invalid types of arguments");

    if (!(i1 + i2)) // both numeric
    {
        i1 = (info.numeric_index)(t1);
        i2 = (info.numeric_index)(t2);
        if (i1 == -1 || i2 == -1)
            throw USER_EXCEPTION2(SE1003, "Invalid types of arguments");

        return (info.numeric_table)[i1][i2];
    }
    else
    {
        bin_op_tuple_cell_tuple_cell op = (info.map_table)[info.map_table_side_size * i1 + i2];
        if (op)
            return op;
        else
            throw USER_EXCEPTION2(SE1003, "Invalid types of arguments");
    }
}


un_op_tuple_cell op_numeric_plus_tbl[5] = 
{
/*xs_untypedAtomic*/                    /*xs_integer*/                    /*xs_decimal*/                    /*xs_float*/                    /*xs_double*/
op_numeric_unary_plus_xs_untypedAtomic, op_numeric_unary_plus_xs_integer, op_numeric_unary_plus_xs_decimal, op_numeric_unary_plus_xs_float, op_numeric_unary_plus_xs_double
};

un_op_tuple_cell op_numeric_minus_tbl[5] = 
{
/*xs_untypedAtomic*/                    /*xs_integer*/                    /*xs_decimal*/                    /*xs_float*/                    /*xs_double*/
op_numeric_unary_minus_xs_untypedAtomic, op_numeric_unary_minus_xs_integer, op_numeric_unary_minus_xs_decimal, op_numeric_unary_minus_xs_float, op_numeric_unary_minus_xs_double
};

xq_unary_op_info_type xq_unary_op_info[] =
{
/*xquop_plus*/ {op_plus,  op_numeric_plus_tbl},
/*xquop_minus*/{op_minus, op_numeric_minus_tbl}
};

int simple_type2un_op_numeric_index(xmlscm_type xtype)
{
    switch(xtype)
    {
    case xs_untypedAtomic: 
        return 0;
    case xs_integer:
        return 1;
    case xs_decimal: 
        return 2;
    case xs_float: 
        return 3;
    case xs_double: 
        return 4;
    default:
        return -1;
    }
}

const char* _op_numeric_unary_generic_err_c_str(xq_unary_op_type t)
{
    switch (t)
    {
    case xquop_plus:
        return "Invalid arguments a op:numeric-unary-plus";
    case xquop_minus:
        return "Invalid arguments a op:numeric-unary-minus";
    default:
        return NULL;
    }
}

const char* _op_unary_generic_err_c_str(xq_unary_op_type t)
{
    switch (t)
    {
    case xquop_plus:
        return "Invalid arguments of unary '+'";
    case xquop_minus:
        return "Invalid arguments of unary '-'";
    default:
        return NULL;
    }
}

tuple_cell _op_generic(xq_unary_op_type t, const tuple_cell &a1, const char* (*err_msg_fun)(xq_unary_op_type))
{
    if (a1.is_eos()) return tuple_cell::eos();

    U_ASSERT(a1.is_atomic());

    int i1 = simple_type2un_op_numeric_index(a1.get_atomic_type());
    if (i1 == -1)
        throw USER_EXCEPTION2(XPTY0004, err_msg_fun(t));
    else
        return (xq_unary_op_info[(int)t].numeric_table[i1])(a1);
}

un_op_tuple_cell get_unary_op(xq_unary_op_type t, xmlscm_type t1)
{
    if (t1 == xs_anyType || t1 == xs_anySimpleType || t1 == xs_anyAtomicType)
        return xq_unary_op_info[(int)t].fun;

    int i1 = simple_type2un_op_numeric_index(t1);
    if (i1 == -1)
        throw USER_EXCEPTION2(SE1003, "Invalid type of argument");

    return xq_unary_op_info[(int)t].numeric_table[i1];
}


tuple_cell op_add (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_add,  a1, a2); }
tuple_cell op_sub (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_sub,  a1, a2); }
tuple_cell op_mul (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_mul,  a1, a2); }
tuple_cell op_div (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_div,  a1, a2); }
tuple_cell op_idiv(const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_idiv, a1, a2); }
tuple_cell op_mod (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_mod,  a1, a2); }
tuple_cell op_eq  (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_eq,   a1, a2); }
tuple_cell op_ne  (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_ne,   a1, a2); }
tuple_cell op_gt  (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_gt,   a1, a2); }
tuple_cell op_lt  (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_lt,   a1, a2); }
tuple_cell op_ge  (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_ge,   a1, a2); }
tuple_cell op_le  (const tuple_cell &a1, const tuple_cell &a2) { return _op_generic(xqbop_le,   a1, a2); }

tuple_cell op_numeric_add           (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_add,  a1, a2); }
tuple_cell op_numeric_subtract      (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_sub,  a1, a2); }
tuple_cell op_numeric_multiply      (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_mul,  a1, a2); }
tuple_cell op_numeric_divide        (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_div,  a1, a2); }
tuple_cell op_numeric_integer_divide(const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_idiv, a1, a2); }
tuple_cell op_numeric_mod           (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_mod,  a1, a2); }
tuple_cell op_numeric_equal         (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_eq,   a1, a2); }
tuple_cell op_numeric_not_equal     (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_ne,   a1, a2); }
tuple_cell op_numeric_greater_than  (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_gt,   a1, a2); }
tuple_cell op_numeric_less_than     (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_lt,   a1, a2); }
tuple_cell op_numeric_greater_equal (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_ge,   a1, a2); }
tuple_cell op_numeric_less_equal    (const tuple_cell &a1, const tuple_cell &a2) { return _op_numeric_generic(xqbop_le,   a1, a2); }

tuple_cell op_plus (const tuple_cell &a1) { return _op_generic(xquop_plus,  a1, _op_unary_generic_err_c_str); }
tuple_cell op_minus(const tuple_cell &a1) { return _op_generic(xquop_minus, a1, _op_unary_generic_err_c_str); }

tuple_cell op_numeric_unary_plus (const tuple_cell &a1) { return _op_generic(xquop_plus,  a1, _op_numeric_unary_generic_err_c_str); }
tuple_cell op_numeric_unary_minus(const tuple_cell &a1) { return _op_generic(xquop_minus, a1, _op_numeric_unary_generic_err_c_str); }

