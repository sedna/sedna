/*
 *  Generator for files numeric_operations.cpp and numeric_operations.h
 */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

const char *ops[] =
{
    "add", "+",
    "subtract", "-",
    "multiply", "*",
    "divide", "xs_divide",
    "integer_divide", "xs_integer_divide",
    "mod", "xs_mod",
    "equal", "==",
    "less_than", "<",
    "greater_than", ">",
    "not_equal", "!=",
    "less_equal", "<=",
    "greater_equal", ">=",
    NULL, NULL
};

const char *types[] =
{
    "xs_integer", "10",
    "xs_decimal", "20",
    "xs_float", "30",
    "xs_double", "40",
    "xs_untypedAtomic", "50",
    NULL, NULL
};

typedef int (*gen_fun)(FILE *f, const char *op, const char *ops, const char *type1, const char *type2);

int type2weight(const char *type)
{
    size_t i = 0;

    while (types[i])
    {
        if (!strcmp(types[i], type))
            return atoi(types[i + 1]);

        i += 2;
    }

    return -1;
}

const char *weight2type(int w)
{
    size_t i = 1;
    char t[5];

    sprintf(t, "%i", w);

    while (types[i])
    {
        if (!strcmp(types[i], t))
            return types[i - 1];

        i += 2;
    }

    return NULL;
}

const char *find_gct(const char *op, const char *type1, const char *type2)
{
    int w1, w2;

    if (!strcmp(type1, "xs_untypedAtomic") && !strcmp(type2, "xs_untypedAtomic"))
    {
        if (!strcmp(op, "idiv"))
            return "xs_integer";
        else
            return "xs_double";
    }
    else if (!strcmp(type1, "xs_untypedAtomic"))
    {
        if (!strcmp(op, "idiv"))
            return find_gct(op, "xs_integer", type2);
        else
            return find_gct(op, "xs_double", type2);
    }
    else if (!strcmp(type2, "xs_untypedAtomic"))
    {
        if (!strcmp(op, "idiv"))
            return find_gct(op, type1, "xs_integer");
        else
            return find_gct(op, type1, "xs_double");
    }
    else
    {
        w1 = type2weight(type1);
        w2 = type2weight(type2);

        if (w1 == -1 || w2 == -1)
            return NULL;

        return weight2type((int)fmax(w1, w2));
    }
}

void generate_arg(FILE *f, const char *gct, const char *type, const char *arg)
{
    if (!strcmp(gct, type))
    {
        fprintf(f, "%s.get_%s()", arg, type);
    }
    else
    {
        fprintf(f, "%s2%s(%s)", type, gct, arg);
    }
}

int gfun_h(FILE *f, const char *op, const char *ops, const char *type1, const char *type2)
{
    fprintf(f, "tuple_cell op_numeric_%s_%s_%s(const tuple_cell &a1, const tuple_cell &a2);\n", op, type1, type2);
    return 0;
}

int gfun_cpp(FILE *f, const char *op, const char *ops, const char *type1, const char *type2)
{
    const char *gct = find_gct(op, type1, type2);

    if (!gct)
        return -1;

    fprintf(f, "tuple_cell op_numeric_%s_%s_%s(const tuple_cell &a1, const tuple_cell &a2)\n{\n", op, type1, type2);
    fprintf(f, "    return tuple_cell::atomic(");

    if (!strcmp(op, "divide") || !strcmp(op, "integer_divide") || !strcmp(op, "mod"))
    {
        fprintf(f, "%s(", ops);
        generate_arg(f, gct, type1, "a1");
        fprintf(f, ", ");
        generate_arg(f, gct, type2, "a2");
        fprintf(f, ")");
    }
    /* Copied from scheme generator:
       This temp hack for doubles is needed because
       you couldn't use '==' in C/C++ to compare them -
       '==' should be changed to 'a-b < epsilon'.
       But operands transposing gives good effect here too ...
    */
    else if (!strcmp(op, "equal") && !strcmp(type1, "xs_double"))
    {
        generate_arg(f, gct, type2, "a2");
        fprintf(f, " %s ", ops);
        generate_arg(f, gct, type1, "a1");
    }
    else
    {
        generate_arg(f, gct, type1, "a1");
        fprintf(f, " %s ", ops);
        generate_arg(f, gct, type2, "a2");
    }
    fprintf(f, ");\n}\n\n");

    return 0;
}

int generate_simple_ops(FILE *f, const char **op_lst, const char **type_lst, gen_fun fun)
{
    size_t opi = 0, t1i, t2i;
    const char *op, *ops, *t1, *t2;
    int err;

    while ((op = op_lst[opi]))
    {
        ops = op_lst[opi + 1];
        t1i = 0;

        while ((t1 = types[t1i]))
        {
            t2i = 0;
            while ((t2 = types[t2i]))
            {
                if ((err = fun(f, op, ops, t1, t2)))
                    return err;

                t2i += 2;
            }
            t1i += 2;
        }

        opi += 2;
    }

    return 0;
}

int main(int argc, char** argv)
{
    FILE *num_ops;

    // first: header file
    if ((num_ops = fopen("numeric_operations.h", "wb")) == NULL)
    {
        fprintf(stderr, "Cannot open file to write header to: numeric_operations.h");
        return 1;
    }

    // static part
    fprintf(num_ops, "#ifndef _NUMERIC_OPERATIONS_H\n");
    fprintf(num_ops, "#define _NUMERIC_OPERATIONS_H\n");
    fprintf(num_ops, "\n// This file was generated. Don't edit it!!!\n\n");
    fprintf(num_ops, "#include \"common/sedna.h\"\n");
    fprintf(num_ops, "#include \"tr/executor/base/PPBase.h\"\n\n");

    // dynamic part
    if (generate_simple_ops(num_ops, ops, types, gfun_h))
    {
        fprintf(stderr, "error while generating numeric_operations.h");
        return 2;
    }

    // another static part
    fprintf(num_ops, "\ntuple_cell op_numeric_unary_plus_xs_integer(const tuple_cell& a);\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_plus_xs_decimal(const tuple_cell& a);\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_plus_xs_float(const tuple_cell& a);\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_plus_xs_double(const tuple_cell& a);\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_plus_xs_untypedAtomic(const tuple_cell& a);\n\n");

    fprintf(num_ops, "tuple_cell op_numeric_unary_minus_xs_integer(const tuple_cell& a);\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_minus_xs_decimal(const tuple_cell& a);\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_minus_xs_float(const tuple_cell& a);\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_minus_xs_double(const tuple_cell& a);\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_minus_xs_untypedAtomic(const tuple_cell& a);\n\n");

    fprintf(num_ops, "#endif\n");

    fclose(num_ops);


    // second: cpp file
    if ((num_ops = fopen("numeric_operations.cpp", "wb")) == NULL)
    {
        fprintf(stderr, "Cannot open file to write header to: numeric_operations.cpp");
        return 1;
    }

    // static part
    fprintf(num_ops, "\n// This file was generated. Don't edit it!!!\n\n");
    fprintf(num_ops, "#include <math.h>\n");
    fprintf(num_ops, "#include \"common/sedna.h\"\n");
    fprintf(num_ops, "#include \"tr/executor/fo/numeric_operations.h\"\n");
    fprintf(num_ops, "#include \"tr/executor/fo/casting_operations.h\"\n");
    fprintf(num_ops, "#include \"tr/executor/base/PPUtils.h\"\n");
    fprintf(num_ops, "#include \"tr/executor/base/xs_helper.h\"\n\n");

    fprintf(num_ops, "xs_decimal_t xs_integer2xs_decimal(const tuple_cell &tc) { return xs_decimal_t(tc.get_xs_integer()); }\n");
    fprintf(num_ops, "float xs_integer2xs_float(const tuple_cell &tc) { return (float)(tc.get_xs_integer()); }\n");
    fprintf(num_ops, "double xs_integer2xs_double(const tuple_cell &tc) { return (double)(tc.get_xs_integer()); }\n");
    fprintf(num_ops, "float xs_decimal2xs_float(const tuple_cell &tc) { return tc.get_xs_decimal().get_float(); }\n");
    fprintf(num_ops, "double xs_decimal2xs_double(const tuple_cell &tc) { return tc.get_xs_decimal().get_double(); }\n");
    fprintf(num_ops, "double xs_float2xs_double(const tuple_cell &tc) { return (double)(tc.get_xs_float()); }\n");
    fprintf(num_ops, "double xs_untypedAtomic2xs_double(const tuple_cell &tc) { return cast_primitive_to_xs_double(tc).get_xs_double(); }\n\n\n\n");

    // dynamic part
    if (generate_simple_ops(num_ops, ops, types, gfun_cpp))
    {
        fprintf(stderr, "error while generating numeric_operations.cpp");
        return 2;
    }

    // another static part
    fprintf(num_ops, "\ntuple_cell op_numeric_unary_plus_xs_integer(const tuple_cell& a) { return a; }\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_plus_xs_decimal(const tuple_cell& a) { return a; }\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_plus_xs_float(const tuple_cell& a) { return a; }\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_plus_xs_double(const tuple_cell& a) { return a; }\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_plus_xs_untypedAtomic(const tuple_cell& a) { return cast_primitive_to_xs_double(a); }\n\n");

    fprintf(num_ops, "tuple_cell op_numeric_unary_minus_xs_integer(const tuple_cell& a)\n");
    fprintf(num_ops, "{ return tuple_cell::atomic(-a.get_xs_integer()); }\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_minus_xs_decimal(const tuple_cell& a)\n");
    fprintf(num_ops, "{ return tuple_cell::atomic(-a.get_xs_decimal()); }\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_minus_xs_float(const tuple_cell& a)\n");
    fprintf(num_ops, "{ return tuple_cell::atomic(-a.get_xs_float()); }\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_minus_xs_double(const tuple_cell& a)\n");
    fprintf(num_ops, "{ return tuple_cell::atomic(-a.get_xs_double()); }\n");
    fprintf(num_ops, "tuple_cell op_numeric_unary_minus_xs_untypedAtomic(const tuple_cell& a)\n");
    fprintf(num_ops, "{ return tuple_cell::atomic(-cast_primitive_to_xs_double(a).get_xs_double()); }\n\n");

    fclose(num_ops);

    return 0;
}
