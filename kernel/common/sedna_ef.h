/*
 * File:  sedna_ef.h
 * Copyright (C) 2005 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SEDNA_EF_H
#define _SEDNA_EF_H

#define SEDNA_ERROR_MSG_BUF_SIZE	128

typedef enum sedna_atomic_type {SEDNATYPE_integer, SEDNATYPE_float, SEDNATYPE_double, SEDNATYPE_string} SEDNA_ATOMIC_TYPE;

typedef int		SEDNA_integer;
typedef float	SEDNA_float;
typedef double	SEDNA_double;
typedef char 	*SEDNA_string;

typedef struct sedna_atomic_value
{
	SEDNA_ATOMIC_TYPE type;
	union
	{
		SEDNA_integer	val_integer;
		SEDNA_float		val_float;
		SEDNA_double	val_double;
		SEDNA_string	val_string;
	};
} SEDNA_ATOMIC_VALUE;

typedef struct sedna_sequence_item
{
	SEDNA_ATOMIC_VALUE	data;
	struct sedna_sequence_item	*next;
} SEDNA_SEQUENCE_ITEM;

typedef struct sedna_ef_init
{
	void *(*sedna_malloc)(size_t);
	void (*sedna_free)(void *);
	SEDNA_SEQUENCE_ITEM *node_buf;
	void *ptr;
} SEDNA_EF_INIT;

typedef struct sedna_ef_args
{
	int length;
	SEDNA_SEQUENCE_ITEM **args;
} SEDNA_EF_ARGS;



#endif /* _SEDNA_EF_H */
