/*
 * File:  test_lex.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>
#include <string.h>
#include "lex.h"
#include "nidalloc.h"
#include "persistent_db_data.h"

extern char DC;

persistent_db_data* entry_point;

void test_lex_divide() {
	t_prefix	dividend;
	int			divisor;
	t_prefix	result;
	int			tmp;
	cout << "Testing lex_divide with alphabet " << ALPHABET_SIZE << "...\n";
	while (true) {
		cout << "Enter size of dividend->";
		cin >> tmp;
		dividend.size = tmp;
		if (dividend.size <= 0) break;
		dividend.prefix = (char*)nid_alloc();
		for (int i=0; i<dividend.size; i++) {
			cout <<	"\nEnter dividend["<<i<<"]->";
			cin >> tmp;
			dividend.prefix[i] = (char)tmp;
		}
		cout << "\nEnter divisor->";
		cin >> divisor;
		result = lex_divide(dividend, divisor);
		cout << "\nQuotient:";
		lex_print(result);
		nid_free(dividend.prefix);
		nid_free(result.prefix);
		cout << "\n";
	}
	cout << "--------------------------------------------------\n";
} 

void test_lex_multiply() {
	t_prefix	mult;
	int			factor;
	t_prefix	result;
	int			tmp;

	cout << "Testing lex_multiply with alphabet " << ALPHABET_SIZE << "...\n";
	while (true) {
		cout << "Enter size of multiplier->";
		cin >> mult.size;
		if (mult.size <= 0) break;
		mult.prefix = (char*)nid_alloc();
		for (int i=0; i<mult.size; i++) {
			cout <<	"\nEnter mult["<<i<<"]->";
			cin >> tmp;
			mult.prefix[i]=(char)tmp;
		}
		cout << "\nEnter factor->";
		cin >> factor;
		result = lex_multiply(mult, factor);
		cout << "\nProduct:";
		lex_print(result);
		nid_free(mult.prefix);
		nid_free(result.prefix);
		cout << "\n";
	}
	cout << "--------------------------------------------------\n";
} 

void test_lex_sum() {
	t_prefix	item1;
	t_prefix	item2;
	t_prefix	result;
	int			tmp;

	cout << "Testing lex_sum with alphabet " << ALPHABET_SIZE << "...\n";
	while (true) {
		cout << "Enter size of items to summarize->";
		cin >> item1.size;
		item2.size = item1.size;
		if (item1.size <= 0) break;
		item1.prefix = (char*)nid_alloc();
		item2.prefix = (char*)nid_alloc();
		for (int i=0; i<item1.size; i++) {
			cout <<	"\nEnter item1["<<i<<"]->";
			cin >> tmp;
			item1.prefix[i]=(char)tmp;
		}
		for (i=0; i<item1.size; i++) {
			cout <<	"\nEnter item2["<<i<<"]->";
			cin >> tmp;
			item2.prefix[i]=(char)tmp;
		}
		result = lex_sum(item1, item2);
		cout << "\nSum:";
		lex_print(result);
		nid_free(item1.prefix);
		nid_free(item2.prefix);
		nid_free(result.prefix);
		cout << "\n";
	}
	cout << "--------------------------------------------------\n";
}

void test_lex_between() {
	t_prefix	item1;
	t_prefix	item2;
	t_prefix	result;
	fnumber	p;
	int		tmp;

	cout << "Testing lex_between with alphabet " << ALPHABET_SIZE << "...\n";
	while (true) {
		item1.prefix = (char*)nid_alloc();
		item2.prefix = (char*)nid_alloc();
		cout << "Enter size of item1 to process->";
		cin >> item1.size;
		if (item1.size <= 0) break;
		for (int i=0; i<item1.size; i++) {
			cout <<	"\nEnter item1["<<i<<"]->";
			cin >> tmp;
			item1.prefix[i]=(char)tmp;
		}
		cout << "Enter size of item2 to process->";
		cin >> item2.size;
		if (item2.size <= 0) break;
		for (i=0; i<item2.size; i++) {
			cout <<	"\nEnter item2["<<i<<"]->";
			cin >> tmp;
			item2.prefix[i]=(char)tmp;
		}
		cout << "Enter proportion.numerator->";
		cin >> tmp;
		p.setx(tmp);
		cout << "Enter proportion.denominator->";
		cin >> tmp;
		p.sety(tmp);
		result = lex_between(item1, item2, p);
		cout << "\nbetween value:";
		lex_print(result);
		nid_free(item1.prefix);
		nid_free(item2.prefix);
		nid_free(result.prefix);
		cout << "\n";
	}
	cout << "--------------------------------------------------\n";
}

void test_lex_next() {
	t_prefix	item1;
	t_prefix	item2;
	t_prefix	result;
	int		tmp;

	cout << "Testing lex_next with alphabet " << ALPHABET_SIZE << "...\n";
	while (true) {
		cout << "Enter size of item to process->";
		cin >> item1.size;
		item2.size = item1.size;
		if (item1.size <= 0) break;
		item1.prefix = (char*)nid_alloc();
		item2.prefix = (char*)nid_alloc();
		for (int i=0; i<item1.size; i++) {
			cout <<	"\nEnter item1["<<i<<"]->";
			cin >> tmp;
			item1.prefix[i]=(char)tmp;
		}
		for (i=0; i<item1.size; i++) {
			cout <<	"\nEnter bound["<<i<<"]->";
			cin >> tmp;
			item2.prefix[i]=(char)tmp;
		}
		result = lex_next(item1, item2);
		cout << "\nnext value:";
		lex_print(result);
		nid_free(item1.prefix);
		nid_free(item2.prefix);
		nid_free(result.prefix);
		cout << "\n";
	}
	cout << "--------------------------------------------------\n";
}


void test_lex_equal() {
	t_prefix p1, p2;
	p1.size = 3;
	p1.prefix = (char*)nid_alloc();
	p1.prefix[0] = 12;
	p1.prefix[1] = 20;
	p1.prefix[2] = 30;
	p2.size = 3;
	p2.prefix = (char*)nid_alloc();
	p2.prefix[0] = 15;
	p2.prefix[1] = 20;
	p2.prefix[2] = 31;
	cout << "t_prefixs are equal:" << lex_cmp(p1, p2) << endl;
	nid_free(p1.prefix);
	nid_free(p2.prefix);
}

void test_nid_assign() {
	n_dsc		node;
	xptr		dsc;
	dsc.addr=&node;
	xptr		blk;
	t_prefix	p;
	p.prefix="haha";
	p.size=4;
	char	dc='a';
	nid_assign(dsc, p, dc, true);
	cout << "assigned node.nid.prefix=" << node.nid.prefix << " node.nid.dc=" << node.nid.dc << endl;
}

/***********************************************************************************
 ***********************************************************************************/

void main(void) {
	//test_lex_divide();
	//test_lex_multiply();
	//test_lex_sum();
	//test_lex_next();
	test_lex_between();
	//test_lex_equal();
	//test_nid_assign();
	cout << "DC=" << (int)(unsigned char)DC << endl;
	cout << "main() done.\n";
}