/*
 * BTrie Sedna specialization
 * Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "btrie_internal.h"
#include "btrie_readstate.h"
#include "btrie_utils.h"

#include <set>
#include <stack>
#include <stdio.h>

typedef std::set<xptr_t> xptrset;
typedef std::stack<char *> statestack;

static char buffer[PAGE_SIZE];
static char buffer2[PAGE_SIZE];


inline static char ctox(int a) {
    return ((a > 9) ? 'A' - 9 + a : '0' + a);
}

static
int escape_str(char * buf, char * str, size_t len) {
    char * c = buf;

    for(intptr_t i; i < len; ++i) {
        if ((str[i] >= '\040') && (str[i] < '\176')) {
            *(c++) = str[i];
        } else {
            int chr = str[i];
            *(c++) = '~';
            *(c++) = ctox(chr & 0x0000000fL);
            *(c++) = ctox((chr >> 4) & 0x0000000fL);
        }
    }

    *c = '\0';

    return c - buf;
}

static
void print_state(char edge, char * state, xptrset * pagesToSee, FILE * f, xptr_t p, bool isroot = false) {
    state_descriptor dsc;
    state = read_state(state, &dsc);

    if ((dsc.flags & STATE_LONG_JUMP) > 0) {
	U_ASSERT(!isroot);
        xptr_t to = block_xptr(dsc.long_jump);
        READ_PAGE(to);
        READ_PAGE(p);
        pagesToSee->insert(to);
        escape_str(buffer, &edge, 1);
//        fprintf(f, " x%08llx -> x%08llx [ label = \"%s\" ];\n", p.to_logical_int(), to.to_logical_int(), buffer);
//        fprintf(f, " x%08llx -> x%08llx [ label = \"%s\" ];\n", p, to, buffer);
//        fprintf(f, " ~JUMP~ ");
    } else {
        if ((dsc.flags & STATE_NO_PREFIX) == 0) {
//            escape_str(buffer, dsc.prefix, dsc.prefix_len);
//           fprintf(f, "(%02x+%s~ ", (int) edge, buffer);
        } else {
//            fprintf(f, "(%02x~ ", (int) edge);
        }

        for (int i = 0; i < dsc.edge_count; ++i) {
            print_state(dsc.edges[i], state + dsc.pointers[i], pagesToSee, f, p);
        }

//        fprintf(f, ")\n");
    }
}

float btrie_collect_page_stats(xptr_t page, xptrset * pagesToSee, FILE * f) {
    st_page_header page_header;
    st_read_page_header(page, &page_header);

    double filling = (1 - (float) page_header.free_space / (float) PAGE_SIZE) * 100;

//    fprintf(f, " x%08llx [ label = \"%.2f\" ];\n", page.to_logical_int(), filling);
//    fprintf(f, " x%08llx [ label = \"%.2f\" ];\n", page, filling);

/*
    fprintf(f, "Block : %08llx; Free : %d; Filling : %.2f%%; Tries: %d;\n {\n",
            page.to_logical_int(),
            page_header.free_space,
            (1 - (float) page_header.free_space / (float) PAGE_SIZE) * 100,
            page_header.trie_count);
*/
/*    fprintf(f, "Block : %08llx; Free : %d; Filling : %.2f%%; Tries: %d;\n ",
            page,
            page_header.free_space,
            (1 - (float) page_header.free_space / (float) PAGE_SIZE) * 100,
            page_header.trie_count);
*/
    for (int i = 0; i < page_header.trie_count; ++i) {
//        fprintf(f, "  trie ");
        print_state('~', get_root_state(&page_header) + page_header.tries[i], pagesToSee, f, page);
//        fprintf(f, "\n");
    };
    return (1 - (float) page_header.free_space / (float) PAGE_SIZE) * 100;

};

static int n = 0;

float btrie_collect_stat(xptr_t entry_point, int i) {
    xptrset pagesToSee;
    xptrset pagesAlreadySeen;
    FILE * f;

	float averageLoad = 0;
	int blocksSeen = 0;
	int blocksLoadedUnderThreshold = 0;

//    sprintf(buffer, "/tmp/btrie.%02d.stats", n++);
    sprintf(buffer, "/home/al/btrie.stats", n++);
    f = fopen(buffer, "w");

    pagesToSee.insert(block_xptr(entry_point));

//    fprintf(f, "digraph page_structure { \n node [shape = rect]; \n");

    while (!pagesToSee.empty()) {
        xptr_t p = *(pagesToSee.begin());

        averageLoad = btrie_collect_page_stats(p, &pagesToSee, f);
        if (averageLoad < 30) blocksLoadedUnderThreshold++;
        blocksSeen++;

        pagesToSee.erase(block_xptr(p));
        pagesAlreadySeen.insert(p);
    }

//    fprintf(f, "} \n");

    fclose(f);

//    printf("Blocks loaded less than 30 percent is %f with %d string inserted \n", 100*((float)blocksLoadedUnderThreshold / (float)blocksSeen), i);
    printf("%d %f\n", i, 100*((float)blocksLoadedUnderThreshold / (float)blocksSeen));
    return  (float) blocksLoadedUnderThreshold / (float) blocksSeen;
}


inline static bool checkTrieList(struct st_page_header * hdr) {
    int off =-1;
    for (int i = 0; i < hdr->trie_count; ++i) {
        U_ASSERT((int) hdr->tries[i] > off);
        off = (int) hdr->tries[i];
        U_ASSERT(off < PAGE_SIZE);
    }
    return true;
}

bool btrie_check_page_ends(xptr_t page, xptrset * pagesToSee, FILE * f)
{
	st_page_header page_header;
	st_read_page_header(page, &page_header);
	state_descriptor dsc;

    char * offset = (char *) XADDR(page) + page_header.trie_offset;
    sptr_t * state_array = page_header.tries;
    int c = page_header.trie_count;

    sptr_t max_offset = 0;

//	fprintf(f, "\n pghdr_trie count %d, offset chngs: ", page_header.trie_count);

	U_ASSERT(c > 0);

	do {
		for (int i = 0; i < c; ++i) {
			if (max_offset < state_array[i]) {
				max_offset = state_array[i];
			}
		}

		offset = read_state(offset + max_offset, &dsc);
		state_array = dsc.pointers;
		max_offset = 0;

//     	    fprintf(f, "%d, ", (offset - (char*) XADDR(page)));

		c = dsc.edge_count;
	} while (c > 0);

	if ((offset - (char*) XADDR(page)) != page_header.data_end) {
//		fprintf(f, " . pghdr.d_end is %d", page_header.data_end);
		U_ASSERT(false);
		return false;

	}

	checkTrieList(&page_header);


	for (int i = 0; i < page_header.trie_count; ++i)
	{
	    print_state('~', get_root_state(&page_header) + page_header.tries[i], pagesToSee, f, page, true);
	}
	return true;
}





bool btrie_data_end_check(xptr_t entry_point)
{
	xptrset pagesToSee;
	xptrset pagesAlreadySeen;
	bool assert_passed = true;

	FILE * f;

//	sprintf(buffer2, "/home/al/Programming/diploma/trie-testing/btrie_check.txt");
//	f = fopen(buffer2, "a");

	pagesToSee.insert(block_xptr(entry_point));
	while (!pagesToSee.empty())
	{
	xptr_t p = *(pagesToSee.begin());
        assert_passed &= btrie_check_page_ends(p, &pagesToSee, f);
        pagesToSee.erase(block_xptr(p));
        pagesAlreadySeen.insert(p);
	}
//	fprintf(f, " asrt ok:%b, ", assert_passed);
//	fclose(f);
	return assert_passed;
}
