
#include "btrie_internal.h"
#include "btrie_readstate.h"
#include "btrie_utils.h"

#include <set>
#include <stack>
#include <stdio.h>

typedef std::set<xptr> xptrset;
typedef std::stack<char *> statestack;

static char buffer[PAGE_SIZE];

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
void print_state(char edge, char * state, xptrset * pagesToSee, FILE * f, xptr p) {
    state_descriptor dsc;
    state = read_state(state, &dsc);

    if ((dsc.flags & STATE_LONG_JUMP) > 0) {
        xptr to = block_xptr(dsc.long_jump);
        READ_PAGE(to);
        READ_PAGE(p);
        pagesToSee->insert(to);
        escape_str(buffer, &edge, 1);
        fprintf(f, " x%08llx -> x%08llx [ label = \"%s\" ];\n", p.to_logical_int(), to.to_logical_int(), buffer);
//        fprintf(f, " ~JUMP~ ");
    } else {
        if ((dsc.flags & STATE_NO_PREFIX) == 0) {
            escape_str(buffer, dsc.prefix, dsc.prefix_len);
//            fprintf(f, "(%02x+%s~ ", (int) edge, buffer);
        } else {
//            fprintf(f, "(%02x~ ", (int) edge);
        }

        for (int i = 0; i < dsc.edge_count; ++i) {
            print_state(dsc.edges[i], state + dsc.pointers[i], pagesToSee, f, p);
        }

//        fprintf(f, ")\n");
    }
}

void btrie_collect_page_stats(xptr page, xptrset * pagesToSee, FILE * f) {
    st_page_header page_header;
    st_read_page_header(page, &page_header);

    double filling = (1 - (float) page_header.free_space / (float) PAGE_SIZE) * 100;

    fprintf(f, " x%08llx [ label = \"%.2f\" ];\n", page.to_logical_int(), filling);

/*
    fprintf(f, "Block : %08llx; Free : %d; Filling : %.2f%%; Tries: %d;\n {\n",
            page.to_logical_int(),
            page_header.free_space,
            (1 - (float) page_header.free_space / (float) PAGE_SIZE) * 100,
            page_header.trie_count);
*/

    for (int i = 0; i < page_header.trie_count; ++i) {
//        fprintf(f, "  trie ");
        print_state('~', get_root_state(&page_header) + page_header.tries[i], pagesToSee, f, page);
//        fprintf(f, "\n");
    };
};

FILE * __bt_debug;

static int n = 0;

void btrie_collect_stat(xptr entry_point) {
    xptrset pagesToSee;
    xptrset pagesAlreadySeen;
    FILE * f;

    sprintf(buffer, "/tmp/btrie.%02d.stats", n++);
    f = fopen(buffer, "w");

    pagesToSee.insert(block_xptr(entry_point));

    fprintf(f, "digraph page_structure { \n node [shape = rect]; \n");

    while (!pagesToSee.empty()) {
        xptr p = *(pagesToSee.begin());
        btrie_collect_page_stats(p, &pagesToSee, f);
        pagesToSee.erase(block_xptr(p));
        pagesAlreadySeen.insert(p);
    }

    fprintf(f, "} \n");

    fclose(f);
}
