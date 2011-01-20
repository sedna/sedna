/*
* BTrie page split implementation
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "btrie_internal.h"
#include "btrie_readstate.h"
#include "btrie_utils.h"

#define TMP_BLOCK_SIZE PAGE_SIZE
#define MAX_LINKS 256


/** Returns the liniear size of the state at <p> */
inline
static sptr_t st_get_local_state_length(const char * p, bool include_subtree)
{
    size_t size;
    struct state_descriptor dsc;

    p = read_state(p, &dsc);
    size = dsc.len;

    if (((dsc.flags & STATE_LONG_JUMP) == 0) && include_subtree && (dsc.edge_count > 0)) {
        sptr_t max_ptr = dsc.pointers[0];
        for (int i = 0; i < dsc.edge_count; i++) {
            if (max_ptr < dsc.pointers[i]) { max_ptr = dsc.pointers[i]; }
        }
        size += max_ptr + st_get_local_state_length(p + max_ptr, true);
    }

    return size;
}

/** Moves the whole state <src> to <dest>  */
inline
static sptr_t st_move_state_tree(char * dest, const char * src, sptr_t len)
{
    size_t l = len > 0 ? len : st_get_local_state_length(src, true);
    memcpy(dest, src, l);
    return (sptr_t) l;
}


/** Copies tries, defined by <segments> from <source> buffer to page, defined by <page_header> */
inline
static void st_copy_tries(char * source, struct trie_segment_t * segments, struct st_page_header * page_header) {
    char * dest = (char *) XADDR(page_header->page) + page_header->trie_offset;
    char * root_links = (char *) page_header->tries;
    int n = page_header->trie_count;
    sptr_t pos = 0;

    WRITE_PAGE(page_header->page);

    for (int i = 0; i < n; i++) {
        struct trie_segment_t * trie = segments + i;

        memcpy(dest, source + trie->p, trie->len);
        CAST_AND_WRITE(root_links, pos);

        pos += trie->len;
        dest += trie->len;
    }

    page_header->data_end = page_header->trie_offset + pos;
}

static void write_jump_list(struct state_descriptor * dsc, struct trie_segment_t * segments, xptr_t base, int n0, int n)
{
    char * jumps = dsc->p + dsc->len;
    static const flags_t state = STATE_LONG_JUMP | STATE_NO_PREFIX;
    xptr_t jump;

    for (int i = n0; i < n; i++) {
        char * dest = jumps + dsc->pointers[segments[i].id];
        jump = base + st_trie_list_offset + (i - n0) * sizeof(sptr_t);
        CAST_AND_WRITE(dest, state);
        CAST_AND_WRITE(dest, jump);
    }
}

inline
static int divide_segments(struct trie_segment_t * segments, int n, sptr_t len)
{
    sptr_t threshold = len / 2;
    int total_length = 0;
    for (int i = 0; i < n; i++) {
        if (total_length + segments[i].len > threshold) {
            return i;
        }
        total_length += segments[i].len;
    }

    return 0;
}

inline
static void swap(struct trie_segment_t * a, struct trie_segment_t * b) {
    struct trie_segment_t x = *a;
    *a = *b;
    *b = x;
}

static int write_segments(struct trie_segment_t * segments, int n, sptr_t len, xptr_t * ltp, xptr_t * rtp, char * source)
{
    struct st_page_header new_page_header;
    int break_point;

    break_point = divide_segments(segments, n, len);
    /* FIXME: actually this can lead to an error, if break_point == 0. To fix it, segments MUST be sorted before division. */
//    U_ASSERT(break_point != 0);
    if (break_point == 0) {
        break_point = 1;
    }

    new_page_header.page = *ltp;
    st_markup_page(&new_page_header, break_point, *ltp == XNULL);
    st_copy_tries(source, segments, &new_page_header);
    st_write_page_header(&new_page_header);
    *ltp = new_page_header.page;

    st_markup_page(&new_page_header, n - break_point, true);
    st_copy_tries(source, segments + break_point, &new_page_header);
    st_write_page_header(&new_page_header);
    *rtp = new_page_header.page;

    return break_point;
}

static int build_segments(char * source, sptr_t * pointers, char * buffer, struct trie_segment_t * segments, int n)
{
    int total_length = 0;
    sptr_t len;
    char * c = buffer;

    for (int i = 0; i < n; i++) {
        sptr_t offset = pointers[i];
        char * state = source + offset;
        len = st_move_state_tree(c, state, 0);

        segments[i].p = (sptr_t) (c - buffer);
        total_length += (segments[i].len = len);
        c += len;
    }

    return total_length;
}

static char source[PAGE_SIZE];
static struct trie_segment_t tries[MAX_LINKS];

static xptr_t st_split_promote_root(struct st_page_header * root_page_hdr, int parent_free_space, xptr_t parent_position)
{
    struct state_descriptor dsc;
    xptr_t rtp = XNULL, ltp = XNULL;
    int total_length;
    int break_point;
    int n;
    int root_length;

    READ_PAGE(root_page_hdr->page);
    read_state(get_root_state(root_page_hdr), &dsc);
    n = dsc.edge_count;

    total_length = build_segments(dsc.p + dsc.len, dsc.pointers, source, tries, n);
    for (int i = 0; i < n; i++) {
        tries[i].id = i;
    }

    root_length = dsc.len + n * (sizeof(xptr_t) + sizeof(flags_t));

    if (root_length <= parent_free_space) {
        char * buf = (char *) malloc(root_length);
        struct st_tmp_trie trie = {buf, root_length};
        struct st_page_header * pghdr = root_page_hdr;

        memcpy(buf, dsc.p, dsc.len);

        ltp = root_page_hdr->page;

        st_read_page_header(GET_PAGE_ADDRESS(parent_position), pghdr);
        st_new_state_write(pghdr, &trie, (char *) XADDR(parent_position), sizeof(xptr_t) + sizeof(flags_t));

        READ_PAGE(parent_position);
        read_state((char *) XADDR(parent_position), &dsc);
    } else {
        parent_position = root_page_hdr->page;
        root_page_hdr->data_end = root_page_hdr->trie_offset + root_length;
    }

    break_point = write_segments(tries, n, total_length, &ltp, &rtp, source);

    WRITE_PAGE(root_page_hdr->page);

    (* (flags_t *) dsc.p) |= STATE_SPLIT_POINT;

    for (int i = 0; i < n; i++) {
        dsc.pointers[tries[i].id] = i * (sizeof(xptr_t) + sizeof(flags_t));
    }
    write_jump_list(&dsc, tries, ltp, 0, break_point);
    write_jump_list(&dsc, tries, rtp, break_point, n);
    st_write_page_header(root_page_hdr);

    return ltp;
}

static xptr_t st_split_tries(xptr_t parent_state, struct st_page_header * page_hdr)
{
    struct state_descriptor dsc;
    xptr_t rtp = XNULL, ltp = XNULL;
    int total_length;
    int break_point;
    int n;
    char * base;
    char * trie_array_base;

    READ_PAGE(page_hdr->page);
    n = page_hdr->trie_count;
    total_length = build_segments(get_root_state(page_hdr), page_hdr->tries, source, tries, n);

    READ_PAGE(parent_state);
    base = read_state((char *) XADDR(parent_state), &dsc);

    trie_array_base = (char *) XADDR(page_hdr->page) + page_hdr->trie_offset - page_hdr->trie_count * sizeof(sptr_t);

    for (int i = 0; i < dsc.edge_count; i++) {
        struct state_descriptor jdsc;
        read_state(base + dsc.pointers[i], &jdsc);
        if ((jdsc.long_jump != XNULL) && (GET_PAGE_ADDRESS(jdsc.long_jump) == page_hdr->page)) {
            tries[((char *) XADDR(jdsc.long_jump) - trie_array_base) / sizeof(sptr_t)].id = i;
        }
    }

    ltp = page_hdr->page;
    break_point = write_segments(tries, n, total_length, &ltp, &rtp, source);

    WRITE_PAGE(parent_state);
    write_jump_list(&dsc, tries, ltp, 0, break_point);
    write_jump_list(&dsc, tries, rtp, break_point, n);

    return ltp;
}


static
sptr_t find_parent_state(struct st_path * path, xptr_t page, xptr_t * parent_state)
{
    struct st_static_state_data * pp = NULL, * state = path->states;
    struct state_descriptor dsc;
    int count = path->state_count;

    *parent_state = XNULL;

    while (count > 0 && state->page->page != page) {
        pp = state;
        ++state;
        --count;
    }

    if (count > 0 && pp != NULL) {
        xptr_t parent_page = pp->p;
        READ_PAGE(pp->p);
        read_state((char *) XADDR(pp->p), &dsc);
        for (int i = 0; i < dsc.edge_count; i++) {
            char * state = dsc.p + dsc.len + dsc.pointers[i];
            xptr_t * x = (xptr_t *) (state + 1);

            if ((((*(flags_t *) state) & STATE_LONG_JUMP) > 0) && SAME_PAGE(page, *x)) {
                *parent_state = parent_page + (state - (char *) XADDR(parent_page));
                return pp->page->free_space;
            }
        }
    }

    return 0;
}

extern
void btrie_collect_stat(xptr entry_point);

#define MAX_CANDIDATES 16

static
xptr_t select_candidate(xptr_t * cl, int n, size_t state_len) {
    struct st_page_header ph;
    int minpfs = state_len;
    xptr_t result = XNULL;

    for (int i = 0; i < n; ++i) {
        if (cl[i] != XNULL) {
            st_read_page_header(cl[i], &ph);
            if (ph.free_space > minpfs) {
                result = cl[i];
                minpfs = ph.free_space;

                if (minpfs > PAGE_SIZE / 2) {
                    return result;
                }
            }
        }
    }

    return result;
}

#include <stdio.h>

xptr_t st_write_split_state(struct st_tmp_trie * new_state)
{
    struct state_descriptor dsc;
    struct st_page_header newpg;

    xptr_t parent_state = new_state->old_state;
    char * buf = new_state->buf2;
    size_t buflen = new_state->len2;
    int lower_edge = new_state->hint_edge;

    char * state = (char *) XADDR(parent_state);
    READ_PAGE(parent_state);
    state = read_state(state, &dsc);
    int n = dsc.edge_count;
    xptr_t * candidates = (xptr_t *) malloc(sizeof(xptr_t) * n);
    xptr_t c;
    xptr_t result;

    for (int i = 0; i < n; ++i) {
        struct state_descriptor tmp;
        read_state(state + dsc.pointers[i], &tmp);
        if ((tmp.flags & STATE_LONG_JUMP) > 0) {
            candidates[i] = tmp.long_jump;
        } else {
            candidates[i] = XNULL;
        }
    }

    c = select_candidate(candidates, n, buflen);

    free(candidates);

    if (c == XNULL) {
        U_ASSERT(false);
        st_markup_page(&newpg, 1, true);
        memcpy((char *) XADDR(newpg.page) + newpg.trie_offset, buf, buflen);
        newpg.data_end = newpg.trie_offset + buflen;
        st_write_page_header(&newpg);
    } else {
        c = GET_PAGE_ADDRESS(c);
        char * page_start = (char *) XADDR(c);
        st_read_page_header(c, &newpg);
        WRITE_PAGE(c);

        /* Add new trie to the end of the trie array */

        newpg.trie_count++;
        memmove_ABd(page_start + newpg.trie_offset, page_start + newpg.data_end, sizeof(sptr_t));
        newpg.tries[newpg.trie_count - 1] = newpg.data_end - newpg.trie_offset;
        newpg.data_end += sizeof(sptr_t);
        newpg.trie_offset += sizeof(sptr_t);
        memcpy(page_start + newpg.data_end, buf, buflen);
        newpg.data_end += buflen;
        st_write_page_header(&newpg);
    }

    result = newpg.page + newpg.trie_offset - sizeof(sptr_t);
    fprintf(__bt_debug, "st_write_split_state returns %16llx\n", result.to_logical_int());
    fflush(__bt_debug);
    return result;
}

xptr_t st_split(struct btrie tree, struct st_path * sp, int cpage)
{
    struct st_static_page_data * page = sp->pages + cpage;
    struct st_page_header page_hdr;
    xptr_t x;
    xptr_t parent;

    btrie_collect_stat(tree.root_page);

    st_read_page_header(page->page, &page_hdr);

    if (page_hdr.trie_count == 1) {
        xptr_t p;
        int free_space;

        free_space = find_parent_state(sp, page_hdr.page, &p);

        DEBUG_INFO("pagesplit case A.1 (promote root)");
        x = st_split_promote_root(&page_hdr, free_space, p);
        fprintf(__bt_debug, "pagesplit case A.1 (promote root)\n");
        fflush(__bt_debug);
    } else {
        DEBUG_INFO("pagesplit case B (split root)");
        parent = page->parent_state->p;
        x = st_split_tries(parent, &page_hdr);
        fprintf(__bt_debug, "pagesplit case B (split root)\n");
        fflush(__bt_debug);
    }

    btrie_collect_stat(tree.root_page);

    return x;
}



