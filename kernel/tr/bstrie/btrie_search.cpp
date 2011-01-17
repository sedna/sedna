/*
* BTrie search
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "btrie_internal.h"
#include "btrie_readstate.h"

#define PAGE_GROW_DELTA 4
#define STATE_GROW_DELTA 16

static struct st_path * st_sp_create(struct st_path * sp)
{
    if (sp == NULL) {
        sp = (struct st_path *) malloc(sizeof(struct st_path));
    }

    sp->page_count = 0;
    sp->__page_cap = PAGE_GROW_DELTA;
    sp->pages = (struct st_static_page_data *) malloc(sizeof(struct st_static_page_data) * sp->__page_cap);

    sp->state_count = 0;
    sp->__state_cap = STATE_GROW_DELTA;
    sp->states = (struct st_static_state_data *) malloc(sizeof(struct st_static_state_data) * sp->__state_cap);

    sp->last_state = NULL;

    return sp;
}

static struct st_path * st_sp_add(struct st_path * sp, struct st_state_ext * state, struct st_page_header * ph)
{
    int i;
    struct st_static_state_data * static_state;

    if (sp->state_count == sp->__state_cap) {
        sp->__state_cap += STATE_GROW_DELTA;
        sp->states = (struct st_static_state_data *) realloc(sp->states, sizeof(struct st_static_state_data) * sp->__state_cap);
    }

    i = sp->state_count;
    sp->state_count++;
    static_state = sp->states + i;
    static_state->p = state->ptr;
//    static_state->edge_count = state->dsc.edge_count;
//    static_state->prefix = str_alloc_ncpy(state->dsc.prefix, state->dsc.prefix_len, malloc);
//    static_state->physical_size = st_get_local_state_length(XADDR(state->ptr), true);
//    static_state->long_edge_count = 0; // FIXME!!!

    if ((sp->page_count == 0) || (sp->pages[sp->page_count - 1].page != ph->page)) {
        struct st_static_page_data * static_page;

        static_state->is_root = true;

        if (sp->page_count == sp->__page_cap) {
            sp->__page_cap += PAGE_GROW_DELTA;
            sp->pages = (struct st_static_page_data *) realloc(sp->pages, sizeof(struct st_static_page_data) * sp->__page_cap);
        }

        static_page = sp->pages + sp->page_count;
        static_page->page = ph->page;
        static_page->free_space = ph->free_space;
        static_page->parent_state = (sp->state_count > 1) ? (static_state - 1) : NULL;
//        static_page->root_count = ph->trie_count;

        sp->page_count++;
    }

    static_state->page = sp->pages + (sp->page_count - 1);

    U_ASSERT(SAME_PAGE(static_state->p, static_state->page->page));

    return sp;
}

static struct st_path * st_sp_done(struct st_path * sp)
{
    sp->last_state = sp->states + (sp->state_count - 1);
    return sp;
}

void st_sp_free(struct st_path * state_path)
{
    free(state_path->states);
    free(state_path->pages);
    free(state_path);
}

inline
static xptr_t st_get_long_jump(char * p) {
    flags_t f;
    CAST_AND_READ(f, p);
    if ((f & STATE_LONG_JUMP) > 0) {
        xptr_t a;
        CAST_AND_READ(a, p);
        return a;
    } else  {
        return XNULL;
    }
}

/*
inline
static void btrie_push_stack(char s, struct state_descriptor * dsc, xptr_t p, int id, struct btrie_enum * cursor) {
    struct btrie_enum_stack_frame * stack_frame = cursor->stack + cursor->stack_len;

    stack_frame->p = p;
    stack_frame->trie_no = id;

    cursor->key[cursor->key_len] = s;
    memcpy(cursor->key + cursor->key_len + 1, dsc->prefix, dsc->prefix_len);
    cursor->key_len += dsc->prefix_len + 1;
    cursor->key[cursor->key_len] = '\0';

    stack_frame->key_len = cursor->key_len;

    cursor->stack_len++;
};

inline
static int btrie_pop_stack(struct btrie_enum * cursor) {
    cursor->stack_len--;

    if (cursor->stack_len == 0) {
        return -1;
    }

    struct btrie_enum_stack_frame * stack_frame = cursor->stack + cursor->stack_len - 1;

    cursor->key_len = stack_frame->key_len;
    cursor->key[cursor->key_len] = '\0';

    return stack_frame->trie_no;
}

bool btrie_next(struct btrie_enum * cursor) {
    state_descriptor dsc;
    int id;
    xptr_t p;

    do {
        struct btrie_enum_stack_frame * stack_frame = cursor->stack + cursor->stack_len - 1;
        p = stack_frame->p;
        id = ++(stack_frame->trie_no);
        READ_PAGE(p);
        read_state((char *) XADDR(p), &dsc);
        if (dsc.edge_count > id) {
            state_descriptor ndsc;
            char s = dsc.edges[id];
            int d = dsc.len + dsc.pointers[id];
            p += d;
            read_state((char *) XADDR(p), &ndsc);
            if ((ndsc.flags & STATE_LONG_JUMP) > 0) {
                p = st_remove_indirection(ndsc.long_jump);
                READ_PAGE(p);
                read_state((char *) XADDR(p), &ndsc);
            }
            btrie_push_stack(s, &ndsc, p, -1, cursor);

            if ((ndsc.flags & STATE_FINAL) > 0) {
                return true;
            }
        } else {
            btrie_pop_stack(cursor);
        }
    } while (cursor->stack_len > 0);

    return false;
}
*/

inline
static int find_state_edge(char k, struct state_descriptor * d, int * result) {
    if ((d->flags & STATE_HAS_EDGES) == 0) { return -1; }

    int i;

    U_ASSERT(d->edge_len > 0);
    U_ASSERT((d->flags & STATE_EDGE_TREE) == 0);

    i = bisearch_char(k, d->edges, d->edge_count);
    if ((i < 0) || (i >= d->edge_count) || (d->edges[i] != k)) { return -1; }

    *result = i;
    sptr_t a = d->pointers[i];
    if (a == NO_EDGE) { return -1; }
    return a;
}

static char * st_find_next_state(char * p, char ** k, size_t * klen, struct st_state_ext * state, int * prefix_break_position, int * move_index)
{
    char * s;
    char * prefix;
    int i = 0;
    size_t l;

    s = read_state(p, &(state->dsc));

    i = 0;
    l = MIN(state->dsc.prefix_len, *klen);
    prefix = state->dsc.prefix;

    while (i < (int) l) {
        if (*prefix != **k) { break; };
        prefix++; (*k)++; i++; (*klen)--;
    }

    *prefix_break_position = i;

    s = state->dsc.p + state->dsc.len;

    if ((i == state->dsc.prefix_len) && (klen > 0)) {
        int offset = find_state_edge(**k, &(state->dsc), move_index);
        if (offset == -1) {
            return NULL;
        } else {
            (*klen)--; (*k)++;
            return s + offset;
        }
    } else return NULL;
}


struct st_path * st_find_state_path(const struct btrie * tree, const char * key, size_t key_len)
{
    struct st_page_header page_header;
    struct st_path * result;
    struct st_state_ext current_state;
    char * next_state_ptr;
    xptr_t current_state_ptr;
    xptr_t current_page;
    xptr_t far_state_ptr;
    char * c = (char *) key;
    size_t clen = key_len;
    int prefix_break_position;

    current_page = tree->root_page;
    st_read_page_header(current_page, &page_header);

    if (page_header.trie_count == 0) {
        return NULL;
    }

    U_ASSERT(page_header.trie_count == 1);

    READ_PAGE(current_page);
    current_state_ptr = current_page + page_header.trie_offset + page_header.tries[0];
    result = st_sp_create(NULL);
    result->key_found = false;

    while (current_state_ptr != XNULL) {
        current_state.ptr = current_state_ptr;
        int next_edge_index = -1;
        next_state_ptr = st_find_next_state((char *) XADDR(current_state_ptr), &c, &clen, &current_state, &prefix_break_position, &next_edge_index);
        st_sp_add(result, &current_state, &page_header);
        result->states[result->state_count-1].from_here = next_edge_index;

        if (next_state_ptr == NULL) {
            result->prefix_break_position = prefix_break_position;
            result->key_break_position = c - key;
            result->key_found =
              (prefix_break_position == current_state.dsc.prefix_len) &&
              (clen == 0) && IS_FINAL_STATE(current_state);
//            result->
            break;
        }

        if ((far_state_ptr = st_get_long_jump(next_state_ptr)) == XNULL) {
            current_state_ptr = current_page + (next_state_ptr - (char *) XADDR(current_page));
        } else {
            current_page = GET_PAGE_ADDRESS(far_state_ptr);
            st_read_page_header(current_page, &page_header);
            current_state_ptr = st_remove_indirection(far_state_ptr);
        }
    };

    st_sp_done(result);
    return result;
}



