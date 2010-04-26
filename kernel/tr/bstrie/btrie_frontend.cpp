/*
* BTrie frontend implementation
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/
#include "btrie_internal.h"
#include "btrie_readstate.h"

#include "btrie.h"

int btrie_last_error;

btrie_t btrie_open(const xptr_t root)
{
    btrie_t result = (btrie_t) malloc(sizeof(struct btrie));
    result->root_page = (xptr_t) root;
    return result;
}

xptr_t btrie_get_root(btrie_t bt)
{
    return bt->root_page;
}

void btrie_close(btrie_t bt)
{
    free(bt);
}

btrie_record_t btrie_find(const btrie_t tree, const char * key, size_t key_length)
{
    struct st_path * states;
    xptr_t result = XNULL;

    if (tree->root_page == XNULL) {
        btrie_last_error = ST_ERROR_NOT_FOUND;
        return XNULL;
    }

    states = st_find_state_path(tree, key, key_length);

    if (states == NULL) {
        btrie_last_error = ST_ERROR_NOT_FOUND;
        return XNULL;
    }

    if (states->key_found) {
        btrie_last_error = ST_ERROR_NO_ERROR;
        result = states->last_state->p;
    } else {
        btrie_last_error = ST_ERROR_NOT_FOUND;
    }

    st_sp_free(states);
    return result;
}

inline
static void fix_state_pointers(struct state_descriptor * state, const char * offset_point, int offset) {
    int maxi = -1;
    int maxv = -1;
    char * base = state->p + state->len;

    if (state->p == offset_point) {
        return;
    }

    for (int i = 0; i < state->edge_count; i++) {
        if (state->pointers[i] == NO_EDGE) { continue; }
        if (base + state->pointers[i] - offset_point <= 0) {
            if (maxv < state->pointers[i]) {
                maxi = i;
                maxv = state->pointers[i];
            }
        } else {
            state->pointers[i] += offset;
        }
    }

    if (maxi != -1) {
        read_state(base + maxv, state);
        fix_state_pointers(state, offset_point, offset);
    }
}


inline
static void fix_trie_pointers(struct st_page_header * pghdr, const char * offset_point, int offset) {
    struct state_descriptor state;
    int maxi = -1;
    int maxv = -1;
    char * p;

    for (int i = 0; i < pghdr->trie_count; i++) {
        char * p = (char *) XADDR(pghdr->page) + pghdr->trie_offset + pghdr->tries[i];
        read_state(p, &state);
        if (p - offset_point > 0) {
            pghdr->tries[i] += offset;
        } else {
            if (maxv < pghdr->tries[i]) {
                maxi = i;
                maxv = pghdr->tries[i];
            }
        }
    }

    if (maxi >= 0) {
        p = (char *) XADDR(pghdr->page) + pghdr->trie_offset + maxv;
        read_state(p, &state);
        fix_state_pointers(&state, offset_point, offset);
    }
}

#include <stdio.h>

btrie_record_t btrie_insert(btrie_t tree, const char * key, size_t key_length, const char * obj, size_t obj_length, bool replace)
{
    struct st_key_object_pair key_object_pair = {(char *) key, key_length, (char *) obj, obj_length, true};
    struct st_path * states;
    btrie_record_t result = XNULL;
    struct st_page_header pghdr;
    struct st_tmp_trie * state = NULL;

    btrie_last_error = ST_ERROR_NO_ERROR;

    if (tree->root_page == XNULL) {
        /* If there is no root page, the trie is empty, we
            should create new page and add the single given key there */

        st_markup_page(&pghdr, 1, true);
        tree->root_page = pghdr.page;
        state = st_new_state_prepare(NULL, 0, 0, &key_object_pair);
        st_new_state_write(state, (char *) XADDR(pghdr.page) + pghdr.trie_offset, state->len, 0);
        pghdr.data_end = pghdr.trie_offset + state->len;
        st_write_page_header(&pghdr);
        st_new_state_free(state);
    } else {
        states = st_find_state_path(tree, key, key_length);

        if (states == NULL) {
            return BTRNULL;
        } else if (states->last_state == NULL) {
            btrie_last_error = ST_ERROR_UNKNOWN;
        } else {
            sptr_t state_offset;
            sptr_t offset;
            struct state_descriptor dsc;
            xptr_t pg = states->last_state->page->page;

            st_read_page_header(pg, &pghdr);
            read_state((char *) XADDR(states->last_state->p), &dsc);
            state_offset = dsc.p - (char *) XADDR(pg);
            state = st_new_state_prepare(dsc.p, states->prefix_break_position, states->key_break_position, &key_object_pair);

            if (state->len >= states->last_state->page->free_space) {
                // FIXME : root page bug
                pg = st_split(*tree, states, states->page_count - 1, &state_offset);
            }

            WRITE_PAGE(pg);
            st_read_page_header(pg, &pghdr);
            offset = st_new_state_write(state, (char *) XADDR(pg) + state_offset, dsc.len, (char *) XADDR(pg) + pghdr.data_end);
            pghdr.data_end += offset;
            fix_trie_pointers(&pghdr, (char *) XADDR(pg) + state_offset, offset);
            st_write_page_header(&pghdr);
            st_new_state_free(state);
        }

        st_sp_free(states);
    }

    return result;
}


bool btrie_delete(btrie_t tree, const char * key, size_t key_length)
{
    bool result = false;
    struct st_path * states;

    if (tree->root_page == XNULL) {
        btrie_last_error = ST_ERROR_NOT_FOUND;
        return false;
    }

    states = st_find_state_path(tree, key, key_length);

    if (states == NULL) {
        btrie_last_error = ST_ERROR_NOT_FOUND;
        return false;
    }

    if (states->key_found) {
        sptr_t offset;
        struct state_descriptor dsc;
        struct st_tmp_trie * state;
        struct st_page_header pghdr;
        xptr_t pg = states->last_state->page->page;

        btrie_last_error = ST_ERROR_NO_ERROR;
        st_read_page_header(pg, &pghdr);
        read_state((char *) XADDR(states->last_state->p), &dsc);
        state = st_state_delete_prepare(dsc.p);
        WRITE_PAGE(pg);
        offset = st_new_state_write(state, dsc.p, dsc.len, (char *) XADDR(pg) + pghdr.data_end);
        pghdr.data_end += offset;
        fix_trie_pointers(&pghdr, dsc.p, offset);
        st_write_page_header(&pghdr);
        st_new_state_free(state);

        result = true;
    } else {
        btrie_last_error = ST_ERROR_NOT_FOUND;
    }

    st_sp_free(states);
    return result;
}

size_t btrie_get_object(const btrie_record_t p, char * object)
{
    struct state_descriptor dsc;

    READ_PAGE(p);
    read_state((char *) XADDR(p), &dsc);

    if (object != NULL) {
        memcpy(object, dsc.object, dsc.object_len);
    }

    return dsc.object_len;
}



#if EL_DEBUG == 1

static char tmp_buffer[1024];

void __debug_walkthrough_branch(char * buffer, char * prefix_append, char * state);

void __debug_walkthrough(btrie_t tree) {
    struct st_page_header pghdr;
    st_read_page_header(tree->root_page, &pghdr);
    __debug_walkthrough_branch(tmp_buffer, tmp_buffer, (char *) XADDR(pghdr.page) + pghdr.trie_offset);
    printf("\n");
}

#include <stdio.h>
#include "btrie_readstate.h"

static char prefixbuf[250];

void __debug_print_state(struct state_descriptor * dsc) {
    bool final = (dsc->flags & STATE_FINAL) > 0;
    printf("(");
    if ((dsc->flags & STATE_NO_PREFIX) == 0) {
        memcpy(prefixbuf, dsc->prefix, dsc->prefix_len);
        prefixbuf[dsc->prefix_len] = '\0';
        printf("%s", prefixbuf);
    }
    if (final) {
        printf("*");
    }
    if ((dsc->flags & STATE_LONG_JUMP) > 0) {
        printf("%llu", dsc->long_jump);
    }
}

void __debug_walkthrough_branch(char * buffer, char * prefix_append, char * state) {
    struct state_descriptor state_dsc;

    read_state(state, &state_dsc);

    if ((state_dsc.flags & STATE_LONG_JUMP) > 0) {
        struct st_page_header pghdr;
        sptr_t a;
        char * p;
        READ_PAGE(state_dsc.long_jump);
        st_read_page_header(state_dsc.long_jump, &pghdr);
        memcpy(&a, XADDR(state_dsc.long_jump), sizeof(sptr_t));
        p = (char*) XADDR(pghdr.page) + pghdr.trie_offset + a;
        printf("\n[\n");
        __debug_walkthrough_branch(buffer, prefix_append, p);
        printf("\n]\n");
    } else {
        if ((state_dsc.flags & STATE_NO_PREFIX) == 0) {
            memcpy(prefix_append, state_dsc.prefix, state_dsc.prefix_len);
            prefix_append += state_dsc.prefix_len;
            *prefix_append = '\0';
//            printf("%s", buffer);
        }

        __debug_print_state(&state_dsc);

        if ((state_dsc.flags & STATE_HAS_EDGES) > 0) {
            for (int i = 0; i < state_dsc.edge_count; i++) {
                * prefix_append = state_dsc.edges[i];
                printf(" %d%c", state_dsc.pointers[i], state_dsc.edges[i]);
                __debug_walkthrough_branch(buffer, prefix_append + 1, state_dsc.p + state_dsc.len + state_dsc.pointers[i]);
            }
        }
        printf(") ");
    }
}

#endif /* EL_DEBUG == 1 */

