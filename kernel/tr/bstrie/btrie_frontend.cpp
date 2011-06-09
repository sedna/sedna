/*
* BTrie frontend implementation
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "btrie.h"
#include "btrie_internal.h"
#include "btrie_readstate.h"
#include "btrie_traverse.h"
#include "btrie_utils.h"

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

void walkthrough_the_block (char * state, std::set<xptr_t> * pagesToFree, xptr_t p) {
    state_descriptor dsc;
    state = read_state(state, &dsc);

    if ((dsc.flags & STATE_LONG_JUMP) > 0) {
	xptr_t to = block_xptr(dsc.long_jump);
        READ_PAGE(to);
        READ_PAGE(p);
        pagesToFree->insert(to);
    } else {
      for (int i = 0; i < dsc.edge_count; ++i) {
            walkthrough_the_block(state + dsc.pointers[i], pagesToFree, p);
      }
    }
}

void btrie_drop(btrie_t bt)
{
    std::set<xptr_t> pagesToFree;
    xptr_t current_page;
    st_page_header page_header;

    pagesToFree.insert(block_xptr(btrie_get_root(bt)));
    while (!pagesToFree.empty()) {
      current_page = *(pagesToFree.begin());
      st_read_page_header(current_page, &page_header);
      for (int i = 0; i < page_header.trie_count; ++i) {
	walkthrough_the_block(get_root_state(&page_header) + page_header.tries[i], &pagesToFree, current_page);
      }
      vmm_delete_block(current_page);
      pagesToFree.erase(block_xptr(current_page));
    }
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

btrie_record_t btrie_insert(btrie_t tree, const char * key, size_t key_length, const char * obj, size_t obj_length, bool replace)
{
    struct st_key_object_pair key_object_pair = {(char *) key, key_length, (char *) obj, obj_length, true};
    struct st_path * states;
    btrie_record_t result = XNULL;
    struct st_page_header pghdr;
    struct st_tmp_trie newstate = {};

    btrie_last_error = ST_ERROR_NO_ERROR;

    if (tree->root_page == XNULL) {
        /* If there is no root page, the trie is empty, we
            should create new page and add the single given key there */

        st_markup_page(&pghdr, 1, true);
        tree->root_page = pghdr.page;
        st_new_state_prepare(&newstate, NULL, 0, 0, &key_object_pair, false);
        pghdr.data_end = pghdr.trie_offset;
        newstate.state = 0;
        newstate.state_len = 0;
        st_new_state_write(&pghdr, &newstate);
    } else {
        states = st_find_state_path(tree, key, key_length);

        if (states == NULL) {
            return BTRNULL;
        } else if (states->last_state == NULL) {
            btrie_last_error = ST_ERROR_UNKNOWN;
        } else {
            sptr_t state_offset;
            struct state_descriptor dsc;
            xptr_t pg = states->last_state->page->page;

            st_read_page_header(pg, &pghdr);
            read_state((char *) XADDR(states->last_state->p), &dsc);
            state_offset = dsc.p - (char *) XADDR(pg);

            st_new_state_prepare(&newstate, dsc.p, states->prefix_break_position, states->key_break_position, &key_object_pair, (dsc.flags & STATE_SPLIT_POINT) > 0);

            if (newstate.len >= states->last_state->page->free_space) {
                pg = st_split(*tree, states, states->page_count - 1);
                st_sp_free(states);
                return btrie_insert(tree, key, key_length, obj, obj_length, replace);
            } else {
                WRITE_PAGE(pg);
                newstate.old_state = states->last_state->p;
                st_read_page_header(pg, &pghdr);

                newstate.state = state_offset - pghdr.trie_offset;
                newstate.state_len = dsc.len;

                st_new_state_write(&pghdr, &newstate);
            }
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
        struct state_descriptor dsc;
        struct st_tmp_trie * state;
        struct st_page_header pghdr;
        xptr_t pg = states->last_state->page->page;

        btrie_last_error = ST_ERROR_NO_ERROR;
        st_read_page_header(pg, &pghdr);
        read_state((char *) XADDR(states->last_state->p), &dsc);
        state = st_state_delete_prepare(dsc.p);

        WRITE_PAGE(pg);

        state->state = dsc.p - get_root_state(&pghdr);
        state->state_len = dsc.len;

        st_new_state_write(&pghdr, state);

        result = true;
    } else {
        btrie_last_error = ST_ERROR_NOT_FOUND;
    }

    st_sp_free(states);
    return result;
}

size_t btrie_get_object(const btrie_record_t p, char * object, size_t object_size)
{
    struct state_descriptor dsc;

    READ_PAGE(p);
    read_state((char *) XADDR(p), &dsc);

    if (object != NULL && object_size >= dsc.object_len) {
        memcpy(object, dsc.object, dsc.object_len);
    }

    return dsc.object_len;
}

bool btrie_replace_object(const btrie_record_t p, const char * object)
{
    struct state_descriptor dsc;

    WRITE_PAGE(p);
    read_state((char *) XADDR(p), &dsc);
    memcpy(dsc.object, object, dsc.object_len);

    return dsc.object_len;
}


btrie_enum_t btrie_find_prefix(const btrie_t tree, const char * key, size_t key_length, bool * first_key_equal) {
    struct st_path * states;
    struct state_descriptor dsc;

    if (tree->root_page == XNULL) {
        btrie_last_error = ST_ERROR_NOT_FOUND;
        return NULL;
    }

    states = st_find_state_path(tree, key, key_length);

    if (states == NULL) {
        btrie_last_error = ST_ERROR_NOT_FOUND;
        return NULL;
    }

    if (first_key_equal != NULL) {
        *first_key_equal = states->key_found;
    }

    return bt_enum_create(states);
}

btrie_record_t btrie_get_state(btrie_enum_t cursor) {
    return cursor->stack[cursor->stack_len-1].p;
}

char * btrie_get_key(btrie_enum_t cursor) {
    return cursor->key;
}

size_t btrie_get_key_len(btrie_enum_t cursor) {
    return cursor->key_len;
}

bool btrie_is_EOT(btrie_enum_t cursor) {
    return cursor->eot;
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
