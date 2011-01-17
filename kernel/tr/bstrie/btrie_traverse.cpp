/*
* BTrie traverse
* This library works as the part of Sedna project
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "btrie_traverse.h"
#include "btrie_readstate.h"

static
void bt_enum_append_key(btrie_enum_t benum, const char * key, size_t len) {
    size_t keylen = benum->key_len + len + 1;

    /* Key capacity must always be at least one character longer then key len */
    if (keylen >= benum->key_cap) {
        benum->key_cap += PAGE_SIZE;
        U_ASSERT(keylen <= benum->key_cap);
        benum->key = (char *) realloc(benum->key, benum->key_cap);
    }

    memcpy(benum->key + benum->key_len, key, len);
    benum->key_len += len;
    benum->key[benum->key_len] = '\0';
}



inline static
struct btrie_enum_stack_frame *
bt_enum_push(btrie_enum_t benum) {
    ++benum->stack_len;

    if (benum->stack_len > benum->stack_cap) {
        benum->stack_cap += 64;
        benum->stack = (struct btrie_enum_stack_frame *) realloc(benum->stack, benum->stack_cap * sizeof(struct btrie_enum_stack_frame));
    }

    return benum->stack + (benum->stack_len - 1);
}

bool btrie_enum_next(btrie_enum_t benum) {
    struct btrie_enum_stack_frame * top = NULL;
    struct state_descriptor state;

    do {
        /* Set pointer to next node in tree */
        while (benum->stack_len > 0) {
            top = benum->stack + (benum->stack_len - 1);
            U_ASSERT(top->current_edge < top->edge_count);
            ++top->current_edge;
            if (top->current_edge == top->edge_count) {
                --benum->stack_len;
                benum->key_len -= top->key_len;
            } else {
                break;
            }
        }

        if (benum->stack_len == 0) {
            return false;
        }

        /* Read state, where current pointer is set */
        xptr_t p = top->p;
        READ_PAGE(p);
        char * state_ptr = (char *) XADDR(p);
        read_state(state_ptr, &state);

        U_ASSERT((state.flags & STATE_EDGE_TREE) == 0);
        char interchar = state.edges[top->current_edge];
        sptr_t offset = state.len + state.pointers[top->current_edge];
        p += offset;
        state_ptr += offset;
        read_state(state_ptr, &state);

        if ((state.flags & STATE_LONG_JUMP) > 0) {
            p = st_remove_indirection(state.long_jump);
            READ_PAGE(p);
            state_ptr = (char *) XADDR(p);
            read_state(state_ptr, &state);
        }

        top = bt_enum_push(benum);
        top->p = p;
        top->edge_count = state.edge_count;
        top->current_edge = -1;
        benum->key[benum->key_len] = interchar;
        ++benum->key_len;
        top->key_len = state.prefix_len + 1;

        bt_enum_append_key(benum, state.prefix, state.prefix_len);
    } while (0 == (state.flags & STATE_FINAL));

    return true;
};

btrie_enum_t bt_enum_create(st_path* path)
{
    btrie_enum_t result = (btrie_enum_t) malloc(sizeof(btrie_enum));
    struct btrie_enum_stack_frame * top;
    struct state_descriptor state;

    U_ASSERT(path->state_count > 0);

    result->key_cap = 1024;
    result->key_len = 0;
    result->key = (char *) malloc(result->key_cap);

    result->stack_cap = path->state_count;
    result->stack_len = path->state_count;
    result->stack = (struct btrie_enum_stack_frame *) malloc(sizeof(struct btrie_enum_stack_frame) * result->stack_cap);
    top = result->stack;
    top->key_len = 0;

    for (int i = 0; i < path->state_count; ++i) {
        struct st_static_state_data * state_link = path->states + i;

        READ_PAGE(state_link->p);
        char * state_ptr = (char *) XADDR(state_link->p);
        read_state(state_ptr, &state);
        top->key_len += state.prefix_len;
        top->edge_count = state.edge_count;
        top->current_edge = state_link->from_here;
        top->p = state_link->p;

        bt_enum_append_key(result, state.prefix, state.prefix_len);

        ++top;
        if (state_link != path->last_state) {
            char interchar = state.edges[state_link->from_here];
            result->key[result->key_len] = interchar;
            ++result->key_len;
            result->key[result->key_len] = '\0';
            top->key_len = 1;
        } else {
            top[-1].current_edge = -1;
        }
    }

    if ((state.flags & STATE_FINAL) == 0) {
        btrie_enum_next(result);
    }

    return result;
}

void btrie_enum_close(btrie_enum_t benum)
{
    free(benum->key);
    free(benum->stack);
    free(benum);
}
