#include "st_unify.h"
#include "st_trie.h"
#include "st_pages.h"

#include "block_string_tree.h"

#define MAX_PAGE_STACK 32

int st_last_error;

st_t st_init()
{
//    struct str_tree * t = (struct str_tree *) malloc(sizeof(struct str_tree));
//    t->root_page = XNULL;
    return XNULL;
};

xptr st_find_string(const st_t tree, const char * key)
{
    return st_find_binary(tree, key, strlen(key));
}

xptr st_find_binary(const st_t tree, const char * key, size_t key_length)
{
    struct tree_state_path * states;
    xptr page_stack[MAX_PAGE_STACK];
    xptr result = XNULL;

    page_stack[0] = XNULL;
    states = find_state_path(tree, key, key_length, page_stack);

    if (states == NULL) { return XNULL; }

    if ((states->last_state != NULL) && (states->last_state->final)) {
        st_last_error = ST_ERROR_NO_ERROR;
        result = states->last_state->ptr;
        free_state_path(states);
        return result;
    } else {
        st_last_error = ST_ERROR_NOT_FOUND;
        free_state_path(states);
        return XNULL;
    }
}

st_t st_insert_string(st_t tree, const char * key, const char * obj, size_t obj_length, bool replace)
{
    return st_insert_binary(tree, key, strlen(key), obj, obj_length, replace);
}

st_t st_insert_binary(st_t tree, const char * key, size_t key_length, const char * obj, size_t obj_length, bool replace)
{
    struct tree_state_path * states;
    struct tree_state * state;
    size_t space_needed;
    struct st_page_header page_header;
    xptr page_stack[MAX_PAGE_STACK], * page_v;
    page_stack[0] = XNULL;
    struct st_key_object_pair key_object_pair = {(char *) key, key_length, (char *) obj, obj_length};

    if (tree == XNULL) {
        return make_first_state(tree, &key_object_pair);
    }

    do {
        states = find_state_path(tree, key, key_length, page_stack);

        key_object_pair.key = (char *) key + states->key_break;
        key_object_pair.key_length = key_length - states->key_break;

        page_v = page_stack + 1;
        while (*page_v != XNULL) page_v++;
        page_v--;

        if (states == NULL) { return tree; }

        if (states->last_state != NULL) {
            st_last_error = ST_ERROR_KEY_EXISTS;
            free_state_path(states);
            return tree;
        };

        state = states->states[states->state_count - 1];

        space_needed = get_space_needed(state, &key_object_pair);

        read_page_header(*page_v, &page_header);
//        printf("\nspace left : %d\n", page_header.free_space);
        if (page_header.free_space < space_needed) {
            page_split(page_v, space_needed, CALC_PAGE_SHIFT(XADDR(state->ptr), *page_v), &page_header);
            continue;
        } else {
            break;
        }
    } while (false);

    VMM_SIGNAL_MODIFICATION(page_header.page);
    split_state(&(page_header.trie), state, &key_object_pair);
    save_page_header(&page_header);
    free_state_path(states);
    return tree;
}

st_t st_delete_string(st_t tree, const char * key)
{
    return st_delete_binary(tree, key, strlen(key));
}

st_t st_delete_binary(st_t tree, const char * key, size_t key_length)
{
    struct tree_state_path * states;
    struct st_page_header page_header;
    xptr page_stack[MAX_PAGE_STACK];
    page_stack[0] = XNULL;

    if (tree == XNULL) {
        return tree;
    }

    states = find_state_path(tree, key, key_length, page_stack);

    if (states->last_state == NULL) {
        st_last_error = ST_ERROR_NOT_FOUND;
        free_state_path(states);
        return tree;
    }

    read_page_header(BLOCKXPTR(states->last_state->ptr), &page_header);
    VMM_SIGNAL_MODIFICATION(page_header.page);
    trie_delete_object(&(page_header.trie), states->last_state);
    save_page_header(&page_header);
    free_state_path(states);
    return tree;
}

//st_t st_delete_string(st_t tree, const char * key);
//st_t st_delete_binary(st_t tree, const char * key, size_t key_length);

//size_t st_get_object(const xptr p, char * object);
//st_object_t st_serialize_object(const xptr p);
//void st_free_object(st_object_t p);


typedef struct st_enum_position_ptr {
    xptr p;
    int i;
    int po;
} pptr;

const pptr NULLPPTR = {XNULL};

struct st_enum {
    pptr * stack_state;
    int stack_state_length;
    int stack_state_capacity;

    struct state_descriptor result_state;

    char * prefix;
    char * saved_prefix;
    size_t prefix_len;
    size_t saved_prefix_len;
    size_t prefix_size;

    xptr cptr;
};

struct st_enum * st_push_xptr(struct st_enum * en, xptr p, int i, int po)
{
    if (en->stack_state_length == en->stack_state_capacity) {
        en->stack_state_capacity += 16;
        en->stack_state = (pptr *) realloc(en->stack_state, en->stack_state_capacity * sizeof(pptr));
    }

    en->stack_state[en->stack_state_length].p = p;
    en->stack_state[en->stack_state_length].i = i;
    en->stack_state[en->stack_state_length].po = po;
    en->stack_state_length++;

    return en;
}

pptr st_pop_xptr(struct st_enum * en)
{
    if (en->stack_state_length == 0) { return NULLPPTR; }
    en->stack_state_length--;
    return en->stack_state[en->stack_state_length];
}

pptr st_pick_xptr(struct st_enum * en)
{
    if (en->stack_state_length == 0) { return NULLPPTR; }
    return en->stack_state[en->stack_state_length - 1];
}

void append_prefix(st_enumeration en, char * prefix, size_t len) {
    if (en->prefix_size <= (en->prefix_len + len)) {
        en->prefix_size += 256;
        en->prefix = (char *) realloc(en->prefix, en->prefix_size);
        en->saved_prefix = (char *) realloc(en->saved_prefix, en->prefix_size);
    }

    memcpy(en->prefix + en->prefix_len, prefix, len);
    en->prefix_len += len;
}

st_enumeration st_enum_start(xptr first_state)
{
    struct state_descriptor dsc;
    struct st_enum * en;

    en = (struct st_enum *) calloc(1, sizeof(struct st_enum));
    en->saved_prefix = (char *) malloc(256);
    en->prefix = (char *) malloc(256);
    en->prefix_len = en->saved_prefix_len = 0;
    en->prefix_size = 256;

    en->stack_state_capacity = 16;
    en->stack_state = (pptr *) malloc(en->stack_state_capacity * sizeof(pptr));
    en->stack_state_length = 0;

    CHECKP(first_state);
    read_state(XADDR(first_state), &dsc); // Check if all is ok!

    st_push_xptr(en, first_state, 0, 0);
    append_prefix(en, dsc.prefix, dsc.prefix_len);

    if ((dsc.flags & STATE_FINAL) == 0) {
        st_enum_next(en);
    }

    return en;
}

xptr get_long_jump(xptr p)
{
    return XNULL;
//    return (shift
}

bool st_enum_next(st_enumeration en)
{
    struct state_descriptor dsc;
    pptr pstate;
    xptr state;
    int i;

    pstate = st_pick_xptr(en);

    if (pstate.p == XNULL) { return false; }

    state = pstate.p;
    CHECKP(state);
    read_state(XADDR(state), &dsc); // Check if all is ok!
    if ((dsc.flags & STATE_FINAL) > 0) {
        en->result_state = dsc;
        en->cptr = state;
        memcpy(en->saved_prefix, en->prefix, en->prefix_len);
        en->saved_prefix_len = en->prefix_len;
    } else {
        en->result_state.p = NULL;
    }

    while (true) {
        if ((dsc.flags & STATE_HAS_OUTGOING_EDGES) > 0) {
            i = 0;
        } else {
            do {
                pstate = st_pop_xptr(en);

                en->prefix_len = pstate.po;

                i = pstate.i+1;
                pstate = st_pick_xptr(en);
                if (pstate.p == XNULL) { return true; }
                state = pstate.p;
                CHECKP(state);
                read_state(XADDR(state), &dsc);
                U_ASSERT((dsc.flags & STATE_LONG_JUMP) == 0);
            } while (i >= dsc.edge_count);
        }

        state = state + dsc.len + dsc.pointers[i];

        while (true) {
            CHECKP(state);
            read_state(XADDR(state), &dsc);
            if ((dsc.flags & STATE_LONG_JUMP) > 0) {
                state = get_long_jump(dsc.long_jump);
            } else break;
        }

        st_push_xptr(en, state, i, en->prefix_len);
        append_prefix(en, dsc.prefix, dsc.prefix_len);

        if ((dsc.flags & STATE_FINAL) > 0) {
            return true;
        }
    }
}

bool st_enum_free(st_enumeration en)
{
    return true;
}

size_t st_enum_get_key(st_enumeration en, char * key)
{
    if (key != NULL) {
        memcpy(key, en->saved_prefix, en->saved_prefix_len);
    }

    return en->saved_prefix_len;
}

xptr st_enum_get_object(st_enumeration en)
{
    return en->cptr;
}


size_t st_get_object(const xptr p, char * object)
{
    struct state_descriptor dsc;

    CHECKP(p);
    read_state(XADDR(p), &dsc);
    U_ASSERT((dsc.flags & STATE_FINAL) > 0);

    if (object != NULL) {
        memcpy(object, dsc.object, dsc.object_len);
    }

    return dsc.object_len;
}

xptr get_initial_state_ptr(const st_t tree)
{
    return tree + 22;
}

