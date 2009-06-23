#include "st_trie.h"
#include "st_misc.h"

struct state_descriptor * read_state(void * start, struct state_descriptor * state)
{
    char * p = (char *) start;
    uint8_t meta;

    state->p = p;
    memcpy(&meta, p, sizeof(uint8_t));
    state->flags = meta;

    if ((meta & STATE_LONG_JUMP) > 0) {
        state->long_jump = XNULL;
        return state;
    };

    p += sizeof(uint8_t);

    if ((meta & STATE_LONG_PREFIX) > 0) {
        U_ASSERT(false); // TODO: NOT IMPLEMENTED
    } else {
        shift_t prefix_size;
        CAST_AND_READ(prefix_size, p);
        state->prefix_len = prefix_size;
        state->prefix = p;
        p += state->prefix_len;
    }

    if ((meta & STATE_HAS_OUTGOING_EDGES) > 0) {
        uint8_t edge_count;
        CAST_AND_READ(edge_count, p);
        state->edge_count = edge_count;
        state->edges = p;
        p += sizeof(char) * edge_count;
        state->pointers = (shift_t *) p;
        p += sizeof(shift_t) * edge_count;
    } else {
        state->edge_count = 0;
        state->edges = NULL;
        state->pointers = NULL;
    }

    if ((meta & STATE_FINAL) > 0) {
        // it is final and has object
        if ((meta & STATE_LARGE_OBJECT) > 0) {
            uint32_t obj_size;
            CAST_AND_READ(obj_size, p);
            state->object_len = obj_size;
            p += sizeof(xptr);
            state->object = p;
        } else {
            shift_t obj_size;
            CAST_AND_READ(obj_size, p);
            state->object_len = obj_size;
            state->object = p;
            p += obj_size;
        }
    } else {
        state->object_len = 0;
        state->object = NULL;
    }

    state->len = p - (char *) start;

    return state;
}

inline static void * fix_state_pointers(void * start, void * offset, int16_t move_offset)
{
    struct state_descriptor dsc;
    char * p;

    read_state(start, &dsc);

    if ((dsc.flags & STATE_LONG_JUMP) > 0) {
        return ((char *) start + 9);
    };

    p = dsc.p + dsc.len;

    if (dsc.edges != NULL) {
        for (int i = 0; i < dsc.edge_count; i++) {
            dsc.pointers[i] += (p + dsc.pointers[i] >= (char *) offset ? move_offset : 0);
        }
    }

    return p;
}

static void fix_all_pointers(void * p, void * offset, int16_t move_offset, void * end)
{
    while (p < end) {
        p = fix_state_pointers(p, offset, move_offset);
    }
}

int bisearch(char k, char * A, int n, bool * found)
{
    int l = 0;
    int r = n - 1;
    int m;

    *found = false;

    if (n == 0) {
        return 0;
    }

    while (l <= r) {
        m = (l + r) >> 1;

        if (A[m] == k) {
            *found = true;
            return m;
        }

        if (k > A[m]) {
            l = m+1;
        } else {
            r = m-1;
        }
    }

    return l;
}

void * state_image_find_prefix(char k, struct tree_state * state)
{
    bool found;
    int r;

    state->edge_index = r = bisearch(k, state->dsc.edges, state->dsc.edge_count, &found);

    return found ? ((char *) state->end_ptr + state->dsc.pointers[r]) : NULL;
}

size_t get_space_needed(struct tree_state * state, struct st_key_object_pair * value)
{
    return
      ((value->key_length == 0) ? 0 : /* new state */ sizeof(uint8_t) + sizeof(shift_t) + value->key_length + /* one more edge */ sizeof(char) + sizeof(shift_t) )
        + sizeof(shift_t) + value->object_size /* object size */
        + (!state->prefix_differs ? 0 : sizeof(uint8_t) + sizeof(char) + sizeof(shift_t) + sizeof(uint8_t) + sizeof(shift_t));
}

size_t get_local_state_length(void * start, bool subtree)
{
    struct state_descriptor dsc;
    char * p;
    size_t size;

    read_state(start, &dsc);

    if ((dsc.flags & STATE_LONG_JUMP) > 0) {
        return 9;
    };

    p = dsc.p + dsc.len;

    size = dsc.len;

    if (subtree && dsc.edges != NULL) {
        for (int i = 0; i < dsc.edge_count; i++) {
            size += get_local_state_length(p + dsc.pointers[i], true);
        }
    }

    return size;
}

struct trie_data * create_initial_state(struct trie_data * data, struct st_key_object_pair * value)
{
    char * p = data->p;

    * (uint8_t *) p = STATE_FINAL; p += sizeof(uint8_t);
    * (shift_t *) p = value->key_length; p += sizeof(shift_t);
    memcpy(p, value->key, value->key_length); p += value->key_length;
    * (shift_t *) p = value->object_size; p += sizeof(shift_t);
    memcpy(p, value->object, value->object_size); p += value->object_size;

    data->len = p - ((char *) data->p);

    return data;
}

struct trie_data * trie_delete_object(struct trie_data * data, struct tree_state * state)
{
    int delta;
    char * split_point;
    shift_t obj_size;

/*
    if ((state->dsc.flags & STATE_HAS_OUTGOING_EDGES) == 0) {
        CHECKP(state->ptr);
        split_point = state->dsc.p + data->len;
        delta = - data->len;
        memmove_ABd(state->dsc.p + data->len, data->p + data->len,);
        data->len += delta;

        state->dsc.
    } else {
*/

    CHECKP(state->ptr);
    VMM_SIGNAL_MODIFICATION(state->ptr);
    obj_size = (sizeof(shift_t) + state->dsc.object_len);
    (* (uint8_t *) state->dsc.p) &= (uint8_t) (~STATE_FINAL);
    split_point = state->dsc.object + state->dsc.object_len;
    delta = - obj_size;
    memmove_ABd(split_point, data->p + data->len, delta);
    data->len += delta;

    fix_all_pointers(data->p, split_point, delta, state->dsc.p);

    return data;
//    }
}


struct trie_data * split_state(struct trie_data * data, struct tree_state * state, struct st_key_object_pair * value)
{
    void * new_state, * split_state, * split_point, * state_ptr;
    size_t object_part_len = sizeof(shift_t) + value->object_size;
    size_t edge_len = (sizeof(char) + sizeof(shift_t));
    size_t new_state_len;
    size_t delta;
    char * p;
    char * key = value->key;

    if (value->key_length != 0) {
        new_state_len = sizeof(uint8_t) + sizeof(shift_t) + value->key_length + object_part_len;
    } else {
        new_state_len = 0;
    }

    // TODO: long objects ang long prefixes

    CHECKP(state->ptr);
    state_ptr = XADDR(state->ptr);

    if (state->prefix_differs) {
        char edge_key_a, edge_key_b;
        shift_t edge_shift_a, edge_shift_b;

        split_point = (char *) state_ptr + sizeof(uint8_t) + sizeof(shift_t) + state->prefix_break_index;
        new_state = (char *) split_point + 2 * edge_len + sizeof(uint8_t);

        delta = sizeof(uint8_t) /* edge count */
               + (new_state_len == 0 ? object_part_len + edge_len  : new_state_len + 2 * edge_len) /* new state/object + edges */
               + sizeof(shift_t) /* prefix len */
               + sizeof(uint8_t) /* meta */;

        split_state = (char *) split_point + delta - sizeof(uint8_t) - sizeof(shift_t);

        memmove_ABd(split_point, data->p + data->len, delta);
        data->len += delta;

        p = (char *) split_state;
        * (uint8_t *) p = * (uint8_t *) state_ptr; p += sizeof(uint8_t);
        * (shift_t *) p = state->dsc.prefix_len - state->prefix_break_index; p += sizeof(shift_t);

        edge_key_a = state->dsc.prefix[state->prefix_break_index];
        edge_shift_a = new_state_len;

        if (new_state_len != 0) {
            edge_key_b = *key;
            edge_shift_b = 0;

            U_ASSERT(edge_key_a != edge_key_b);

            if (edge_key_a > edge_key_b) {
                char xk = edge_key_b; shift_t xs = edge_shift_b;
                edge_key_b = edge_key_a; edge_shift_b = edge_shift_a;
                edge_key_a = xk; edge_shift_a = xs;
            }
        }

        p = (char *) state_ptr;
        * (uint8_t *) p = STATE_HAS_OUTGOING_EDGES | ((new_state_len != 0) ?  0 : STATE_FINAL); p += sizeof(uint8_t);
        * (shift_t *) p = state->prefix_break_index; p += sizeof(shift_t);
        p += state->prefix_break_index;
        * (uint8_t *) p = ((new_state_len != 0) ? 2 : 1);
        p += sizeof(uint8_t);
        * (char *) p = edge_key_a; p += sizeof(char);

        if (new_state_len != 0) {
            * (char *) p = edge_key_b; p += sizeof(char);
            * (shift_t *) p = edge_shift_a; p += sizeof(shift_t);
            * (shift_t *) p = edge_shift_b; p += sizeof(shift_t);
        } else {
            * (shift_t *) p = edge_shift_a; p += sizeof(shift_t);
            * (shift_t *) p = value->object_size; p += sizeof(shift_t);
            memcpy(p, value->object, value->object_size); p += value->object_size;
        }
    } else if (value->key_length != 0) {
        if (state->dsc.edge_count == 0) {
            new_state = state->end_ptr + sizeof(char) + sizeof(shift_t) + sizeof(uint8_t);
            delta = sizeof(char) + sizeof(shift_t) + new_state_len + sizeof(uint8_t);
        } else {
            new_state = state->end_ptr + sizeof(char) + sizeof(shift_t);
            delta = sizeof(char) + sizeof(shift_t) + new_state_len;
        }

        split_point = state->end_ptr;
        memmove_ABd(split_point, data->p + data->len, delta);
        data->len += delta;

        p = (char *) state_ptr;
        * (uint8_t *) p |= STATE_HAS_OUTGOING_EDGES; p += sizeof(uint8_t);
        p += sizeof(shift_t);
        p += state->dsc.prefix_len;

        if (state->dsc.edge_count == 0) {
            memmove_ABd(p, state->end_ptr, sizeof(shift_t) + sizeof(char) + sizeof(uint8_t));

            * (uint8_t *) p = 1; p += sizeof(uint8_t);
            * (char *) p = *key; p += sizeof(char);
            * (shift_t *) p = 0; p += sizeof(shift_t);
        } else {
            char * image = state->dsc.edges;
            shift_t * shifts = (shift_t *) state->dsc.pointers;
            int i;

            for (int i = 0; i < state->dsc.edge_count; i++) {
                shifts[i] += new_state_len;
            }

            * (uint8_t *) p += 1;

            state_image_find_prefix(*key, state);
            i = state->edge_index;

            memmove_ABd(shifts + i, state->end_ptr, sizeof(shift_t) + sizeof(char));
            memmove_ABd(image + i, shifts + i, sizeof(char));
            shifts = (shift_t *) ( (char *) shifts + sizeof(char) );
            image[i] = *key;
            shifts[i] = 0;
        }
    } else {
        U_ASSERT(!state->final);

        new_state_len = 0;
        delta = sizeof(shift_t) + value->object_size;
        p = (char *) state_ptr;
        * (uint8_t *) p |= STATE_FINAL; p += sizeof(uint8_t);

        p = (char *) (split_point = state->end_ptr);
        memmove_ABd(split_point, data->p + data->len, delta);
        data->len += delta;

        * (shift_t *) p = value->object_size; p += sizeof(shift_t);
        memcpy(p, value->object, value->object_size); p += value->object_size;
    }

    if (new_state_len != 0) {
        p = (char *) new_state;

        * (uint8_t *) p = STATE_FINAL; p += sizeof(uint8_t);
        * (shift_t *) p = value->key_length; p += sizeof(shift_t);
        memcpy((char *) p, key, value->key_length); p += value->key_length;

        * (shift_t *) p = value->object_size; p += sizeof(shift_t);
        memcpy(p, value->object, value->object_size); p += value->object_size;
    }

    // TODO: fix pointers only if there are some moved states
    fix_all_pointers(data->p, split_point, delta, state_ptr);

    return data;
}


