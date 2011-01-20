/*
* BTrie insert implementation
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "btrie_internal.h"
#include "btrie_readstate.h"


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

/** Write the state to the buffer and offset buffer as precalculated
  * @return the linear amount of octets, by which trie is increased
  */
int st_new_state_write(struct st_page_header * pghdr, struct st_tmp_trie * new_state, char * state, sptr_t state_len) {
    int offset = new_state->len - state_len;
    char * data_end = (char *) XADDR(pghdr->page) + pghdr->data_end;

    /* Splitted part (goes to another block). Block is selected beetween existent ones by choosing the least loaded */
    if (new_state->buf2 != NULL) {
        * (new_state->buf2ptr) = st_write_split_state(new_state);
    }

    WRITE_PAGE(pghdr->page);

    if (offset != 0 && ((state + state_len) != data_end)) {
        memmove_ABd(state + state_len, data_end, offset);
    }

    memcpy(state, new_state->buf, new_state->len);

    pghdr->data_end += offset;
    fix_trie_pointers(pghdr, state, offset);
    st_write_page_header(pghdr);
    st_new_state_free(new_state);

    return offset;
}


inline
static size_t  write_lj(char * p, xptr_t ** x)
{
    flags_t meta = STATE_LONG_JUMP;
    char * s = p;

    CAST_AND_WRITE(s, meta);
    *x = (xptr_t *) s;

    return (sizeof(meta) + sizeof(xptr_t));
}

inline
static size_t write_state(char * p, struct state_descriptor * d)
{
    flags_t meta;
    char * s = p;
    char * edges = d->edges;
    sptr_t * pointers = d->pointers;
    uint8_t old_edge_count = 0;

    meta = d->flags |=
      (d->edge_count > 0 ? STATE_HAS_EDGES : 0) |
      (d->prefix_len > 0 ? 0 : STATE_NO_PREFIX) |
      (d->object_len > 0 ? STATE_FINAL : STATE_NO_OBJECT) ;

    CAST_AND_WRITE(s, meta);

    if (d->prefix_len > 0) {
        uint8_t ps = (uint8_t) d->prefix_len;
        CAST_AND_WRITE(s, ps);
    }

    if (d->edge_count > 0) {
        uint8_t ps = (uint8_t) d->edge_count;
        old_edge_count = (uint8_t) (d->edge_len / (sizeof(char) + sizeof(sptr_t)));

        CAST_AND_WRITE(s, ps);
        d->edge_len = ps * (sizeof(char) + sizeof(sptr_t));
    }

    if (d->object_len > 0) {
        uint16_t ps = (uint16_t) d->object_len;
        CAST_AND_WRITE(s, ps);
    }

    memcpy(s, d->prefix, d->prefix_len);
    d->prefix = s;
    s += d->prefix_len;

    d->edges = s;
    s += d->edge_count * sizeof(char);
    d->pointers = (sptr_t *) s;
    s += d->edge_count * sizeof(sptr_t);

    if (edges != NULL) {
        memcpy(d->edges, edges, old_edge_count * sizeof(char));
        memcpy(d->pointers, pointers, old_edge_count * sizeof(sptr_t));
    }

    memcpy(s, d->object, d->object_len);
    d->object = s;
    s += d->object_len;

    d->len = (s - p);

    return d->len;
}


/** Add new edge to the given state (maintaining the sort order) */
inline
static void add_edge(struct state_descriptor * dsc, char key, sptr_t value)
{
    int i;
    i = bisearch_char(key, (char *) dsc->edges, dsc->edge_count - 1);
    if ((i < (dsc->edge_count - 1)) && (dsc->edges[i] == key)) { U_ASSERT(false); }
    memmove_ABd(dsc->edges + i, dsc->edges + dsc->edge_count - 1, 1);
    dsc->edges[i] = key;
    memmove_ABd(dsc->pointers + i, dsc->pointers + dsc->edge_count - 1, sizeof(sptr_t));
    dsc->pointers[i] = value;
}


/** State constructor */
inline
static struct state_descriptor * fill_state(
    struct state_descriptor * dsc,
    char * prefix,
    size_t prefix_len,
    char * object,
    size_t object_len,
    uint8_t edge_count,
    bool is_final,
    bool is_split)
{
    dsc->edge_count = edge_count;
    dsc->prefix = prefix;
    dsc->prefix_len = prefix_len;

    dsc->object = NULL;
    dsc->object_len = 0;
    dsc->flags = (is_final ? STATE_FINAL : 0) | (is_split ? STATE_SPLIT_POINT : 0);

    if (object != NULL) {
        dsc->object = object;
        dsc->object_len = object_len;
    } else {
        dsc->flags |= STATE_NO_OBJECT;
    }
    dsc->edges = NULL;

    return dsc;
}



char tmp_buffer[2*MAX_STATE_SIZE];
char tmp_buffer_2[MAX_STATE_SIZE];

struct st_tmp_trie * st_state_delete_prepare(char * state)
{
    struct state_descriptor state_dsc;
    struct st_tmp_trie * result = (struct st_tmp_trie *) malloc(sizeof(struct st_tmp_trie));
    char * buf =  tmp_buffer;

    read_state(state, &state_dsc);

    state_dsc.object_len = 0;
    state_dsc.object = NULL;
    state_dsc.flags &= ~STATE_FINAL;
    write_state(buf, &state_dsc);
    buf += state_dsc.len;

    result->buf = tmp_buffer;
    result->len = (buf - tmp_buffer);
    result->offset = 0;

    return result;
}

/** Break the given <state> into new ones (two or three). If state is null, create it.
  */
void st_new_state_prepare(struct st_tmp_trie * result, char * state, int prefix_pos, int key_pos,
        struct st_key_object_pair * key_object_pair,
        bool split_state  /* this means we are in pure header page, and newly created node must as far as possible be located in child block */
     )
{
    int key_len = key_object_pair->key_length - key_pos;
    int prefix_len;
    struct state_descriptor state_dsc;
    struct state_descriptor new_state;
    char * buf =  tmp_buffer;

    result->buf2 = NULL;

/* There are five cases in this function */

/* IMPORTANT: Old state must always be at the end of buffer! */

    if (state == NULL) {
/*  - case 1 : there are no states yet on given page, so create new one */
        fill_state(&new_state, key_object_pair->key, key_object_pair->key_length,
            key_object_pair->object, key_object_pair->object_size, 0, key_object_pair->is_final, false);
        write_state(buf, &new_state);
        buf += new_state.len;
    }  else {
        struct state_descriptor key_state;
        int offset;

        read_state(state, &state_dsc);
        prefix_len = state_dsc.prefix_len - prefix_pos;

        if (prefix_len > 0) {
            /* In the following two cases we need a new parent state for a given one to be created */
            struct state_descriptor par_state;
            if (key_len > 0) {
                DEBUG_INFO("case 5 (P=(K-E) split)");

/*  - case 5 : (the most common one) the goal key and the prefix share common part, but there are parts of both left
               create new state as the parent of the given one, containing the common part of prefix
               create new state as the sibling to the given one */

                /* Here goes the parent state with strictly two children and no object */
                fill_state(&par_state, state_dsc.prefix, prefix_pos, NULL, 0, 2, false, split_state);
                write_state(buf, &par_state);
                buf += par_state.len;

                par_state.edges[0] = key_object_pair->key[key_pos];
                par_state.pointers[0] = 0;

                /* The goal FINAL state with no outgoing edges */
                fill_state(&key_state, key_object_pair->key + key_pos + 1, key_len - 1,
                    key_object_pair->object, key_object_pair->object_size, 0, key_object_pair->is_final, false);

                if (split_state) {
                    result->buf2 = tmp_buffer_2;
                    offset = write_lj(buf, &(result->buf2ptr));
                    write_state(result->buf2, &key_state);
                    result->len2 = key_state.len;
                    result->hint_edge = prefix_pos-1;
                } else {
                    write_state(buf, &key_state);
                    offset = key_state.len;
                }
                buf += offset;

                add_edge(&par_state, state_dsc.prefix[prefix_pos], (uint16_t) offset);
            } else {
                DEBUG_INFO("case 3 (PK-E split)");

/*  - case 3 : the goal key ends somewhere inside the prefix
               create new state as the parent of the given one */

                /* Here goes the parent state which is the goal one with one outgoing edge */
                fill_state(&par_state, state_dsc.prefix, prefix_pos,
                    key_object_pair->object, key_object_pair->object_size, 1, key_object_pair->is_final, split_state);
                write_state(buf, &par_state);
                buf += par_state.len;

                par_state.edges[0] = state_dsc.prefix[prefix_pos];
                par_state.pointers[0] = 0;
            }

            new_state = state_dsc;
            new_state.prefix = state_dsc.prefix + prefix_pos + 1;
            new_state.prefix_len = state_dsc.prefix_len - (prefix_pos + 1);
            write_state(buf, &new_state);
            buf += new_state.len;
        } else if (key_len > 0) {
            DEBUG_INFO("case 4  (K-E split)");

            char new_key;

/*  - case 4 : the goal key breaks at the end of the prefix, but there is a part of it left
               create new state as a child of the given one */

            new_state = state_dsc;
            new_state.edge_count++;
            write_state(buf, &new_state);
            buf += new_state.len;

            new_key = key_object_pair->key[key_pos];
            add_edge(&new_state, new_key, 0);

            fill_state(&key_state, key_object_pair->key + key_pos + 1, key_len - 1,
                key_object_pair->object, key_object_pair->object_size, 0, key_object_pair->is_final, false);

            if (split_state) {
                result->buf2 = tmp_buffer_2;
                offset = write_lj(buf, &(result->buf2ptr));
                write_state(result->buf2, &key_state);
                result->len2 = key_state.len;
            } else {
                write_state(buf, &key_state);
                offset = key_state.len;
            }
            buf += offset;

            for (int i = 0; i < new_state.edge_count; i++) {
                if (new_state.pointers[i] == NO_EDGE) { continue; }
                if (new_state.edges[i] != new_key) {
                    new_state.pointers[i] += offset;
                } else if (split_state) {
                    result->hint_edge = i-1;
                }
            }
        } else {
            DEBUG_INFO("case 2  (no split)");

/*  - case 2 : there is already a state (final or not), that can be considered as goal
               add (or replace) object to this one */

            new_state = state_dsc;

            if (key_object_pair->is_final) {
                new_state.flags |= STATE_FINAL;
                new_state.object = key_object_pair->object;
                if (key_object_pair->object != NULL) {
                    new_state.object_len = key_object_pair->object_size;
                    new_state.flags &= ~STATE_NO_OBJECT;
                } else {
                    new_state.object_len = 0;
                    new_state.flags |= STATE_NO_OBJECT;
                }
            }
            write_state(buf, &new_state);
            buf += new_state.len;
        }
    }

    result->buf = tmp_buffer;
    result->len = (buf - tmp_buffer);
    result->offset = 0;
}


/** Free state insertion info */
void st_new_state_free(struct st_tmp_trie * state)
{
}
