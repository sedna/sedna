#include "st_pages.h"
#include "st_misc.h"

#define ST_PAGE_HEADER_LEN (sizeof(((struct trie_data *) NULL)->len) + sizeof(((struct trie_data *) NULL)->root_count))

struct tree_state * state_find_key(char * p, char ** k, size_t * key_length, struct tree_state * state)
{
    state->ptr = ADDR2XPTR(p);

    while (true) {
        read_state(p, &(state->dsc));
        if ((state->dsc.flags & STATE_LONG_JUMP) > 0) {
            state->ptr = state->dsc.long_jump;
            CHECKP(state->ptr);
            p = (char *) XADDR(state->ptr);
        } else break;
    }

    int i, l;
    char * prefix;

    state->final = (state->dsc.flags & STATE_FINAL) > 0;

    i = 0;
    l = MIN(state->dsc.prefix_len, *key_length);

    prefix = state->dsc.prefix;

    while (i < l) {
        if (*prefix != **k) { break; };
        prefix++; (*k)++; i++; (*key_length)--;
    }

    state->prefix_differs = (i < state->dsc.prefix_len);
    if (state->prefix_differs) { state->prefix_break_index = i; }

    state->end_ptr = state->dsc.p + state->dsc.len;

    return state;
};

struct st_page_header * st_create_markup_page(struct st_page_header * new_page_header, int root_count, bool create)
{
    if (create) {
        vmm_alloc_data_block(&new_page_header->page);
        VMM_SIGNAL_MODIFICATION(new_page_header->page);
    }

    new_page_header->trie.root_count = root_count;
    new_page_header->root_offset = ST_PAGE_HEADER_OFFSET + ST_PAGE_HEADER_LEN;
    new_page_header->trie.roots = (shift_t *) ((char *) XADDR(new_page_header->page) + new_page_header->root_offset);

    new_page_header->trie.len = 0;
    new_page_header->trie_offset = new_page_header->root_offset + root_count * sizeof(shift_t);
    new_page_header->trie.p = (char *) XADDR(new_page_header->page) + new_page_header->trie_offset;

    new_page_header->free_space = PAGE_SIZE - new_page_header->trie.len - new_page_header->trie_offset;

    return new_page_header;
}

struct st_page_header * st_add_root(struct st_page_header * header, unsigned int root_count)
{
/*
    new_page_header->trie.root_count += root_count;
    new_page_header->root_offset = ST_PAGE_HEADER_OFFSET + ST_PAGE_HEADER_LEN;
    new_page_header->trie.roots = (char *) XADDR(new_page_header->page) + new_page_header->root_offset;

    new_page_header->trie_len = 0;
    new_page_header->trie_offset = new_page_header->root_offset + root_count * sizeof(shift_t);
    new_page_header->trie = (char *) XADDR(new_page_header->page) + new_page_header->trie_offset;

    memmove_ABd()

    new_page_header->free_space = PAGE_SIZE - new_page_header->trie_len - new_page_header->trie_offset;
*/

    U_ASSERT(false); // unimplemented

    return header;
}

struct st_page_header * read_page_header(xptr page, struct st_page_header * header)
{
    char * p;

    CHECKP(page);
    p = (char *) XADDR(page);

    header->page = page;

    p += ST_PAGE_HEADER_OFFSET;
    CAST_AND_READ(header->trie.root_count, p);
    CAST_AND_READ(header->trie.len, p);

    header->root_offset = p - (char *) XADDR(page);
    header->trie.roots = (shift_t *) p;
    p += header->trie.root_count * sizeof(shift_t);
    header->trie_offset = p - (char *) XADDR(page);
    header->trie.p = p;

    header->free_space = PAGE_SIZE - header->trie.len - header->trie_offset;

    return header;
}

struct st_page_header * save_page_header(struct st_page_header * header)
{
    char * p;

    CHECKP(header->page);
    p = (char *) XADDR(header->page);

    p += ST_PAGE_HEADER_OFFSET;
    CAST_AND_WRITE(header->trie.root_count, p);
    CAST_AND_WRITE(header->trie.len, p);

    U_ASSERT(header->root_offset == (p - (char *) XADDR(header->page)));

    header->free_space = PAGE_SIZE - header->trie.len - header->trie_offset;

    return read_page_header(header->page, header);
}

st_t make_first_state(st_t tree, struct st_key_object_pair * value)
{
    struct st_page_header page_header;

    st_create_markup_page(&page_header, 1, true);
    tree = page_header.page;
    VMM_SIGNAL_MODIFICATION(tree);
    create_initial_state(&(page_header.trie), value);
    save_page_header(&page_header);

    return tree;
}


static shift_t find_long_jump(char * p, char * end, xptr key)
{
    char * initp = p;
    struct state_descriptor dsc;

    while (p < end) {
        read_state(p, &dsc);
        if ((dsc.flags & STATE_LONG_JUMP) > 0) {
            if (* (xptr *) (dsc.p + 1) == key) {
                return (shift_t) (p - initp);
            }
            p += 9;
        } else {
            p += dsc.len;
        }
    }

    return -1;
}



static xptr shift_root(struct st_page_header * header, xptr * pages_stack)
{
    xptr state;
    xptr state_indir;
    xptr parent_page = *pages_stack;
    shift_t state_jump;
    struct st_page_header new_page_header;
    struct state_descriptor state_dsc;
    char * tmp_buffer;
    shift_t offset;

    #define JUMP_LENGTH (sizeof(uint8_t) + sizeof(xptr))

    state_indir = header->page + CALC_PAGE_SHIFT(header->trie.roots, header->page);

    state = ADDR2XPTR(header->trie.p + * (shift_t *) XADDR(state_indir));
    read_state(XADDR(state), &state_dsc);
    memcpy(tmp_buffer = (char *) malloc(state_dsc.len), state_dsc.p, state_dsc.len);

    if (parent_page == XNULL) {
        st_create_markup_page(&new_page_header, 1, true);
//        tree->root_page = page_header.page;
        VMM_SIGNAL_MODIFICATION(new_page_header.page);
        offset = state_dsc.len + state_dsc.edge_count * JUMP_LENGTH;
        new_page_header.trie.len += offset;
        state_jump = new_page_header.trie.roots[0] = 0;
        save_page_header(&new_page_header);
    } else {
        offset = state_dsc.len + state_dsc.edge_count * JUMP_LENGTH - JUMP_LENGTH;

        CHECKP(parent_page);
        read_page_header(BLOCKXPTR(parent_page), &new_page_header);
        state_jump = find_long_jump(new_page_header.trie.p, new_page_header.trie.p + new_page_header.trie.len, state_indir);
        U_ASSERT(state_jump != 0);

        if (new_page_header.free_space < offset) {
            page_split(pages_stack, offset, state_jump, &new_page_header);
        }

        CHECKP(new_page_header.page);
        VMM_SIGNAL_MODIFICATION(new_page_header.page);
        memmove_ABd(new_page_header.trie.p + state_jump, new_page_header.trie.p + new_page_header.trie.len, offset);
        new_page_header.trie.len += offset;
        save_page_header(&new_page_header);
    }

    memcpy(new_page_header.trie.p + state_jump, tmp_buffer, state_dsc.len);
    free(tmp_buffer);
    read_state(new_page_header.trie.p + state_jump, &state_dsc);

    for (int i = 0; i < state_dsc.edge_count; i++) {
        state_dsc.pointers[i] = i * JUMP_LENGTH;
    }

    return new_page_header.page + (new_page_header.trie_offset + state_jump + state_dsc.len);
}

static void move_part(char * source, shift_t * subtrees, size_t * sizes, int slice_start, int slice_end, xptr xpage, xptr out_xptr)
{
    int c = slice_end - slice_start;
    char * p, * page_start;
    shift_t * shifts;
    char * out;
    xptr x;

    struct st_page_header page;

    read_page_header(xpage, &page);

    page_start = p = page.trie.p + c * sizeof(shift_t);
    shifts = (shift_t *) page.trie.p;

    for (int i = slice_start; i < slice_end; i++) {
        memcpy(p, source + subtrees[i], sizes[i]);
        *shifts = p - page_start; shifts++;
        p += sizes[i];
    }

    x = page.page + page.trie_offset;

    CHECKP(out_xptr);
    out = (char *) XADDR(out_xptr);

    for (int i = slice_start; i < slice_end; i++) {
        * (xptr *) (out + i * JUMP_LENGTH + 1) = x;
        x += sizeof(shift_t);
    }

    page.trie.len = p - page_start;
    page.trie.root_count = c;

    save_page_header(&page);
}

static void split_pages(xptr xpage, shift_t * subtrees, shift_t offset, int count, xptr out)
{
    size_t * sizes;
    size_t counter = 0, total_size = 0;
    int split_index;
    struct st_page_header page, rpage_header;
    char * state_tmp_buffer;

    read_page_header(xpage, &page);
    state_tmp_buffer = (char * ) malloc(page.trie.len - (page.trie_offset + offset));
    memcpy(state_tmp_buffer, page.trie.p + offset, page.trie.len - (page.trie_offset + offset));

    sizes = (size_t *) malloc(count * sizeof(size_t));

    for (int i = 0; i < count; i++) {
        total_size += (sizes[i] = get_local_state_length(state_tmp_buffer + subtrees[i], true));
    }

    total_size /= 2;

    counter = 0;
    for (int i = 0; i < count; i++) {
        counter += sizes[i];
        if (counter > total_size) {
            split_index = i == 0 ? 1 : i;
            break;
        }
    }

    VMM_SIGNAL_MODIFICATION(rpage_header.page);
    st_create_markup_page(&rpage_header, split_index, true);
    move_part(state_tmp_buffer, subtrees, sizes, 0, split_index, rpage_header.page, out);
    VMM_SIGNAL_MODIFICATION(page.page);
    st_create_markup_page(&page, count - split_index, false);
    move_part(state_tmp_buffer, subtrees, sizes, split_index, count, xpage, out);

    free(state_tmp_buffer);
    free(sizes);
}

void page_split(xptr * page_stack, shift_t size, shift_t state, struct st_page_header * page_header)
{
    xptr page = * page_stack;
    struct state_descriptor dsc;
    xptr state_long_jumps;
    struct st_page_header rpage;
    shift_t * subtrees;
    void * state_long_jumps_ptr;

    if (page_header->trie.root_count == 1) {
        read_state(page_header->trie.p + page_header->trie.roots[0], &dsc);
        memcpy(subtrees = (shift_t *) malloc(dsc.edge_count * sizeof(shift_t)), dsc.pointers, dsc.edge_count * sizeof(shift_t));
        state_long_jumps = shift_root(page_header, page_stack - 1);
        split_pages(page, subtrees, dsc.len, dsc.edge_count, state_long_jumps);
    } else {
        xptr indir;
        memcpy(subtrees = (shift_t *) malloc(page_header->trie.root_count * sizeof(shift_t)), page_header->trie.roots, page_header->trie.root_count * sizeof(shift_t));
        indir = page_header->page + CALC_PAGE_SHIFT(page_header->trie.roots, page_header->page);

        U_ASSERT(page_stack[-1] != XNULL);
        read_page_header(page_stack[-1], &rpage);
        state_long_jumps = rpage.page + rpage.trie_offset + find_long_jump(rpage.trie.p, rpage.trie.p + rpage.trie.len, indir);
        split_pages(page, subtrees, 0, page_header->trie.root_count, state_long_jumps);
    }
    free(subtrees);
}

#define MAX_STATE_PATH_LENGTH 32

struct tree_state_path * find_state_path(st_t tree, const char * key, size_t key_length, xptr * page_stack)
{
    char * c = (char *) key;
    struct st_page_header page_header;
    struct tree_state * state;
    int count = 0;
    struct tree_state * state_array[MAX_STATE_PATH_LENGTH];
    struct tree_state_path * result;
    struct tree_state * last_state;
    xptr * page = page_stack;
    void * state_ptr;

    read_page_header(tree, &page_header);
    U_ASSERT(page_header.trie.root_count == 1);
    state_ptr = page_header.trie.p + page_header.trie.roots[0];
    last_state = NULL;

    if (state_ptr != NULL) do {
        state = state_find_key((char *) state_ptr, &c, &key_length, (struct tree_state *) calloc(1, sizeof(struct tree_state)));
        state_array[count++] = state;

        if (*page != BLOCKXPTR(state->ptr)) {
            page++;
            *page = BLOCKXPTR(state->ptr);
        }

        if (state->prefix_differs) {
            break;
        }

        if ((*c == '\0') && (state->final)) {
            last_state = state;
            break;
        }

        state_ptr = state_image_find_prefix(*c, state);
    } while ((state_ptr != NULL) && (*c != '\0'));

    page++;
    *page = XNULL;

    result = (struct tree_state_path *) calloc(1, sizeof(struct tree_state_path) + count * sizeof(void*));
    result->last_state = last_state;
    result->state_count = count;
    result->key_break = (c - key);
    memcpy(result->states, state_array, sizeof(void *) * count);

    return result;
};


void free_state_path(struct tree_state_path * state_path)
{
    struct tree_state * state;

    for (int i = 0; i < state_path->state_count; i++) {
        free(state_path->states[i]);
    }

    free(state_path);

    return;
}

