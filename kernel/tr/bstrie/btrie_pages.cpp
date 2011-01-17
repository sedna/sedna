/*
* BTrie page header routin implementations
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "btrie_internal.h"

struct st_page_header * st_read_page_header(xptr_t p, struct st_page_header * header)
{
    char * c;
    READ_PAGE(p);
    p = GET_PAGE_ADDRESS(p);
    c = (char *) XADDR(p);
    uint16_t trie_count;
    sptr_t all_trie_len;

    header->page = p;

    c += ST_PAGE_HEADER_OFFSET;
    CAST_AND_READ(trie_count, c);
    CAST_AND_READ(all_trie_len, c);
    header->trie_count = trie_count;
    header->tries = (sptr_t *) c;

    c += trie_count * sizeof(sptr_t);
    header->trie_offset = c - ((char *) XADDR(p));
    header->data_end = header->trie_offset + all_trie_len;

    header->free_space = PAGE_SIZE - header->data_end;

    return header;
}

struct st_page_header * st_write_page_header(struct st_page_header * header)
{
    char * c;
    WRITE_PAGE(header->page);
    c = (char *) XADDR(GET_PAGE_ADDRESS(header->page));
    uint16_t trie_count = header->trie_count;
    sptr_t all_trie_len = header->data_end - header->trie_offset;

    c += ST_PAGE_HEADER_OFFSET;
    CAST_AND_WRITE(c, trie_count);
    CAST_AND_WRITE(c, all_trie_len);

    return header;
}

struct st_page_header * st_markup_page(struct st_page_header * new_page_header, int root_count, bool create_page)
{
    char * c;

    if (create_page) {
        NEW_PAGE(new_page_header->page);
    }

    WRITE_PAGE(new_page_header->page);
    
    c = (char *) XADDR(new_page_header->page);
    uint16_t trie_count = root_count;
    sptr_t all_trie_len = 0;

    c += ST_PAGE_HEADER_OFFSET;
    CAST_AND_WRITE(c, trie_count);
    CAST_AND_WRITE(c, all_trie_len);
    new_page_header->trie_count = trie_count;
    new_page_header->tries = (sptr_t *) c;

    memset(c, 0, trie_count * sizeof(sptr_t));
    c += trie_count * sizeof(sptr_t);
    new_page_header->trie_offset = c - ((char *) XADDR(new_page_header->page));
    new_page_header->data_end = new_page_header->trie_offset + all_trie_len;
    new_page_header->free_space = PAGE_SIZE - new_page_header->data_end;

    return new_page_header;
}
