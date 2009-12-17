/*
 * File:  text.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/microoperations.h"

#include "tr/mo/microsurgery.h"
#include "tr/mo/indexupdate.h"

#include "tr/crmutils/node_utils.h"
#include "tr/pstr/pstr.h"
#include "tr/pstr/pstr_long.h"
#include "tr/strings/e_string.h"

inline xptr findNearestTextContainerCP(xptr node_xptr)
{
    t_dsc * node;

    CHECKP(node_xptr);
    node = (t_dsc*) XADDR(node_xptr);
    do {
        node = (t_dsc*) getPreviousDescriptorOfSameSort(node);
    } while (node != NULL && (node->data == XNULL || node->size > PSTRMAXSIZE));

    if (node != NULL) {
        return block_xptr(node->data);
    }

    CHECKP(node_xptr);
    node = (t_dsc*) XADDR(node_xptr);
    do {
        node = (t_dsc*) getNextDescriptorOfSameSort(node);
    } while (node != NULL && (node->data == XNULL || node->size > PSTRMAXSIZE));

    return (node == NULL) ? XNULL : block_xptr(node->data);
}


char * copyTextToBuffer(char * buffer, const void* text, strsize_t size, text_type ttype)
{
    xptr ptr;

    switch (ttype) {
      case text_mem :
        U_ASSERT(size < PSTRMAXSIZE);
        memcpy(buffer, text, (size_t) size);
        break;
      case text_doc :
        ptr = * (xptr*) text;
        CHECKP(ptr);
        U_ASSERT(size < PSTRMAXSIZE);
        memcpy(buffer, (char*) XADDR(ptr), (size_t) size);
        break;
      case text_estr :
        estr_copy_to_buffer(buffer, *(xptr*) text, size);
        break;
    }

    return buffer;
}


inline xptr pstr_allocate_u(xptr text_block, xptr node, const void * s, strsize_t size, text_type ttype)
{
    xptr result;

    U_ASSERT(size <= PSTRMAXSIZE);
    if (ttype==text_mem) {
        result = pstr_allocate(text_block, node, (char *) s, (size_t) size);
    } else {
        char * buffer = (char *) malloc((size_t) size);
        copyTextToBuffer(buffer, s, (size_t) size, ttype);
        result = pstr_allocate(text_block, node, buffer, (size_t) size);
        free(buffer);
    }

    return result;
}


void insertTextValue(xptr node_xptr, const void* text, strsize_t size, text_type ttype)
{
    t_dsc* node= (t_dsc*) XADDR(node_xptr);

    if (size > STRMAXSIZE) {
        throw USER_EXCEPTION(SE2037);
    } else if (size < 1) {
        CHECKP(node_xptr);
        VMM_SIGNAL_MODIFICATION(node_xptr);
        node->data=XNULL;
        node->size=0;
    } else if (size > PSTRMAXSIZE) {
        pstr_long_create_str(node_xptr, text, size, ttype);
    } else {
        /* Search for the nearest text descriptor of the same sort, to insert text near it */
        xptr text_block;

        text_block = findNearestTextContainerCP(node_xptr);
        if (text_block == XNULL) {
            text_block = pstr_create_blk(IS_DATA_BLOCK(node_xptr));
        }

        pstr_allocate_u(text_block, node_xptr, text, size, ttype);
    }
}


void insertTextValue(enum insert_position_t position, xptr node_xptr, const void* text, strsize_t size, text_type ttype)
{
    t_dsc* node= (t_dsc*) XADDR(node_xptr);
    strsize_t curr_size;
    CHECKP(node_xptr);

    curr_size = node->size;

    if ((curr_size + size) > STRMAXSIZE) throw USER_EXCEPTION(SE2037);

    if (curr_size > PSTRMAXSIZE) {
        if (position == ip_tail) {
            pstr_long_append_tail(node_xptr, text, size, ttype);
        } else {
            pstr_long_append_head(node_xptr, text, size, ttype);
        }
    } else {
        CHECKP(node_xptr);
        xptr data = textDereferenceCP(node->data);

        if ((curr_size + size) > PSTRMAXSIZE) {
            U_ASSERT((ttype != text_doc) || (* (xptr *) text) != data);
            char * tmp_buffer = (char *) malloc(curr_size);
            copyTextToBuffer(tmp_buffer, &data, curr_size, text_doc);
            pstr_deallocate(node_xptr);

            if (position == ip_tail) {
                pstr_long_create_str(node_xptr, tmp_buffer, curr_size, text_mem);
                if ((ttype == text_doc) && * (xptr *) text == data) {
                    /* FIXME: Dirty hack! We nid to fix updates to get rid of this */
                    pstr_long_append_tail(node_xptr, tmp_buffer, curr_size, text_mem);
                } else {
                    pstr_long_append_tail(node_xptr, text, size, ttype);
                }
            } else {
                if ((ttype == text_doc) && * (xptr *) text == data) {
                    /* FIXME: Dirty hack! We nid to fix updates to get rid of this */
                    pstr_long_create_str(node_xptr, tmp_buffer, curr_size, text_mem);
                } else {
                    pstr_long_create_str(node_xptr, text, size, ttype);
                }
                pstr_long_append_tail(node_xptr, tmp_buffer, curr_size, text_mem);
            }

            free(tmp_buffer);
        } else {
            char * buffer = (char *) malloc((size_t) (size + curr_size));
            CHECKP(data);
            if (position == ip_tail) {
                memcpy(buffer, XADDR(data), curr_size);
                copyTextToBuffer(buffer + (size_t) curr_size, text, (size_t) size, ttype);
            } else {
                copyTextToBuffer(buffer, text, (size_t) size, ttype);
                memcpy(buffer + (size_t) size, XADDR(data), (size_t) curr_size);
            }
            pstr_modify(node_xptr, buffer, (size_t) (size + curr_size));
            free(buffer);
        }
    }
}

inline void convertLongtextToText(xptr node_xptr, size_t size)
{
    char * buf = (char *) malloc(size);
    pstr_long_copy_to_buffer(buf, node_xptr);
    pstr_long_delete_str(node_xptr);
    insertTextValue(node_xptr, buf, size);
    free(buf);
}

void deleteTextValue(enum insert_position_t position, xptr node_xptr, strsize_t size)
{
    CHECKP(node_xptr);
    t_dsc * node = (t_dsc *) XADDR(node_xptr);
    strsize_t cur_size = node->size;

    if (cur_size <= size)
        throw SYSTEM_EXCEPTION("wrong recovery of text node");

    CHECKP(node_xptr);
    if (cur_size <= PSTRMAXSIZE) {
        size_t new_size = (size_t) (cur_size - size);
        xptr data_ptr = textDereferenceCP(node->data);

        char * buf = (char *) malloc(new_size);

        CHECKP(data_ptr);
        if (position == ip_tail) {
            memcpy(buf, XADDR(data_ptr), new_size);
        } else {
            memcpy(buf, (char *) XADDR(data_ptr) + size, new_size);
        }
        pstr_modify(node_xptr, buf, new_size);

        free(buf);
    } else {
        if (position == ip_tail) {
            pstr_long_truncate(node_xptr, size);
        } else {
            pstr_long_delete_head(node_xptr, size);
        }

        if ((cur_size - size) <= PSTRMAXSIZE) {
            convertLongtextToText(node_xptr, (size_t) (cur_size - size));
        }
    }
}

void deleteTextValue(xptr node_xptr)
{
    if (((t_dsc*)XADDR(node_xptr))->size > PSTRMAXSIZE) {
        pstr_long_delete_str(node_xptr);
    } else {
        pstr_deallocate(node_xptr);
    }
}

