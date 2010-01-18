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

static char tmp_text_buffer[PAGE_SIZE];

inline xptr findNearestTextContainerCP(xptr node_xptr)
{
    t_dsc * node;

    CHECKP(node_xptr);
    node = (t_dsc*) XADDR(node_xptr);
    do {
        node = (t_dsc*) getPreviousDescriptorOfSameSort(node);
    } while (node != NULL && !isPstr(node));

    if (node != NULL) {
        return block_xptr(node->data.lsp.p);
    }

    CHECKP(node_xptr);
    node = (t_dsc*) XADDR(node_xptr);
    do {
        node = (t_dsc*) getNextDescriptorOfSameSort(node);
    } while (node != NULL && !isPstr(node));

    return (node == NULL) ? pstr_create_blk(IS_DATA_BLOCK(node_xptr)) : block_xptr(node->data.lsp.p);
}


char * copyTextToBuffer(char * buffer, const void* text, strsize_t size, text_type ttype)
{
    switch (ttype) {
      case text_mem : {
        U_ASSERT(size <= PSTRMAXSIZE);
        memcpy(buffer, text, (size_t) size);
      }
        break;
      case text_doc : {
        xptr ptr;
        ptr = * (xptr*) text;
        CHECKP(ptr);
        U_ASSERT(size <= PSTRMAXSIZE);
        memcpy(buffer, (char*) XADDR(ptr), (size_t) size);
      }
        break;
      case text_estr : {
        estr_copy_to_buffer(buffer, *(xptr*) text, size);
      }
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
        copyTextToBuffer(tmp_text_buffer, s, (size_t) size, ttype);
        result = pstr_allocate(text_block, node, tmp_text_buffer, (size_t) size);
    }

    return result;
}

void insertTextValue(xptr node_xptr, const void* text, strsize_t size, text_type ttype)
{
    t_dsc* node= (t_dsc*) XADDR(node_xptr);

    if (size > STRMAXSIZE) {
        throw USER_EXCEPTION(SE2037);
    } else if (size <= max_indsc_text_size) {
        WRITEP(node_xptr);

        if (size > 0) {
            char * buffer;

            if (ttype==text_mem) {
                buffer = (char *) text;
            } else {
                buffer = tmp_text_buffer;
                copyTextToBuffer(buffer, text, (size_t) size, ttype);
                WRITEP(node_xptr);
            }

            memcpy(node->data.st, buffer, size);
        }

        node->ss = (int8_t) size;
    } else if (size > PSTRMAXSIZE) {
        pstr_long_create_str(node_xptr, text, size, ttype);
    } else {
        pstr_allocate_u(findNearestTextContainerCP(node_xptr), node_xptr, text, size, ttype);
    }
}

void insertTextValue(enum insert_position_t position, xptr node_xptr, const void* text, strsize_t size, text_type ttype)
{
    t_dsc* node= (t_dsc*) XADDR(node_xptr);
    strsize_t curr_size;

    CHECKP(node_xptr);
    curr_size = getTextSize(node);
    if ((curr_size + size) > STRMAXSIZE) throw USER_EXCEPTION(SE2037);

    if (isPstrLong(node)) {
        if (position == ip_tail) {
            pstr_long_append_tail(node_xptr, text, size, ttype);
        } else {
            pstr_long_append_head(node_xptr, text, size, ttype);
        }
    } else {
        CHECKP(node_xptr);
        xptr data = getTextPtr(node);

        if ((curr_size + size) > PSTRMAXSIZE) {
            U_ASSERT((ttype != text_doc) || (* (xptr *) text) != data);
            char * tmp_buffer = tmp_text_buffer;
            copyTextToBuffer(tmp_buffer, (char*) XADDR(data), curr_size, text_mem);
            pstr_deallocate(node_xptr);

            if (position == ip_tail) {
                pstr_long_create_str(node_xptr, tmp_buffer, curr_size, text_mem);
                if ((ttype == text_doc) && * (xptr *) text == data) {
                    /* FIXME: Dirty hack! We need to fix updates to get rid of this */
                    pstr_long_append_tail(node_xptr, tmp_buffer, curr_size, text_mem);
                } else {
                    pstr_long_append_tail(node_xptr, text, size, ttype);
                }
            } else {
                if ((ttype == text_doc) && * (xptr *) text == data) {
                    /* FIXME: Dirty hack! We need to fix updates to get rid of this */
                    pstr_long_create_str(node_xptr, tmp_buffer, curr_size, text_mem);
                } else {
                    pstr_long_create_str(node_xptr, text, size, ttype);
                }
                pstr_long_append_tail(node_xptr, tmp_buffer, curr_size, text_mem);
            }
        } else {
            char * buffer = tmp_text_buffer;
            if (position == ip_tail) {
                CHECKP(data);
                memcpy(buffer, XADDR(data), curr_size);
                copyTextToBuffer(buffer + (size_t) curr_size, text, (size_t) size, ttype);
            } else {
                copyTextToBuffer(buffer, text, (size_t) size, ttype);
                CHECKP(data);
                memcpy(buffer + (size_t) size, XADDR(data), (size_t) curr_size);
            }
            if (isPstr(node)) {
                pstr_modify(node_xptr, buffer, (size_t) (size + curr_size));
            } else {
                pstr_allocate(findNearestTextContainerCP(node_xptr), node_xptr, buffer, (size_t) (size + curr_size));
            }
        }
    }
}

inline void convertLongtextToText(xptr node_xptr, size_t size)
{
    pstr_long_copy_to_buffer(tmp_text_buffer, node_xptr);
    pstr_long_delete_str(node_xptr);
    insertTextValue(node_xptr, tmp_text_buffer, size);
}

void deleteTextValue(enum insert_position_t position, xptr node_xptr, strsize_t size)
{
    CHECKP(node_xptr);
    t_dsc * node = (t_dsc *) XADDR(node_xptr);
    strsize_t cur_size = getTextSize(node);

    if (cur_size <= size)
        throw SYSTEM_EXCEPTION("wrong recovery of text node");

    CHECKP(node_xptr);

    if (node->ss >= 0) {
        VMM_SIGNAL_MODIFICATION(node_xptr);

        node->ss -= (int8_t) size;
        if (position == ip_head) {
            memmove(node->data.st, node->data.st + (int8_t) size, node->ss);
        }
    } else if (isPstr(node)) {
        size_t new_size = (size_t) (cur_size - size);
        xptr data_ptr = getTextPtr(node);

        CHECKP(data_ptr);
        if (position == ip_tail) {
            memcpy(tmp_text_buffer, XADDR(data_ptr), new_size);
        } else {
            memcpy(tmp_text_buffer, (char *) XADDR(data_ptr) + size, new_size);
        }

        pstr_modify(node_xptr, tmp_text_buffer, new_size);
    } else {
        CHECKP(node_xptr);
        U_ASSERT(isPstrLong(node));

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
    CHECKP(node_xptr);

    if (isPstrLong(T_DSC(node_xptr))) {
        pstr_long_delete_str(node_xptr);
    } else if (isPstr(T_DSC(node_xptr))) {
        pstr_deallocate(node_xptr);
    } else {
        WRITEP(node_xptr);
        T_DSC(node_xptr)->ss = 0;
    }
}

