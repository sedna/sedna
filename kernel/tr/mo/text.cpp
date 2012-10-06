/*
 * File:  text.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/mo.h"

#include "tr/mo/microoperations.h"
#include "tr/mo/microsurgery.h"
#include "tr/mo/indexupdate.h"

#include "tr/pstr/pstr.h"
#include "tr/pstr/pstr_long.h"
#include "tr/strings/strings.h"
#include "tr/strings/e_string.h"

#include "tr/mo/nodemoutils.h"
#include "tr/structures/nodeinterface.h"

#include <string.h>

static char tmp_text_buffer[PAGE_SIZE];

using namespace internal;

inline xptr findNearestTextContainerCP(xptr anode)
{
    CommonTextNode node = anode;
    node.checkp();
    do {
        node = getPreviousDescriptorOfSameSort(node.getPtr());
    } while (!node.isNull() && !node.isPstr());

    if (!node.isNull()) {
        return block_xptr(node.getPstrPtr());
    }

    node = anode;
    node.checkp();
    do {
        node = getNextDescriptorOfSameSort(node.getPtr());
    } while (!node.isNull() && !node.isPstr());

    return node.isNull() ? pstr_create_blk(IS_DATA_BLOCK(anode)) : block_xptr(node.getPstrPtr());
}

static
char * copyTextToBuffer(char * buffer, const text_source_t &src)
{
    // FIXME : UNSAFE size usage!

    switch (src.type) {
      case text_source_t::text_mem : {
        memcpy(buffer, src.u.cstr, (size_t) src._text_size);
      } break;
      case text_source_t::text_pstr : {
        xptr ptr = src.u.data;
        CHECKP(ptr);
        memcpy(buffer, (char*) XADDR(ptr), (size_t) src._text_size);
      } break;
      case text_source_t::text_estr : {
        estr_copy_to_buffer(buffer, src.u.data, src._text_size);
      } break;
      default : throw USER_EXCEPTION2(SE1003, "Failed to copy too long text to in-memory buffer");
    }

    return buffer;
}


inline static
xptr pstr_allocate_u(xptr text_block, xptr node, const text_source_t &src)
{
    // FIXME : UNSAFE size usage!

    xptr result;

    if (src.type == text_source_t::text_mem) {
        result = pstr_allocate(text_block, node, src.u.cstr, (size_t) src._text_size);
    } else {
        copyTextToBuffer(tmp_text_buffer, src);
        result = pstr_allocate(text_block, node, tmp_text_buffer, (size_t) src._text_size);
    }

    return result;
}

/**
 * There are three different kinds of text storage:
 *  - Very short text (usually leq then pointer size) is stored in
 *   the node descriptor itself.
 *  - Less then blocksize text is stored in PSTR storage.
 *  - Large text (more then a blocksize) is stored in PSTR_LONG
 *
 *  All three different types of storage are handeled with
 *  different functions here.
 */

void insertTextValue(xptr node_xptr, const text_source_t source)
{
    node_text_t * text_node = getTextFromAnyNode(node_xptr);
    const strsize_t size = get_text_size(source);

    if ((uint64_t)size > STRMAXSIZE) {
            throw USER_EXCEPTION2(SE2037, "Too long text value to insert");
    } else if (size == 0) {
        WRITEP(node_xptr);
        text_node->size = emptyText;
    } else if (size <= maxDescriptorTextSize) {
        /* Very short text case */
        char * buffer;

        if (source.type == text_source_t::text_mem) {
            buffer = (char *) source.u.cstr;
        } else {
            buffer = tmp_text_buffer;
            copyTextToBuffer(buffer, source);
        }

        WRITEP(node_xptr);
        memcpy(text_node->data, buffer, (size_t) size);
        text_node->size = (uint16_t) size;
    } else if (size <= PSTRMAXSIZE) {
        /* PSTR text case */
        pstr_allocate_u(findNearestTextContainerCP(node_xptr), node_xptr, source);
    } else {
        /* PSTR_LONG text case */
        pstr_long_create_str(node_xptr, source);
    }
}

/* This function can combine all types of text storage. */

void insertTextValue(enum insert_position_t position, xptr node_xptr, const text_source_t source)
{
    node_text_t * text_node = getTextFromAnyNode(node_xptr);
    CommonTextNode nodeobject = node_xptr;
    const strsize_t curr_size = nodeobject.getTextSize();
    const strsize_t add_size = get_text_size(source);
    const strsize_t new_size = curr_size + add_size;

    if ((uint64_t)new_size > STRMAXSIZE) {
        throw USER_EXCEPTION2(SE2037, "Too long text value to insert");
    } else if (new_size == 0) {
        return ;
    } else if (nodeobject.isPstrLong()) {
        if (position == ip_tail) {
            pstr_long_append_tail(node_xptr, source);
        } else {
            pstr_long_append_head(node_xptr, source);
        }
    } else {
        xptr data = nodeobject.getTextPointerCP();
        U_ASSERT(curr_size <= PSTRMAXSIZE);
        const size_t curr_size_s = (size_t) curr_size;

        /* Old insertion (uncatchable at runtime) bug check when
         * the node is inserted to itself */
        U_ASSERT((source.type != text_source_t::text_pstr) || source.u.data != data);

        CHECKP(data)
        memcpy(tmp_text_buffer, (char *) XADDR(data), curr_size_s);

        if (new_size > PSTRMAXSIZE) {
            if (nodeobject.isPstr()) {
                pstr_deallocate(node_xptr);
            }

            if (position == ip_tail) {
                pstr_long_create_str(node_xptr, text_source_mem(tmp_text_buffer, curr_size_s));
                pstr_long_append_tail(node_xptr, source);
            } else {
                pstr_long_create_str(node_xptr, source);
                pstr_long_append_tail(node_xptr, text_source_mem(tmp_text_buffer, curr_size_s));
            }
        } else {
            CHECKP(node_xptr);
            if (position == ip_tail) {
                copyTextToBuffer(tmp_text_buffer + curr_size_s, source);
            } else {
                const size_t offset = add_size;
                memmove(tmp_text_buffer + offset, tmp_text_buffer, curr_size_s);
                copyTextToBuffer(tmp_text_buffer, source);
            }

            if (new_size > maxDescriptorTextSize) {
                /* New size still less than PSTRMAXSIZE */
                if (nodeobject.isPstr()) {
                    pstr_modify(node_xptr, tmp_text_buffer, (size_t) new_size);
                } else {
                    pstr_allocate(findNearestTextContainerCP(node_xptr), node_xptr, tmp_text_buffer, (size_t) new_size);
                }
            } else {
                /* Very short text case */
                WRITEP(node_xptr);
                text_node->size = (uint16_t) new_size;
                memcpy(text_node->data, tmp_text_buffer, (size_t) new_size);
            }
        }
    }
}

inline void convertLongtextToText(xptr node_xptr, size_t size)
{
    pstr_long_copy_to_buffer(tmp_text_buffer, node_xptr);
    pstr_long_delete_str(node_xptr);
    insertTextValue(node_xptr, text_source_mem(tmp_text_buffer, size));
}

void deleteTextValue(enum insert_position_t position, xptr node_xptr, strsize_t size)
{
    CHECKP(node_xptr);
    node_text_t * text_node = getTextFromAnyNode(node_xptr);
    CommonTextNode nodeobject = node_xptr;
    const strsize_t curr_size = nodeobject.getTextSize();

    if (curr_size <= size)
        throw SYSTEM_EXCEPTION("wrong recovery of text node");

    CHECKP(node_xptr);

    if (text_node->size <= maxDescriptorTextSize) {
        VMM_SIGNAL_MODIFICATION(node_xptr);

        text_node->size -= (uint16_t) size;
        if (position == ip_head) {
            memmove(text_node->data, text_node->data + (ptrdiff_t) size, text_node->size);
        }
    } else if (nodeobject.isPstr()) {
        size_t new_size = (size_t) (curr_size - size);
        xptr data_ptr = nodeobject.getTextPointerCP();

        CHECKP(data_ptr);
        if (position == ip_tail) {
            memcpy(tmp_text_buffer, XADDR(data_ptr), new_size);
        } else {
            memcpy(tmp_text_buffer, (char *) XADDR(data_ptr) + size, new_size);
        }

        pstr_modify(node_xptr, tmp_text_buffer, new_size);
    } else {
        CHECKP(node_xptr);
        U_ASSERT(nodeobject.isPstrLong());

        if (position == ip_tail) {
            pstr_long_truncate(node_xptr, size);
        } else {
            pstr_long_delete_head(node_xptr, size);
        }

        if ((curr_size - size) <= PSTRMAXSIZE) {
            convertLongtextToText(node_xptr, (size_t) (curr_size - size));
        }
    }
}

void deleteTextValue(xptr node_xptr)
{
    CHECKP(node_xptr);
    CommonTextNode nodeobject = node_xptr;

    if (nodeobject.isPstrLong()) {
        pstr_long_delete_str(node_xptr);
    } else if (nodeobject.isPstr()) {
        pstr_deallocate(node_xptr);
    } else {
        WRITEP(node_xptr);
        getTextFromAnyNode(node_xptr)->size = 0;
    }
}

