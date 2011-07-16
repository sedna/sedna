/*
 * File:  descriptor.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef DESCRIPTOR_H_
#define DESCRIPTOR_H_

#include "tr/structures/nodetypes.h"
#include "tr/structures/nodeblocks.h"

#include "tr/nid/numb_scheme.h"

namespace internal {
    struct node_base_t {
        t_nid   nid;        /* ordering scheme number */
        xptr    pdsc;       /* pointer to the record in the table of indirect addresses */
        xptr    ldsc;       /* pointer to the descriptor of left sibling item */
        xptr    rdsc;       /* pointer to the descriptor of right sibling item */
        xptr    indir;      /* record in indirection table*/
        shft    desc_next;  /* shift to the next descriptor in block */
        shft    desc_prev;  /* shift to the previous descriptor in block */
    };

    struct node_text_t {
        /* data can be casted to xptr; the only purpose
         * of this is to get rid of padding at the end
         * of the structure */
        char data[sizeof(xptr)]; /* short text or pointer */
        uint16_t size;
    };

    enum { emptyText = 0 };
    enum { textInPstrLong = 0xffff };
    enum { maxDescriptorTextSize = sizeof(xptr) };

    /* For simplicity the same as document child count */
    enum { maxElementChildCount = ((int) ((PAGE_SIZE - sizeof(internal::node_blk_hdr) / 4) / sizeof(xptr))) };

    struct element_node {
        node_base_t base;
        xmlscm_type type;
    };

    struct namespace_node {
        node_base_t base;
        xptr   ns;
    };

    struct attribute_node {
        node_base_t base;
        node_text_t text;
        xmlscm_type type;
    };

    struct document_node {
        node_base_t base;
        node_text_t text;
    };

    struct text_node {
        node_base_t base;
        node_text_t text;
        shft       flags;
    };

    struct pi_node {
        node_base_t base;
        node_text_t text;
        shft   separator;
    };

    inline
    node_base_t * getBase(xptr p) { return (node_base_t *) XADDR(p); }

    inline
    bool isTextType(t_item type) {
        return (type == comment || type == text || type == attribute || type == pr_ins || type == document);
    };

    inline
    size_t size_of_node(t_item t) {
        switch (t) {
        case element : return sizeof(element_node);
        case attribute : return sizeof(attribute_node);
        case text : case comment : return sizeof(text_node);
        case virtual_root: case document : return sizeof(document_node);
        case pr_ins : return sizeof(pi_node);
        case xml_namespace : return sizeof(namespace_node);
        default : return 0;
        }
    }

    inline
    int getNodeChildCount(internal::node_blk_hdr * hdr) {
        return (int) (hdr->dsc_size - internal::size_of_node(hdr->node_type)) / sizeof(xptr);
    }

}

#endif /* DESCRIPTOR_H_ */
