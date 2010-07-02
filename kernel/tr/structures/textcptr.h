/*
 * File:  textcptr.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef TEXTCPTR_H_
#define TEXTCPTR_H_

class text_cptr {

#ifdef _MSC_VER
#pragma warning( disable : 4200 )
#endif /* _MSC_VER */

    struct counted_str {
        int counter;
        size_t size;
        char text[0];
    } * target;

#ifdef _MSC_VER
#pragma warning( default : 4200 )
#endif /* _MSC_VER */

    inline void release() {
        if (target != NULL) {
            if (--target->counter == 0) {
                se_free(target);
            }
            target = NULL;
        }
    }

    inline void acquire(counted_str * c) {
        target = c;
        if (target != NULL) { ++target->counter; }
    }

public :
    inline explicit text_cptr(xptr node) : target(NULL) {
        CHECKP(node);
//        internal::node_text_t * dsc = getTextFromAnyNode(node);
        TextNode tnode = node;

        if (tnode.isEmpty()) {
            target = NULL;
        } else if (tnode.isPstrLong()) {
            // TODO!
        } else {
            size_t size = (size_t) tnode.getTextSize();
            target = (counted_str *) se_alloc(sizeof(counted_str) + size);
            target->counter = 1;
            target->size = size;
            if (size > 0) {
                tnode.copyToBuffer(target->text, 0, size);
            }
        }
    }

    text_cptr(const text_cptr &p) : target(p.target) { acquire(p.target); };

    text_cptr & operator=(const text_cptr & r) {
        if (this != &r) {
            release();
            acquire(r.target);
        }
        return *this;
    }

    ~text_cptr() { release(); }

    char * get() const { return target == NULL ? NULL : target->text; };
    size_t getSize() const { return target == NULL ? 0 : target->size; };
};

#endif /* TEXTCPTR_H_ */
