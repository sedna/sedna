/*
 * File: catstructures.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CATSTRUCTURES_H
#define _CATSTRUCTURES_H

#include "tr/cat/catmem.h"
#include "tr/cat/simplestream.h"

template<class T>
struct cat_list {

    struct item {
        T   object;
        typename cat_list<T>::item *next;
        typename cat_list<T>::item *prev;
    } * first;

    inline cat_list() : first(NULL) {};

    void clear() {
        typename cat_list<T>::item * j, * i = this->first;

        while (i != NULL) {
            j = i;
            i = i->next;
            j->object.~T();
            cat_free(j);
        }
    };

    ~cat_list() {
        clear();
    }

    inline bool empty() { return first == NULL; };

    void add(T obj) {
        typename cat_list<T>::item * i = (typename cat_list<T>::item *) cat_malloc(this, sizeof(struct cat_list<T>::item));
        i->object = obj;
        i->prev = NULL;
        i->next = this->first;
        if (this->first != NULL) this->first->prev = i;
        this->first = i;
    }

    void remove_object(struct cat_list<T>::item * item) {
        if (item->next != NULL) { item->next->prev = item->prev; }
        if (item->prev != NULL) { item->prev->next = item->next; }
        if (item == this->first) { this->first = item->next; }
        item->object.~T();
        cat_free(item);
    }

    void remove(T obj) {
        typename cat_list<T>::item *j, * i = this->first;

        while (i != NULL) {
            j = i;
            i = i->next;
            if (j->object == obj) {
                if (j->next != NULL) { j->next->prev = j->prev; }
                if (j->prev != NULL) { j->prev->next = j->next; }
                if (j == this->first) { this->first = j->next; }
                j->object.~T();
                cat_free(j);
            }
        }
    }

    int get_count() {
        int c = 0;
        for (typename cat_list<T>::item *i = this->first; i != NULL; i = i->next) c++;
        return c;
    }

    void serialize(se_simplestream &stream) {
        int c = get_count();
        stream.write(&c, sizeof(int));
        for (typename cat_list<T>::item *i = this->first; i != NULL; i = i->next) 
            stream.write(&(i->object), sizeof(T));
    }

    void deserialize(se_simplestream &stream) {
        int c;
        typename cat_list<T>::item *i, *p = NULL;
        stream.read(&c, sizeof(int));

        for (; c > 0; c--) {
            i = (typename cat_list<T>::item *) cat_malloc(this, sizeof(struct cat_list<T>::item));
            stream.read(&(i->object), sizeof(T));
            i->next = NULL;
            if (this->first == NULL) {
                this->first = i;
                i->prev = NULL;
            } else {
                i->prev = p;
                p->next = i;
            }
            p = i;
        }
    }
};


#endif // _CATSTRUCTURES_H
