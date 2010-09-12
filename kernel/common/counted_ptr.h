/*
 * File:  counted_ptr.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef COUNTED_PTR_H
#define COUNTED_PTR_H

template <class T> struct de_delete {
    inline static void deallocate(T * p) { delete p; }
};

template <class T> struct de_delete_array {
    inline static void deallocate(T * p) { delete[] p; }
};

template <class T> struct de_free {
    inline static void deallocate(T * p) { free(p); }
};

template <class T, class Deallocator = de_delete<T> >
class counted_ptr {
  private:
    struct storage {
        int count;   // number of owners
        T* ptr;       // pointer to the value
    } * item;

  public:
    // initialize pointer with existing pointer
    // - requires that the pointer p is a return value of new
    counted_ptr (T* p = NULL)
     : item(new storage) {
        item->ptr = p;
        item->count = 1;
    }

    // copy pointer (one more owner)
    counted_ptr (const counted_ptr<T, Deallocator>& p) throw()
     : item(p.item) {
        if (item != NULL) { ++(item->count); }
    }

    // destructor (delete value if this was the last owner)
    ~counted_ptr () throw() {
        dispose();
    }

    // assignment (unshare old and share new value)
    counted_ptr<T, Deallocator>& operator= (const counted_ptr<T, Deallocator>& p) throw() {
        if (this != &p) {
            dispose();
            item = p.item;
            if (item != NULL) { ++(item->count); }
        }
        return *this;
    }

    // access the value to which the pointer refers
    T& operator*() const throw() { return *(item->ptr); }
    T* operator->() const throw() { return item->ptr; }
    T& operator[](int i) const throw() { return item->ptr[i]; }
    T* get() const throw() { return item == NULL ? NULL : item->ptr; }
    bool unique() const throw() {return item->count == 1; }

  private:
    void dispose() {
        if (item && item->count == 0) {
            if (item->ptr != NULL) { Deallocator::deallocate(item->ptr); }
            delete item;
        }
    }
};

#endif // COUNTED_PTR_H

