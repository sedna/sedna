/*
 * File:  counted_ptr.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef COUNTED_PTR_H
#define COUNTED_PTR_H

template <typename T> struct de_delete {
    inline static void deallocate(T * p) { delete p; }
};

template <typename T> struct de_delete_array {
    inline static void deallocate(T * p) { delete[] p; }
};

template <typename T> struct de_free {
    inline static void deallocate(T * p) { free(p); }
};

/*
  Scoped ptr is only appliable to non-array objects, created with new statement
  Notice, that the best way to delete an object stored in this cursor is to NULL or call clear method
*/

template <typename T, class Deallocator = de_delete<T> >
class scoped_ptr {
  private:
    T * p;
  public:
    T& operator*() const throw() { return *p; }
    T* operator->() const throw() { return p; }

    scoped_ptr(T * _p = NULL) : p(_p) {}

    /* Recall: delete operator have no effect on NULL anyway, so this is safe. */
    ~scoped_ptr() { Deallocator::deallocate(p); }

    inline
    T* get() const { return p; };

    // Scoped pointer MUST NOT be neither copied nor assigned to any other scoped pointer!
    // scoped_ptr (const scoped_ptr<T> &ptr) throw() { U_ASSERT(false); }

    void clear() { Deallocator::deallocate(p); p = NULL; };
    bool isnull() const { return NULL == p; };

    /* This implementation of scoped pointer does DELETE old object on assignment  */
    scoped_ptr<T>& operator= (T* ptr) throw() {
        U_ASSERT(p != ptr);
        Deallocator::deallocate(p);
        p = ptr;
        return *this;
    }
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
        if (item != NULL) {
            --(item->count);
            if (item->count == 0) {
                if (item->ptr != NULL) { Deallocator::deallocate(item->ptr); }
                delete item;
            }
        }
    }
};

#endif // COUNTED_PTR_H

