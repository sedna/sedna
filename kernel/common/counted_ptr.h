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
    T* ptr;        // pointer to the value
    long* count;   // shared number of owners

  public:
    // initialize pointer with existing pointer
    // - requires that the pointer p is a return value of new
    counted_ptr (T* p = NULL)
     : ptr(p), count(new long(1)) {
    }

    // copy pointer (one more owner)
    counted_ptr (const counted_ptr<T, Deallocator>& p) throw()
     : ptr(p.ptr), count(p.count) {
        ++*count;
    }

    // destructor (delete value if this was the last owner)
    ~counted_ptr () throw() {
        dispose();
    }

    // assignment (unshare old and share new value)
    counted_ptr<T, Deallocator>& operator= (const counted_ptr<T, Deallocator>& p) throw() {
        if (this != &p) {
            dispose();
            ptr = p.ptr;
            count = p.count;
            ++*count;
        }
        return *this;
    }

    // access the value to which the pointer refers
    T& operator*() const throw() { return *ptr; }
    T* operator->() const throw() { return ptr; }
    T& operator[](int i) const throw() { return ptr[i]; }
    T* get() const throw() { return ptr;}
    bool unique() const throw() {return *count == 1; }

  private:
    void dispose() {
        if (count != NULL && --*count == 0) {
             delete count;
             Deallocator::deallocate(ptr);
        }
    }
};


#endif // COUNTED_PTR_H

