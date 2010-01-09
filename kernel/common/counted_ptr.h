/*
 * Copyright (c) Yonat Sharon (yonat@ootips.org)
 *
 * This is the file originated from http://ootips.org/yonat/4dev/counted_ptr.h
 * Its author is Yonat Sharon. He says "fill free to use it".
 */

/*
 * counted_ptr - simple reference counted pointer.
 *
 * The is a non-intrusive implementation that allocates an additional
 * int and pointer for every counted object.
 */

#ifndef COUNTED_PTR_H
#define COUNTED_PTR_H

/* For ANSI-challenged compilers, you may want to #define
 * NO_MEMBER_TEMPLATES or explicit */

//#define NO_MEMBER_TEMPLATES

template <class X> class counted_ptr
{
public:
    typedef X element_type;

    explicit counted_ptr(X* p = 0) // allocate a new counter
        : itsCounter(0) {if (p) itsCounter = se_new counter(p);}
    ~counted_ptr()
        {release();}
    counted_ptr(const counted_ptr& r) throw()
        {acquire(r.itsCounter);}
    counted_ptr& operator=(const counted_ptr& r)
    {
        if (this != &r) {
            release();
            acquire(r.itsCounter);
        }
        return *this;
    }

    X* detach()
    {
        if (unique() && itsCounter)
        {
            X* ptr = itsCounter->ptr;
            delete itsCounter;
            itsCounter = 0;
            return ptr;
        }
        else return NULL;
    }
/*
#ifndef NO_MEMBER_TEMPLATES
    template <class Y> friend class counted_ptr<Y>;
    template <class Y> counted_ptr(const counted_ptr<Y>& r) throw()
        {acquire(r.itsCounter);}
    template <class Y> counted_ptr& operator=(const counted_ptr<Y>& r)
    {
        if (this != &r) {
            release();
            acquire(r.itsCounter);
        }
        return *this;
    }
#endif // NO_MEMBER_TEMPLATES
*/
    X& operator[](int i)  const throw()   {return itsCounter->ptr[i];} // added by Andrey Fomichev
    X& operator*()        const throw()   {return *itsCounter->ptr;}
    X* operator->()       const throw()   {return itsCounter->ptr;}
    X* get()              const throw()   {return itsCounter ? itsCounter->ptr : 0;}
    bool unique()         const throw()
        {return (itsCounter ? itsCounter->count == 1 : true);}

private:

    struct counter {
        counter(X* p = 0, unsigned c = 1) : ptr(p), count(c) {}
        X*          ptr;
        unsigned    count;
    }* itsCounter;

    void acquire(counter* c) throw()
    { // increment the count
        itsCounter = c;
        if (c) ++c->count;
    }

    void release()
    { // decrement the count, delete if it is 0
        if (itsCounter) {
            if (--itsCounter->count == 0) {
                delete itsCounter->ptr;
                delete itsCounter;
            }
            itsCounter = 0;
        }
    }
};

#endif // COUNTED_PTR_H

