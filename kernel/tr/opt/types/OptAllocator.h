#ifndef _OPT_ALLOCATOR_H_
#define _OPT_ALLOCATOR_H_

template <typename T, typename Pool>
struct opt_allocator
{
    typedef T value_type;
    typedef value_type* pointer;
    typedef const value_type* const_pointer;
    typedef value_type& reference;
    typedef const value_type& const_reference;
    typedef size_t size_type;

    template<typename U>
    struct rebind {
        typedef opt_allocator<U> other;
    };

    inline explicit opt_allocator() {}
    inline explicit opt_allocator(opt_allocator const&) {}

    template<typename U>
    inline opt_allocator(opt_allocator<U> const&) {}
    inline ~opt_allocator() {}

    inline pointer address(reference r) { return &r; }
    inline const_pointer address(const_reference r) { return &r; }

    inline pointer allocate(size_type cnt, const_pointer hint = 0)
    {
        return reinterpret_cast<pointer>(Pool::alloc(cnt * sizeof (T)));
    }

    inline void deallocate(pointer p, size_type) { }

    inline size_t max_size() const {
        return Pool::blocksize / sizeof(T);
    }

    // construction/destruction
    inline void construct(pointer p, const T& t) { new(p) T(t); }
    inline void destroy(pointer p) { p->~T(); }

    inline bool operator==(opt_allocator const&) { return true; }
    inline bool operator!=(opt_allocator const& a) { return !operator==(a); }
};

#endif /* _OPT_ALLOCATOR_H_ */
