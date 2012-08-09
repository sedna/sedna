#ifndef _BLOCKING_QUEUE_H_
#define _BLOCKING_QUEUE_H_

#include "common/u/umutex.h"

#include <deque>

struct ScopedMutexWrapper
{
private:
    uMutexType * mutex;
    bool locked;
public:
    void lock() { locked = (uMutexLock(mutex, __sys_call_error) == 0); };
    void unlock() { if (locked) { uMutexUnlock(mutex, __sys_call_error); } };

    ScopedMutexWrapper(uMutexType * _mutex, bool _locked = true)
      : mutex(_mutex), locked(false) { if (_locked) { lock(); } };
    ~ScopedMutexWrapper() { if (locked) { unlock(); } };
};

class TimeoutException : public std::exception
{ };

template<typename _Tp, typename _Alloc = std::allocator<_Tp> >
class BlockingQueue : protected std::deque<_Tp, _Alloc>
{
public:
    typedef _Tp value_type;
    typedef typename std::deque<_Tp, _Alloc> parent_t;
private:
    uMutexType mutex;
    uCondType isEmpty;
    uCondType isFull;

    bool nullSink;

    std::size_t m_max_size;
    unsigned timeout;
public:
    BlockingQueue(std::size_t max_size, unsigned _timeout)
      : m_max_size(max_size), timeout(_timeout)
    {
        uMutexInit(&mutex, __sys_call_error);
        uCondInit(&isEmpty, __sys_call_error);
        uCondInit(&isFull, __sys_call_error);
    };

    ~BlockingQueue() {
        uCondInit(&isFull, __sys_call_error);
        uCondInit(&isEmpty, __sys_call_error);
        uMutexInit(&mutex, __sys_call_error);
    };

    void setNullSink() {
        ScopedMutexWrapper lock(&mutex);
        nullSink = true;
        lock.unlock();

        clear();

        uCondSignal(&isEmpty, __sys_call_error);
        uCondSignal(&isFull, __sys_call_error);
    };

    std::size_t maxSize() const
    {
        return m_max_size;
    };

    std::size_t setMaxSize(std::size_t size)
    {
        ScopedMutexWrapper lock(&mutex);
        m_max_size = size;
        return size;
    };

    value_type pop()
    {
        ScopedMutexWrapper lock(&mutex);

        while (!nullSink && parent_t::empty()) {
            if (uCondTimedWait(&isEmpty, &mutex, timeout, __sys_call_error) != 0) {
                throw TimeoutException();
            };
        };

        if (nullSink) {
            return value_type();
        };

        value_type result = parent_t::back();
        parent_t::pop_back();

        lock.lock();

        uCondSignal(&isFull, __sys_call_error);

        return result;
    };

    void push(const value_type & value)
    {
        ScopedMutexWrapper lock(&mutex);

        if (!nullSink && parent_t::size() >= m_max_size) {
            if (uCondTimedWait(&isFull, &mutex, timeout, __sys_call_error) != 0) {
                throw TimeoutException();
            };
        };

        if (nullSink) {
            return;
        };

        parent_t::push_back(value);

        lock.unlock();

        uCondSignal(&isEmpty, __sys_call_error);
    };

    void clear()
    {
        ScopedMutexWrapper lock(&mutex);
        parent_t::clear();
        lock.unlock();
        uCondSignal(&isFull, __sys_call_error);
    };
};

#endif /* _BLOCKING_QUEUE_H_ */
