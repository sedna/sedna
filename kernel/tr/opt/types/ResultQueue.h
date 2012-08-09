#ifndef _RESULT_QUEUE_H_
#define _RESULT_QUEUE_H_

#include "tr/structures/nodetypes.h"
#include "tr/models/BlockingQueue.h"
#include "tr/crmutils/crmbase.h"

struct result_item_t
{
    bool continued;
    bool final_one;
    qepNextAnswer next;

    unsigned error;
    
    bool is_atomic;
    xmlscm_type st;
    t_item nt;
    const char * url;

    uint64_t timeConsumed;

    size_t length;
    const char * data;
};

class BlockingItemQueue
{
    BlockingQueue<result_item_t> container;
    typedef BlockingQueue<result_item_t> container_t;

    char ** buffers;
public:
    BlockingItemQueue(std::size_t max_size, unsigned int _timeout, size_t bufferSize)
      : container(max_size, _timeout)
    {
        buffers = new char*[max_size];

        for (std::size_t i = 0; i < container.maxSize(); ++i)
        {
            buffers[i] = (char *) malloc(bufferSize);
        };
    };

    ~BlockingItemQueue()
    {
        for (std::size_t i = 0; i < container.maxSize(); ++i)
        {
            free(buffers[i]);
        };

        delete[] buffers;
    };

    result_item_t pop() { return container.pop(); };
};

#endif /* _RESULT_QUEUE_H_ */
