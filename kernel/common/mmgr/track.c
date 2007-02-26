#include "common/sedna.h"
#include "stdio.h"

#ifdef SE_MEMORY_TRACK

#undef malloc(size)
#undef free(pointer)
#undef realloc(pointer, size)

#endif /* SE_MEMORY_TRACK */

struct ALLOC_INFO;
struct ALLOC_INFO_LIST;

typedef struct ALLOC_INFO {
	void* address;
	struct ALLOC_INFO* next;
    usize_t size;
    char file[64];
    int line;
} ALLOC_INFO;

typedef struct ALLOC_INFO_LIST
{
    ALLOC_INFO* top;
    void  (*insert) (struct ALLOC_INFO_LIST* list, ALLOC_INFO* info);
    void  (*remove) (struct ALLOC_INFO_LIST* list, void* ptr);

} ALLOC_INFO_LIST;

ALLOC_INFO_LIST *allocList;

void insert_alloc_list(ALLOC_INFO_LIST* list, ALLOC_INFO* info)
{
    info->next = list->top;
    list->top = info;
}

void remove_alloc_list(ALLOC_INFO_LIST* list, void* ptr)
{
    ALLOC_INFO*  itr             = list->top;
    ALLOC_INFO** ptr_to_next_ptr = &(list->top);
    while(itr)
    {
        if(itr->address == ptr)
        {
            ALLOC_INFO* next_cur = itr->next;
            free(itr);
            *ptr_to_next_ptr = next_cur; 
            break;
        }
        else
        {
            ptr_to_next_ptr = &(itr->next);
            itr = itr->next;
        }
    }
}

void init_alloc_list(ALLOC_INFO_LIST* list)
{
    list->top = NULL;
    list->insert = &insert_alloc_list;
    list->remove = &remove_alloc_list;
}

void AddTrack(void* addr, usize_t asize, const char *fname, int lnum)
{
    ALLOC_INFO *info;

    if(!allocList) {
	    allocList = (ALLOC_INFO_LIST*)malloc(sizeof(ALLOC_INFO_LIST));
	    init_alloc_list(allocList);
	}

    info = (ALLOC_INFO*)malloc(sizeof(ALLOC_INFO));
    info->next = NULL;
    info->address = addr;
    strncpy(info->file, fname, 63);
    info->line = lnum;
    info->size = asize;
    allocList->insert(allocList, info);
}

void RemoveTrack(void* addr)
{
    if(!allocList) return;
    allocList->remove(allocList, addr);
}

void DumpUnfreed()
{
    int totalSize = 0;
    char buf[1024];
    ALLOC_INFO* itr;

    if(!allocList) return;
    itr = allocList->top;

    while(itr) {
        printf("%-50s:\t\tLINE %d,\t\tADDRESS %d\t%d unfreed\n", itr->file, itr->line, itr->address, itr->size);
        totalSize += itr->size;
        itr = itr->next;
    }
	
	printf("-----------------------------------------------------------\n");
    printf("Total Unfreed: %d bytes\n", totalSize);
}
