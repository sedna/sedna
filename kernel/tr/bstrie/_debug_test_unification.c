
#include "st_unify.h"

char buffer[10*1024*1024];
char * curbuf = buffer;

void vmm_alloc_data_block(xptr * block) {
    *block = (uint32_t) (curbuf = (char *) ((((uint32_t) curbuf) & PAGE_BIT_MASK) + PAGE_SIZE));
    //
//    *block = ADDR2XPTR(malloc(PAGE_SIZE));
}
