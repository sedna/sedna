#include "tr/cat/catmem.h"

int allocated_objects;
int deallocated_objects;

char * cat_strcpy(void * parent, const char * src)
{
    if (src == NULL) { return NULL; }
    int n = strlen(src) + 1;
    char * dest = (char *) cat_malloc(parent, n);
    return strncpy(dest, src, n);
}
