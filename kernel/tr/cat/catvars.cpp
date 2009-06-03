#include "tr/cat/catvars.h"

#include "common/sedna.h"

const bool ccache_available = false;
int ccache_size;

int last_nid_size = 0;
char * last_nid = NULL;

xptr catalog_masterblock = XNULL;
