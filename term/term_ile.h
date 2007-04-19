#ifndef ILE_H

#ifndef _WIN32
#include <unistd.h>
#else
#include <io.h>
#define isatty(fd) _isatty(fd)
#define fileno(file) _fileno(file)
#endif

#include <cstdio>

int ile_init();
void ile_deinit();
const char * ile_gets(size_t * sz);

#endif

