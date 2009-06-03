#ifndef ST_MISC_H
#define ST_MISC_H

#define CAST_AND_READ(dest, src) memcpy(&dest, src, sizeof(dest)); src += sizeof(dest)
#define CAST_AND_WRITE(src, dest) memcpy(dest, &src, sizeof(src)); dest += sizeof(src)

#define memmove_ABC(A, B, C) memmove(C, A, ((char *) (B)) - ((char *) (A)))
#define memmove_ABd(A, B, d) memmove(((char *) (A)) + d, A, ((char *) (B)) - ((char *) (A)))


#endif