#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "block_string_tree.h"

void trial(st_t a, const char * k, const char * obj)
{
    st_enumeration en;
    static char object[32];
    static char key[32];
    size_t len;

    printf("Trial %s \n\n", k);
    st_insert_string(a, k, obj, strlen(obj)+1, false);

    en = st_enum_start(get_initial_state_ptr(a));
    while (st_enum_next(en)) {
        st_get_object(st_enum_get_object(en), object);
        len = st_enum_get_key(en, key);
        key[len] = 0;

        printf("%s = %s\n", key, object);
    }
    st_enum_free(en);
//    __debug_st_print(a);
    printf("-----------------\n\n\n");
}


char * gen(char * a) {
    a[0] = rand() % 27 + 'A';
    for (int i = 1; i < 15; i++) {
        a[i] = rand() % 36 + 'A';
        if (a[i] > 'Z') {
            a[i] = 0;
        }
    }
    return a;
}

int main () {
    st_t a = st_init();
    char s[16];
    s[15] = 0;

    for (int i = 0; i < 300; i++) {
        gen(s);
        trial(a, s, s);
    }

//    st_find(a, "abc2");

//    while (true) {
//    }
//    fgets();
}