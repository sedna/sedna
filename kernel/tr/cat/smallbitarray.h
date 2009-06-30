#ifndef _SBA_H
#define _SBA_H

#include<stdint.h>

struct bit_array {
    uint32_t a[SBA_SIZE];
};

static const int DeBruijnBitPosition[32] = 
{
  0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8, 
  31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
};

static const uint32_t DeBruijnSequence = 0x077CB531UL;

inline static int get_bit_position(uint32_t v) 
{
    return DeBruijnBitPosition[((v & -v) * DeBruijnSequence) >> 27];
}

static int sba_find     (struct bit_array *B, int * n)
{
    uint32_t c, tmp;
    int seq_begin, seq_end, seq_len, map_id;
    int i = 0;

    while (i < SBA_SIZE) {
        c = B->a[i];
        map_id = i;
        i++;
        tmp = ~c;
        if (tmp == 0) {
            continue;
        } else {
            seq_begin = get_bit_position(tmp);
            seq_end = get_bit_position((0xFFFFFFFFUL << seq_begin) ^ tmp);
            if (seq_end == 0) seq_end = 32;
        }

        while ((i < SBA_SIZE) && ((seq_end & 31) == 0)) {
            tmp = B->a[i];
            if (tmp == 0) {
                seq_end += 32;
                i++;
                continue;
            } else if ((tmp & 0x1) == 0) {
                seq_end += get_bit_position(tmp);
            } else break;
        }

        seq_len = seq_end - seq_begin;
        if (seq_len < *n) *n = seq_len;
        return seq_begin + map_id * 32;
    }
    return -1;
}

#if defined(__GNUC__) && (__GNUC__ >= 4) && (__GNUC_MINOR__ >= 2)
#pragma GCC diagnostic ignored "-Wunused-function"
#endif /* GNUC */

static void sba_print (struct bit_array *B)
{
    for (int i = 0; i < SBA_SIZE*32; i++) {
        if ((i % 32 == 0) && (i != 0)) printf("\n");
        if (i % 32 == 0) printf("%08x ", B->a[i / 32]);
        if (i % 32 == 0) printf(" %3d ", i);
        if (i % 4 == 0) printf(" ");
        printf("%d", ((B->a[i / 32] & (1 << (i % 32))) != 0));
    }
    printf("\n");
}

static int sba_occupy (struct bit_array *B, int a, int n)
{
    int map_id, bit, c, ca;
    uint32_t mask;

//  no checks at all !!! (c & mask == 0)

    while (n > 0) {
        map_id = a >> 5;
        bit = a & (32-1);
        ca = (((map_id + 1) << 5) - a);
        c = n + (ca - n) * ((ca < n) & 1);
        mask = ((uint32_t) ((0x1ULL << c) - 1)) << bit;
        B->a[map_id] |= mask;
        n -= c;
        a += c;
    }

    return 0;
}


static int sba_unoccupy (struct bit_array *B, int a, int n)
{
    int map_id, bit, c, ca;
    uint32_t mask;

//  no checks at all !!! (c & mask == 0)

    while (n > 0) {
        map_id = a >> 5;
        bit = a & (32-1);
        ca = (((map_id + 1) << 5) - a);
        c = n + (ca - n) * ((ca < n) & 1);
        mask = ~(((uint32_t) ((0x1ULL << c) - 1)) << bit);
        B->a[map_id] &= mask;
        n -= c;
        a += c;
    }

    return 0;
}


/*
static int sba_unoccupy (struct bit_array *B, int a, int n)
{
}
*/
#endif /* _SBA_H */
