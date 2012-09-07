#ifndef _INT_BIT_ITERATOR_
#define _INT_BIT_ITERATOR_

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

template<typename IntType>
class IntBitIterator {
  private:
    IntType z;
    int pos;
  public:
    IntBitIterator(IntType _desc) : z(_desc), pos(0) {};

    inline bool empty() const { return z == 0; };

    inline int next() {
        if (z == 0) {
            return -1;
        }

        if ((z & 0xFFFFFFFFUL) == 0) {
            z >>= 32;
            pos += 32;
        }

        int i = get_bit_position(z & 0xFFFFFFFFUL);

        z >>= i;
        z >>= 1;
        pos += i+1;

        return pos-1;
    };
};

#endif /* _INT_BIT_ITERATOR_ */
