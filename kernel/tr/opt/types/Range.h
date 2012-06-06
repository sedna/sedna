#ifndef _RANGE_H_
#define _RANGE_H_

struct Range {
    double lower, upper;

    inline Range() : lower(0), upper(0) {};
    inline explicit Range(double x) : lower(x), upper(x) {};
    inline explicit Range(double _lower, double _upper) : lower(_lower), upper(_upper) { };
    inline Range(const Range & _range) : lower(_range.lower), upper(_range.upper) {};

    inline Range & operator= (const Range & _range) { if (this != &_range) { lower = _range.lower; upper = _range.upper; }; return *this; };
    inline Range & operator= (double y) { lower = y; upper = y; return *this; };

    inline Range & normalize() { if (lower > upper) { std::swap(lower, upper); }; return *this; };

    inline Range operator* (const Range &y) const { return Range(lower * y.lower, upper * y.upper).normalize(); }
    inline Range operator/ (const Range &y) const { return Range(lower / y.lower, upper / y.upper).normalize(); }

    inline Range operator+ (const Range &y) const { return Range(lower + y.lower, upper + y.upper); }
    inline Range operator- (const Range &y) const { return Range(lower - y.lower, upper - y.upper); }

    inline Range operator* (double y) const { return Range(lower * y, upper * y).normalize(); }
    inline Range operator/ (double y) const { return Range(lower / y, upper / y).normalize(); }

    inline Range operator+ (double y) const { return Range(lower + y, upper + y); }
    inline Range operator- (double y) const { return Range(lower - y, upper - y); }

    inline Range & operator*= (const Range &y) { lower *= y.lower; upper *= y.upper; return this->normalize(); }
    inline Range & operator/= (const Range &y) { lower /= y.lower; upper /= y.upper; return this->normalize(); }

    inline Range & operator+= (const Range &y) { lower += y.lower; upper += y.upper; return *this; }
    inline Range & operator-= (const Range &y) { lower -= y.lower; upper -= y.upper; return *this; }

    inline Range & operator*= (double y) { lower *= y; upper *= y; return this->normalize(); }
    inline Range & operator/= (double y) { lower /= y; upper /= y; return this->normalize(); }

    inline Range & operator+= (double y) { lower += y; upper += y; return *this; }
    inline Range & operator-= (double y) { lower -= y; upper -= y; return *this; }

    template <typename OP>
    inline Range map() const { OP op; return Range(op(lower), op(upper)); };

    inline double avg() const { return (lower + upper) / 2; };

    inline bool operator< (const Range &y) const { return avg() < y.avg(); }
};

#endif /* _RANGE_H_ */
