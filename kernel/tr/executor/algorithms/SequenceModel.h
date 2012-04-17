/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEQUENCE_MODEL_H
#define SEQUENCE_MODEL_H

struct tuple;
struct AbstractSequence;

struct SequenceElement {
    AbstractSequence * seq;
    int pos;

    bool sorted();

    SequenceElement(AbstractSequence * _seq,  int _pos) : seq(_seq), pos(_pos) {};
    SequenceElement(const SequenceElement & from) : seq(from.seq), pos(from.pos) {};
    SequenceElement & operator =(SequenceElement & to) { seq = to.seq; pos = to.pos; };
};

struct AbstractSequence {
protected:
    tuple body;
public:
    AbstractSequence(int size) : body(size) {};

    virtual bool open() = 0;
    virtual bool next() = 0;

    const tuple& get() const { return body; };
};

#endif /* SEQUENCE_MODEL_H */
