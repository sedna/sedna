#ifndef SCANS_H
#define SCANS_H

#include "SequenceModel.h"

struct AbsPath : public ITupleOperator {
    pe::NodeIterator iterator;
    bool docOrder;

    AbsPath(const pe::Path& path, bool _docOrder = false) : ITupleOperator(1), path(pe), docOrder(_docOrder) {};

    virtual bool open() {
        iterator = path.execute(XNULL);
    }

    virtual bool next() {
        Node result;

        result = iterator.next();

        if (result.isNull()) {
            body.set_eos();
        }
    };
};


#endif /* SCANS_H */
