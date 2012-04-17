#include "Scans.h"
#include "SequenceHelpers.h"

#include "tr/executor/xpath/XPathLookup.h"

#include "tr/executor/base/XPath.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/base/PPUtils.h"

struct AbsPath : public AbstractSequence {
    xpe::XPathLookup path;
    xpe::NodeIterator iterator;
    bool docOrder;

    AbsPath(const xpe::Path * pe, bool _docOrder = false) : AbstractSequence(1), path(pe), docOrder(_docOrder) {};

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

struct PathStep : public AbstractSequence {
    SequenceElement inSequence;
    int outTuple;
    xpe::XPathLookup path;
    int state;
    xpe::NodeIterator iterator;

    PathStep(SequenceElement in, const xpe::Path * pe)
    : AbstractSequence(in.seq->body.cells_number + 1), inSequence(in), path(pe), state(0)
    {
        outTuple = in.seq->body.cells_number;
        path.compile();
    };

    virtual bool open() {
        state = 0;
    }

    virtual bool next() {
        tuple_cell node;
        Node result;

        do {
            switch(state) {
                case 0:
                    inSequence.seq->next();
                    copy_tuple(body, inSequence.seq->get(), 0);

                    if (body.is_eos()) {
                        return false;
                    }

                    node = body.cells[inSequence.pos];

                    if (!node.is_node()) {
                        U_ASSERT(false);
                        // throw error
                    }

                    iterator = path.execute(node.get_node());

                    ++state;
                case 1:
                    result = iterator.next();

                    if (result.isNull()) {
                        state = 0;
                        continue;
                    }

                    body.cells[outTuple].set_node(result.getPtr());

                    return true;
                default:
                    U_ASSERT(false);
                    state = 0;
            };
        } while (true);
    };
};
