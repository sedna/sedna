#include "LazyIterators.h"
#include "VariableMap.h"

using namespace executor;

void NextWindow::execute(ResultSequence* sequence)
{
    uint64_t saveRestrictMask = producer->restrictMask;
    producer->restrictMask = restrictMask;

    producer->clear();

    if (producer->next()) {
        sequence->push(Result(new NextWindow(*this)));
        nextOp->evaluateTo(sequence->context);
    }

    producer->restrictMask = saveRestrictMask;

}
