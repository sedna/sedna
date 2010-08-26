#include "tr/structures/portal.h"
#include "tr/executor/base/tuple.h"
#include "tr/crmutils/serialization.h"

using namespace std;
using namespace portal;

portal::VirtualStorage * portal::virtualNodeStorage;

void portalOnTransactionBegin() {
    virtualNodeStorage = new VirtualStorage();
}

void portalOnTransactionEnd(bool commit) {
    delete virtualNodeStorage;
    virtualNodeStorage = NULL;
}

VirtualNode * VirtualStorage::createElement() {
    return new VirtualElement();
}

void VirtualStorage::releaseNode(VirtualNode * node) {
    delete node;
}

void VirtualElement::print(Serializer * out) const {
    //
}

void VirtualElement::addNode(const xptr node) {
    this->portal.add(node);
    this->count++;
}

void VirtualElement::addVirtual(const tuple_cell * t) {
    this->vitual_nodes.push_back(PositionedVirtualElement(this->count, (VirtualElement *) t->get_portal()));
    this->count++;
}
