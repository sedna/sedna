#include "portal.h"

using namespace std;
using namespace portal;

void portalOnTransactionBegin() {
    virtualNodeStorage = new VirtualStorage();
}

void portalOnTransactionEnd(bool commit) {
    delete virtualNodeStorage;
    virtualNodeStorage = NULL;
}

se_ostream & VirtualElement::print(se_ostream & crmout) const {

}

void VirtualElement::addNode(const xptr node) {
    this->portal.add(node);
    this->count++;
}

void VirtualElement::addNode(const tuple_cell &t) {
    this->vitual_nodes.push_back(PositionedVirtualElement(this->count, t.get_portal()));
    this->count++;
}
