/*
* File:  PPConstructors.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <vector>
#include <stack>

#include "common/sedna.h"

#include "tr/executor/xqops/PPConstructors.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/xs_names.h"
#include "tr/executor/base/xsd.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/base/xs_helper.h"

#include "tr/updates/updates.h"
#include "tr/structures/metadata.h"
#include "tr/strings/e_string.h"
#include "tr/mo/mo.h"
#include "tr/mo/blocks.h"
#include "tr/mo/microoperations.h"

#include "tr/structures/nodeoperations.h"
#include "tr/structures/nodeutils.h"

#include "tr/structures/portal.h"

using namespace std;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// Static helpers
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

static inline
tuple_cell getQnameParameter(const PPOpIn &qname)
{
    tuple name(qname.ts);

    qname.op->next(name);
    if (name.is_eos() || name.cells_number != 1 )
        throw XQUERY_EXCEPTION2(XPTY0004, "single atomic value is expected in the name expression of an attribute/element constructor");

    tuple_cell res = atomize(name.cells[0]);
    xmlscm_type xtype = res.get_atomic_type();

    if (xtype == xs_untypedAtomic) {
        res = cast(res, xs_string);
    } else if (!is_derived_from_xs_string(xtype) && xtype != xs_string && xtype != xs_QName)
        throw XQUERY_EXCEPTION2(XPTY0004, "unexpected type in the name expression of an attribute/element constructor");

    res = tuple_cell::make_sure_light_atomic(res);

    qname.op->next(name);
    if (!(name.is_eos()))
        throw XQUERY_EXCEPTION2(XPTY0004, "single atomic value is expected in the name expression of an attribute/element constructor");

    return res;
}

static inline
xsd::NCName getNCNameParameter(const PPOpIn &ncname)
{
    tuple_cell name = getQnameParameter(ncname);
    xsd::NCName result;

    if (name.is_atomic_type(xs_QName)) {
        if (name.get_xs_qname().getPrefix()[0] != '\0') {
            throw XQUERY_EXCEPTION2(XPTY0004, "no prefix supposed for NCName");
        }

        result = xsd::NCName::check(name.get_xs_qname().getLocalName(), false);
    } else {
        result = xsd::NCName::check(name.get_str_mem(), false);
    }

    return result;
}

static bool
getStringParameter(PPOpIn content)
{
    tuple value(content.ts);
    content.op->next(value);
    sequence at_vals(1);
    if (value.is_eos())
    {
        executor_globals::tmp_op_str_buf.clear();
        executor_globals::tmp_op_str_buf.append(EMPTY_STRING_TC);
        return true;
    }
    else
    {
        at_vals.add(value);
        content.op->next(value);
    }

    while (!(value.is_eos()))
    {
        if (!(value.cells_number==1 )) throw USER_EXCEPTION2(SE1003, "in PPConstructor");
        at_vals.add(value);
        content.op->next(value);
    }
    executor_globals::tmp_op_str_buf.clear();
    sequence::iterator it=at_vals.begin();
    do
    {
        tuple_cell res=atomize((*it).cells[0]);
        res=cast(res, xs_string);
        res=tuple_cell::make_sure_light_atomic(res);
        executor_globals::tmp_op_str_buf.append(res);
        it++;

    }
    while (it!=at_vals.end());
    return false;
}

static void
getStringWSParameter(PPOpIn content)
{
    executor_globals::tmp_op_str_buf.clear();
    tuple value(content.ts);
    content.op->next(value);
    sequence at_vals(1);
    if (value.is_eos()) return;

    while (!(value.is_eos()))
    {
        if (!(value.cells_number==1 )) throw USER_EXCEPTION2(SE1003, "in PPConstructor");
        at_vals.add(value);
        content.op->next(value);
    }
    executor_globals::tmp_op_str_buf.clear();
    sequence::iterator it=at_vals.begin();
    do
    {
        tuple_cell res=atomize((*it).cells[0]);
        res=cast(res, xs_string);
        res=tuple_cell::make_sure_light_atomic(res);
        executor_globals::tmp_op_str_buf.append(res);
        it++;

    }
    while (it!=at_vals.end());
}


text_source_t getTextContent(const char * cstr, PPOpIn op, bool preserveWS) {
    if (cstr == NULL) {
        if (preserveWS) {
            getStringWSParameter(op);
        } else {
            getStringParameter(op);
        }

        return text_source_strbuf(&(executor_globals::tmp_op_str_buf));
    } else {
        return text_source_mem(cstr, strlen(cstr));
    }
}

static inline
bool isNameValid(xsd::QName qname, bool check_name = true)
{
    const char* name = qname.getLocalName();
    xmlns_ptr ns = qname.getXmlNs();

    /* It has no namespace prefix and its local name is xmlns */
    if(check_name && (ns == NULL_XMLNS || !ns->has_prefix()) && strcmpex(name,"xmlns") == 0) return false;

    if(ns == NULL_XMLNS) return true;

    /* Its namespace prefix is xmlns. */
    if (ns->same_prefix("xmlns")) { return false; }

    if (ns->same_prefix("xml")) {
        if (!ns->same_uri("http://www.w3.org/XML/1998/namespace")) {
            return false;
        }
    } else if (ns->same_uri("http://www.w3.org/XML/1998/namespace")) {
        return false;
    }

    return true;
}

/*
 * Helper for element and docuemnt constructors to insert sequence
 * of atomic values. Returns true if node was actually inserted.
 * In this case left pointer is changed to the last inserted indirection.
 * In any case at_vals sequence is cleared.
 */
static inline bool
process_atomic_values(xptr& left, const xptr& parent, sequence& at_vals) {
    if (at_vals.size() > 0)
    {
        executor_globals::tmp_op_str_buf.clear();
        tuple_cell tcc;
        sequence::iterator it = at_vals.begin();
        do {
            tcc = tuple_cell::make_sure_light_atomic((*it).cells[0]);
            tcc = cast(tcc, xs_string);
            executor_globals::tmp_op_str_buf.append(tcc);
            it++;
        }
        while (it != at_vals.end());

        at_vals.clear();
        if(executor_globals::tmp_op_str_buf.get_size() > 0) {
            insert_text(indirectionDereferenceCP(left),
                        XNULL,
                        indirectionDereferenceCP(parent),
                        text_source_strbuf(&(executor_globals::tmp_op_str_buf)));
            left = get_last_mo_inderection();
            return true;
        }
    }
    return false;
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPConstructor (carries global state shared between all constructors)
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

/*
 * Global state shared between all constructors and some classes
 * which are also inherited from PPConstructor
 */

struct constructor_context_t {
    struct dynamic_part {
        xptr parentElementIndir;
        xptr leftElementIndir;

        int count;

        void clear() {
            parentElementIndir = XNULL;
            leftElementIndir = XNULL;

            count = 0;
        };
    } d;

    schema_node_xptr root_schema;
    xptr lastVirtualElementIndir;
    xptr virtual_root;
};

static std::stack<constructor_context_t::dynamic_part> constructorContextStack;
static constructor_context_t constructorContext = { {XNULL, XNULL, 0}, XNULL, XNULL };

Node PPConstructor::getVirtualRoot()
{
    return constructorContext.virtual_root;
}


xsd::QName PPConstructor::resolveQName(const char* nameString, PPOpIn qname, dynamic_context* cxt) {
    xsd::QName result;

    tuple_cell qnameTuple; /* TRICKY It is defined here, because string, it contains must be used as a parameter in resolve context
     Destructor must not free it earlier that it is needed */

    if (NULL == nameString) {
        qnameTuple = getQnameParameter(qname);
        if (qnameTuple.is_atomic_type(xs_QName)) {
            result = qnameTuple.get_xs_qname();
            nameString = NULL;
        } else {
            nameString = qnameTuple.get_str_mem();
        }
    }

    if (NULL != nameString) {
        result = xsd::QName::createResolveContext(nameString, cxt, false);
    }

    U_ASSERT(result.valid());

    /* Check constraints on full name */
    if (!isNameValid(result, false)) {
        throw XQUERY_EXCEPTION(XQDY0096);
    }

    return result;
}


void PPConstructor::checkInitial()
{
    if (constructorContext.root_schema == XNULL) {
        node_info_t node_info = {XNULL, XNULL, XNULL, virtual_root};
        constructorContext.root_schema = doc_schema_node_object::create_virtual_root()->p;
        xptr blk = createBlock(constructorContext.root_schema, XNULL);
        insertNodeFirst(blk, &node_info);
        constructorContext.virtual_root = node_info.node_xptr;
        constructorContext.d.clear();
        constructorContext.lastVirtualElementIndir = XNULL;
        constructorContextStack.push(constructorContext.d);
    }
}

/*
 * Clears global state of constructors.
 * It's called in kernel statement end in trn.
 */
void PPConstructor::clear_virtual_root()
{
    if (constructorContext.root_schema != XNULL) {
        nid_delete(constructorContext.virtual_root);
        constructorContext.root_schema->drop();
        constructorContext.root_schema = XNULL;
        constructorContext.virtual_root = XNULL;
        constructorContext.lastVirtualElementIndir = XNULL;
    }
}

class ProperNamespace {
  private:
    tuple contentIterator;
    PPOpIn * contentProducer;
    dynamic_context * cxt;

    std::stack<xmlns_ptr> nsList;
    xmlns_ptr ns;
  public:
    ProperNamespace(PPOpIn * _contentProducer, dynamic_context * _cxt)
      : contentIterator(1), contentProducer(_contentProducer),
       cxt(_cxt), ns(NULL_XMLNS) {
    };

    void add(xmlns_ptr _ns) {
        /* Default namespace handling should be done automatically in add_to_context */
        cxt->add_to_context(_ns);
        nsList.push(_ns);
    }

    void collect() {
        if (contentProducer->op) {
            contentProducer->op->next(contentIterator);
            U_ASSERT(!contentIterator.is_eos());
            U_ASSERT(contentIterator.cells[0].is_node());
            U_ASSERT(Node(contentIterator.cells[0].get_node()).checkp().getNodeType() == xml_namespace);

            ns = NSNode(contentIterator.cells[0].get_node()).getNamespaceLocal();

            this->add(ns);

            contentProducer->op->next(contentIterator);

            if (!(contentIterator.is_eos())) {
                throw XQUERY_EXCEPTION2(XPTY0004, "single atomic value is expected in the namespace expression of an attribute/element constructor");
            }
        }
    };

    xmlns_ptr getNamespace() const { return ns; };

    void clear() {
        while (!nsList.empty()) {
            cxt->remove_from_context(nsList.top());
            nsList.pop();
        }
    }
};


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPVirtualConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPVirtualConstructor::PPVirtualConstructor(dynamic_context *_cxt_,
        operation_info _info_, PPOpIn _qname_, PPOpIn _content_, bool _deep_copy, PPOpIn _ns) :
            PPConstructor(_cxt_, _info_, _deep_copy), qname(_qname_), content(_content_), inner_ns_node(_ns)

{
    el_name=NULL;
}

PPVirtualConstructor::PPVirtualConstructor(dynamic_context *_cxt_,
        operation_info _info_, const char* name, PPOpIn _content_, bool _deep_copy, PPOpIn _ns) :
            PPConstructor(_cxt_, _info_, _deep_copy), content(_content_), inner_ns_node(_ns)
{
    el_name=se_new char[strlen(name)+1];
    strcpy(el_name,name);

}
PPVirtualConstructor::~PPVirtualConstructor()
{
    if (el_name!=NULL) {
        delete [] el_name;
    } else {
        delete qname.op;
        qname.op = NULL;
    }

    delete content.op;

    content.op = NULL;
}

void PPVirtualConstructor::do_open ()
{
    checkInitial();
    if (el_name==NULL) qname.op->open();
    content.op->open();

    if (inner_ns_node.op != NULL) {
        inner_ns_node.op->open();
    }

    first_time = true;
}

void PPVirtualConstructor::do_reopen()
{
    if (el_name==NULL)  qname.op->reopen();
    content.op->reopen();

    if (inner_ns_node.op != NULL) {
        inner_ns_node.op->reopen();
    }

    first_time = true;
}

void PPVirtualConstructor::do_close()
{
    if (el_name==NULL) qname.op->close();
    content.op->close();

    if (inner_ns_node.op != NULL) {
        inner_ns_node.op->close();
    }
}

inline static
bool isNamespaceTuple(const tuple &t) {
    return t.cells[0].is_node() && getNodeType(checkp(t.cells[0].get_node())) == xml_namespace;
}

tuple_cell PPVirtualConstructor::parent_element;

static inline
void atomics_to_text(portal::VirtualElementWriter &element, sequence &atomic_acc, xptr text_root_indirection) {
    xptr atomic_node = XNULL;
    if (process_atomic_values(atomic_node, text_root_indirection, atomic_acc)) {
        element.add(tuple_cell::node_indir(atomic_node));
    }
}

void PPVirtualConstructor::do_next (tuple &t)
{
    if (first_time) {
        first_time = false;
        std::vector<xmlns_ptr> ns_list;
        portal::VirtualElementWriter elementProducer(cxt);

        U_ASSERT(constructorContext.d.parentElementIndir == XNULL);

        /* Resolve namespace */
        ProperNamespace nsHandler(&inner_ns_node, cxt);
        nsHandler.collect();

        /* Resolve name */
        xsd::QName name = resolveQName(el_name, qname, cxt);

        schema_node_cptr snode = constructorContext.root_schema->get_first_child(name, element);

        if (!snode.found()) {
            snode = constructorContext.root_schema->add_child(name, element);
        }

        elementProducer.create(snode.ptr(), parent_element);

        tuple_cell old_parent_element = parent_element;
        /* TODO: There is a problem with deep_copy constructors: we can create element on
         * next only if we are sure, that there will be no other element inserted after it, that should
         * be inserted before it */
//        parent_element = elementProducer.get();

        const xptr virtual_root_i = getIndirectionSafeCP(constructorContext.virtual_root);
        sequence atomic_acc(1);
        bool noMoreAttributes;

        tuple contentTuple(content.ts); /* Content iterator, used throughout the function */
        content.op->next(contentTuple);

        // Add proper namespace
        elementProducer.add(tuple_cell(nsHandler.getNamespace()));

        while (!contentTuple.is_eos()) {
            tuple_cell tc = contentTuple.cells[0];

            if (tc.is_atomic()) {
                /* Accumulate atomic values */
                atomic_acc.add(contentTuple);
            } else {
                /* If there are any atomic values, add them as a text node */
                atomics_to_text(elementProducer, atomic_acc, virtual_root_i);

                /* Analyze and insert it */
                if (tc.is_node()) {
                    xptr node = tc.get_node();
                    t_item nodeType = getNodeType(node);

                    if (nodeType == attribute && noMoreAttributes) {
                        throw XQUERY_EXCEPTION(XQTY0024);
                    }

                    if (nodeType == xml_namespace) {
                        nsHandler.add(NSNode(node).getNamespaceLocal());
                    }

                    if (nodeType != attribute && nodeType != xml_namespace) {
                        noMoreAttributes = true;
                    }

                    if (!(nodeType == text && CommonTextNode(node).isEmpty())) {
                        elementProducer.add(tc);
                    }
                } else {
                    elementProducer.add(tc);
                }
            }

            content.op->next(contentTuple);
        }

        /* If there are any atomic values left, add them as a text node */
        atomics_to_text(elementProducer, atomic_acc, virtual_root_i);

         /* Clear context namespaces deleting local namespace declarations */
        nsHandler.clear();

        t.copy(elementProducer.get());
        elementProducer.close();
        parent_element = old_parent_element;
    } else {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPVirtualConstructor::do_copy(dynamic_context *_cxt_)
{
    PPVirtualConstructor *res ;
    if (el_name!=NULL)
        res = se_new PPVirtualConstructor(_cxt_, info, el_name, content, deep_copy, inner_ns_node);
    else
    {
        res = se_new PPVirtualConstructor(_cxt_, info, qname, content, deep_copy, inner_ns_node);
        res->qname.op = qname.op->copy(_cxt_);
    }
    res->content.op = content.op->copy(_cxt_);
    return res;
}




///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPElementConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPElementConstructor::PPElementConstructor(dynamic_context *_cxt_,
                                           operation_info _info_,
                                           PPOpIn _qname_,
                                           PPOpIn _content_,
                                           bool _deep_copy,
                                           PPOpIn _ns) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                              qname(_qname_),
                                                              content(_content_),
                                                              el_name(NULL),
                                                              inner_ns_node(_ns)
{
}

PPElementConstructor::PPElementConstructor(dynamic_context *_cxt_,
                                           operation_info _info_,
                                           const char* name,
                                           PPOpIn _content_,
                                           bool _deep_copy,
                                           PPOpIn _ns): PPConstructor(_cxt_, _info_, _deep_copy),
                                                             qname(),
                                                             content(_content_),
                                                             el_name(NULL),
                                                             inner_ns_node(_ns)
{
    el_name=se_new char[strlen(name)+1];
    strcpy(el_name, name);

}
PPElementConstructor::~PPElementConstructor()
{

    if (el_name!=NULL)
    {
        delete [] el_name;
    }
    else
    {
        delete qname.op;
        qname.op = NULL;
    }

    delete content.op;

    content.op = NULL;
}

void PPElementConstructor::do_open ()
{
    checkInitial();
    if (el_name == NULL) qname.op->open();
    content.op->open();

    if (inner_ns_node.op != NULL) {
        inner_ns_node.op->open();
    }

    first_time = true;
}

void PPElementConstructor::do_reopen()
{
    if (el_name==NULL)  qname.op->reopen();
    content.op->reopen();

    if (inner_ns_node.op != NULL) {
        inner_ns_node.op->reopen();
    }

    first_time = true;
}

void PPElementConstructor::do_close()
{
    if (el_name==NULL) qname.op->close();

    if (inner_ns_node.op != NULL) {
        inner_ns_node.op->close();
    }

    content.op->close();
}

void PPElementConstructor::do_next (tuple &t)
{
    if (first_time) {
        first_time = false;

        /* Resolve namespace */
        ProperNamespace nsHandler(&inner_ns_node, cxt);
        nsHandler.collect();

        /* Resolve name */
        xsd::QName name = resolveQName(el_name, qname, cxt);

        /* Element insertion */
        xptr newElement = XNULL;
        bool preserveType = cxt->get_static_context()->get_construction_mode();

        if (constructorContext.d.parentElementIndir == XNULL || deep_copy) {
            newElement = insert_element(
              indirectionDereferenceCP(constructorContext.lastVirtualElementIndir), XNULL, constructorContext.virtual_root,
              name, preserveType ? xs_anyType : xs_untyped);

            constructorContext.lastVirtualElementIndir = get_last_mo_inderection();
        } else if (constructorContext.d.leftElementIndir != XNULL) {
            newElement = insert_element(
              indirectionDereferenceCP(constructorContext.d.leftElementIndir), XNULL, XNULL,
              name, preserveType ? xs_anyType : xs_untyped);

            ++constructorContext.d.count;
            constructorContext.d.leftElementIndir = get_last_mo_inderection();
        } else {
            newElement = insert_element(
              XNULL, XNULL, indirectionDereferenceCP(constructorContext.d.parentElementIndir),
              name, preserveType ? xs_anyType : xs_untyped);

            ++constructorContext.d.count;
            constructorContext.d.leftElementIndir = get_last_mo_inderection();
        }

        xptr indir = get_last_mo_inderection();

        /* Context change */
        U_ASSERT(newElement != XNULL);

        /* Save context */
        constructorContextStack.push(constructorContext.d);
        constructor_context_t::dynamic_part context = constructorContext.d;
        constructorContext.d.clear();
        constructorContext.d.parentElementIndir = indir;

        /* Insert proper namespace node */
        if (nsHandler.getNamespace() != NULL_XMLNS) {
              insert_namespace(XNULL, XNULL, newElement, nsHandler.getNamespace());
              constructorContext.d.leftElementIndir = get_last_mo_inderection();
        }

        /* Process content nodes */
        sequence atomicAccum(1);
        int count = constructorContext.d.count;
        bool noMoreAttributes = false;

        xptr local_left = constructorContext.d.leftElementIndir; /* pointer to insert accumulated text nodes */
        tuple contentTuple(content.ts); /* Content iterator, used throughout the function */
        content.op->next(contentTuple);

        while (!contentTuple.is_eos()) {
            tuple_cell tc = contentTuple.cells[0];

            if (tc.is_atomic()) {
                atomicAccum.add(contentTuple);
            } else {
                if (process_atomic_values(local_left, indir, atomicAccum)) {
                    noMoreAttributes = true;
                    constructorContext.d.leftElementIndir = local_left;
                } else {
                    Node node = Node(tc.get_node()).checkp();
                    t_item nodeType = node.getNodeType();

                    if (nodeType == attribute && noMoreAttributes) {
                        throw XQUERY_EXCEPTION(XQTY0024);
                    }

                    if (nodeType == xml_namespace) {
                        nsHandler.add(NSNode(node).getNamespaceLocal());
                    }

                    /* Skip empty text nodes */
                    if (!(nodeType == text && CommonTextNode(node).isEmpty())) {
                        U_ASSERT(nodeType != virtual_root);

                        // Check if we've already inserted the node to this node as a context
                        if (count < constructorContext.d.count) {
                            count = constructorContext.d.count;
                            constructorContext.d.leftElementIndir = node.getIndirection();
                        } else {
                            if (nodeType == document) {
                                xptr left = copy_node_content(indir, node.getPtr(), constructorContext.d.leftElementIndir, NULL, preserveType);

                                if (left != XNULL) {
                                    constructorContext.d.leftElementIndir = left;
                                }
                            } else {
                                /* depth 1 means, that this is not an update operation copying */
                                constructorContext.d.leftElementIndir =
                                    deep_copy_node_ii(constructorContext.d.leftElementIndir, XNULL, indir, node.getPtr(), NULL, preserveType, 1);
                            }
                        }
                    }
                }
            }

            local_left = constructorContext.d.leftElementIndir;
            content.op->next(contentTuple);
        }

        process_atomic_values(constructorContext.d.leftElementIndir, indir, atomicAccum);

        constructorContext.d = constructorContextStack.top();
        constructorContextStack.pop();

        nsHandler.clear();

        /* Result */
        t.copy(tuple_cell::node_indir(indir));
    } else {
        first_time = true;
        t.set_eos();
    }
}


PPIterator* PPElementConstructor::do_copy(dynamic_context *_cxt_)
{
    PPElementConstructor *res ;
    if (el_name!=NULL)
        res = se_new PPElementConstructor(_cxt_, info, el_name,content, deep_copy, inner_ns_node);
    else
    {
        res = se_new PPElementConstructor(_cxt_, info, qname,content, deep_copy, inner_ns_node);
        res->qname.op = qname.op->copy(_cxt_);
    }
    res->content.op = content.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAttributeConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPAttributeConstructor::PPAttributeConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               PPOpIn _qname_,
                                               PPOpIn _content_,
                                               bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy),
                                                                 qname(_qname_),
                                                                 content(_content_)
{
    at_name=NULL;
    at_value=NULL;
}

PPAttributeConstructor::PPAttributeConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               const char* name,
                                               PPOpIn _content_,
                                               bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy),
                                                                 content(_content_)
{
    at_name=se_new char[strlen(name)+1];
    strcpy(at_name,name);
    at_value=NULL;

}

PPAttributeConstructor::PPAttributeConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               PPOpIn _qname_,
                                               const char* value,
                                               bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy),
                                                                 qname(_qname_)
{
    at_name=NULL;
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
}

PPAttributeConstructor::PPAttributeConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               const char* name,
                                               const char* value,
                                               bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy)
{
    at_name=se_new char[strlen(name)+1];
    strcpy(at_name,name);
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
}

PPAttributeConstructor::~PPAttributeConstructor()
{

    if (at_name!=NULL)
        delete [] at_name;
    else
    {
        delete qname.op;
        qname.op = NULL;
    }
    if (at_value!=NULL)
        delete [] at_value;
    else
    {
        delete content.op;
        content.op = NULL;
    }
}

void PPAttributeConstructor::do_open ()
{
    checkInitial();
    if (at_name==NULL)  qname.op->open();
    if (at_value==NULL) content.op->open();
    first_time = true;
}

void PPAttributeConstructor::do_reopen()
{
    if (at_name==NULL)  qname.op->reopen();
    if (at_value==NULL) content.op->reopen();
    first_time = true;
}

void PPAttributeConstructor::do_close()
{
    if (at_name==NULL)  qname.op->close();
    if (at_value==NULL) content.op->close();
}

void PPAttributeConstructor::do_next (tuple &t)
{
    if (first_time) {
        first_time = false;
        xsd::QName name = resolveQName(at_name, qname, cxt);

        /* TRICKY, REVIEW . From here : http://www.w3.org/TR/xquery/#id-attributes
          <<  If the attribute name has no namespace prefix, the attribute is in no namespace. >>
          But this is only valid for "direct" attribute constructors */

        if (at_name != NULL /* This is a check for direct constructor */
              && name.getXmlNs() != NULL_XMLNS
              && !name.getXmlNs()->has_prefix()) {
            name = xsd::QName::createNsN(NULL_XMLNS, name.getLocalName());
        }

        text_source_t value = getTextContent(at_value, content, true);

        /* Attribute insertion */
        xptr newAttribute;

        if (constructorContext.d.parentElementIndir == XNULL || deep_copy) {
            newAttribute = insert_attribute(XNULL, XNULL, constructorContext.virtual_root, name, xs_untypedAtomic, value);
        } else {
            if (constructorContext.d.leftElementIndir !=XNULL) {
                schema_node_cptr ss = getSchemaNode(
                    indirectionDereferenceCP(
                        constructorContext.d.leftElementIndir));

                t_item typ = ss->type;

                if (typ != attribute && typ != xml_namespace) {
                    if (ss->parent->type != document)
                        throw XQUERY_EXCEPTION(XQTY0024);
                    else
                        throw XQUERY_EXCEPTION(XPTY0004);
                }

                if (name.getXmlNs() != NULL_XMLNS) {
                    const xptr parent = indirectionDereferenceCP(
                        constructorContext.d.parentElementIndir);

                    const xmlns_ptr new_ns = swizzle_namespace(parent, name.getXmlNs());

                    if (new_ns != NULL_XMLNS) {
                        insert_namespace(XNULL, XNULL, parent, new_ns);
                        name = xsd::QName::createNsN(new_ns, name.getLocalName());
                    }
                }

                newAttribute = insert_attribute(
                    indirectionDereferenceCP(constructorContext.d.leftElementIndir),
                        XNULL, XNULL, name, xs_untypedAtomic, value);
            } else {
                newAttribute = insert_attribute(
                    XNULL, XNULL, indirectionDereferenceCP(constructorContext.d.parentElementIndir),
                        name, xs_untypedAtomic, value);
            }

            constructorContext.d.leftElementIndir = get_last_mo_inderection();
            ++constructorContext.d.count;
        }

        /* Result */
        t.copy(tuple_cell::node(newAttribute));
    } else {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPAttributeConstructor::do_copy(dynamic_context *_cxt_)
{
    PPAttributeConstructor *res ;
    if (at_name!=NULL)
    {
        if (at_value!=NULL) res = se_new PPAttributeConstructor(_cxt_, info, at_name,at_value, deep_copy);
        else res = se_new PPAttributeConstructor(_cxt_, info, at_name,content, deep_copy);
    }
    else
    {
        if (at_value!=NULL) res = se_new PPAttributeConstructor(_cxt_, info, qname,at_value, deep_copy);
        else res = se_new PPAttributeConstructor(_cxt_, info, qname,content, deep_copy);
    }
    if (at_name==NULL)res->qname.op = qname.op->copy(_cxt_);
    if (at_value==NULL)res->content.op = content.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPNamespaceConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPNamespaceConstructor::PPNamespaceConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               const char* name,
                                               PPOpIn _content_): PPConstructor(_cxt_, _info_, true),
                                                                  content(_content_)
{
    if (name!=NULL)
    {
        at_name=se_new char[strlen(name)+1];
        strcpy(at_name,name);
    }
    else
        at_name=NULL;
    at_value=NULL;
}

PPNamespaceConstructor::PPNamespaceConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               const char* name,
                                               const char* value): PPConstructor(_cxt_, _info_, true)
{
    if (name!=NULL)
    {
        at_name=se_new char[strlen(name)+1];
        strcpy(at_name,name);
    }
    else
        at_name=NULL;
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
}

PPNamespaceConstructor::~PPNamespaceConstructor()
{
    if (at_name!=NULL)
        delete [] at_name;
    if (at_value!=NULL)
        delete [] at_value;
    else
    {
        delete content.op;
        content.op = NULL;
    }
}

void PPNamespaceConstructor::do_open ()
{
    checkInitial();
    if (at_value==NULL) content.op->open();
    first_time = true;
}

void PPNamespaceConstructor::do_reopen()
{
    if (at_value==NULL) content.op->reopen();
    first_time = true;
}

void PPNamespaceConstructor::do_close()
{
    if (at_value==NULL) content.op->close();
}

void PPNamespaceConstructor::do_next (tuple &t)
{
    if (first_time) {
        first_time = false;

        text_source_t content_value = getTextContent(at_value, content, false);

        xmlns_ptr ns = xmlns_touch(at_name, content_value.u.cstr);

        xptr newNamespace;

        if (constructorContext.d.parentElementIndir == XNULL || deep_copy) {
            newNamespace = insert_namespace(
                XNULL, XNULL, constructorContext.virtual_root, ns);
        } else {
            newNamespace = insert_namespace(XNULL, XNULL,
                /* parent */ indirectionDereferenceCP(constructorContext.d.parentElementIndir), ns);
            ++ constructorContext.d.count;
        }

        t.copy(tuple_cell::node(newNamespace));
    } else {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPNamespaceConstructor::do_copy(dynamic_context *_cxt_)
{
    PPNamespaceConstructor *res ;
    if (at_value!=NULL) res = se_new PPNamespaceConstructor(_cxt_, info, at_name,at_value);
    else res = se_new PPNamespaceConstructor(_cxt_, info, at_name,content);
    if (at_value==NULL)res->content.op = content.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPCommentConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPCommentConstructor::PPCommentConstructor(dynamic_context *_cxt_,
                                           operation_info _info_,
                                           PPOpIn _content_,
                                           bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy),
                                                             content(_content_)
{
    at_value=NULL;
    strm.add_str("--","-");

}

PPCommentConstructor::PPCommentConstructor(dynamic_context *_cxt_,
                                           operation_info _info_,
                                           const char* value,
                                           bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy)
{
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
    strm.add_str("--","-");
}

PPCommentConstructor::~PPCommentConstructor()
{
    if (at_value!=NULL)
        delete [] at_value;
    else
    {
        delete content.op;
        content.op = NULL;
    }
}

void PPCommentConstructor::do_open ()
{
    checkInitial();
    if (at_value==NULL) content.op->open();
    first_time = true;
}

void PPCommentConstructor::do_reopen()
{
    if (at_value==NULL) content.op->reopen();
    first_time = true;
}

void PPCommentConstructor::do_close()
{
    if (at_value==NULL) content.op->close();
}

void PPCommentConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;
        tuple_cell res;

        text_source_t content_value = getTextContent(at_value, content, false);

        if (content_value.type != text_source_t::text_mem) {
            throw USER_EXCEPTION(SE2024);
        }

        int rst = strm.parse(content_value.u.cstr, content_value.size, NULL,NULL);

        if (rst == 1 || (content_value.size > 0 && content_value.u.cstr[content_value.size-1] == '-')) {
            throw XQUERY_EXCEPTION(XQDY0072);
        }

        xptr newComm;
        if (constructorContext.d.parentElementIndir == XNULL || deep_copy )
            newComm= insert_comment(XNULL, XNULL, constructorContext.virtual_root, content_value.u.cstr, content_value.size);
        else
        {
            if (constructorContext.d.leftElementIndir != XNULL) {
                newComm= insert_comment(
                    /* left */ indirectionDereferenceCP(constructorContext.d.leftElementIndir),
                    XNULL, XNULL, content_value.u.cstr, content_value.size);
            } else {
                newComm= insert_comment(XNULL, XNULL,
                    /* parent */ indirectionDereferenceCP(constructorContext.d.parentElementIndir),
                    content_value.u.cstr, content_value.size);
            }

            ++ constructorContext.d.count;
            constructorContext.d.leftElementIndir = get_last_mo_inderection();
        }

        //Result
        t.copy(tuple_cell::node(newComm));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPCommentConstructor::do_copy(dynamic_context *_cxt_)
{
    PPCommentConstructor *res ;
    if (at_value!=NULL) res = se_new PPCommentConstructor(_cxt_, info, at_value, deep_copy);
    else
    {
        res = se_new PPCommentConstructor(_cxt_, info, content, deep_copy);
        res->content.op = content.op->copy(_cxt_);
    }
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPIConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPPIConstructor::PPPIConstructor(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _qname_,
                                 PPOpIn _content_,
                                 bool _deep_copy) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                    qname(_qname_),
                                                    content(_content_)
{
    at_name=NULL;
    at_value=NULL;
    strm.add_str("?>","--");

}
PPPIConstructor::PPPIConstructor(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 const char* name,
                                 PPOpIn _content_,
                                 bool _deep_copy) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                    content(_content_)
{
    at_name=se_new char[strlen(name)+1];
    strcpy(at_name,name);
    at_value=NULL;
    strm.add_str("?>","--");

}
PPPIConstructor::PPPIConstructor(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _qname_,
                                 const char* value,
                                 bool _deep_copy) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                    qname(_qname_)
{
    at_name=NULL;
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
    strm.add_str("?>","--");
}

PPPIConstructor::PPPIConstructor(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 const char* name,
                                 const char* value,
                                 bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy)
{
    at_name=se_new char[strlen(name)+1];
    strcpy(at_name,name);
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
    strm.add_str("?>","--");
}

PPPIConstructor::~PPPIConstructor()
{

    if (at_name!=NULL)
        delete [] at_name;
    else
    {
        delete qname.op;
        qname.op = NULL;
    }
    if (at_value!=NULL)
        delete [] at_value;
    else
    {
        delete content.op;
        content.op = NULL;

    }
}

void PPPIConstructor::do_open ()
{
    checkInitial();
    if (at_name==NULL)  qname.op->open();
    if (at_value==NULL) content.op->open();
    first_time = true;
}

void PPPIConstructor::do_reopen()
{
    if (at_name==NULL)  qname.op->reopen();
    if (at_value==NULL) content.op->reopen();
    first_time = true;
}

void PPPIConstructor::do_close()
{
    if (at_name==NULL)  qname.op->close();
    if (at_value==NULL) content.op->close();
}


inline static
text_source_t skipWS(text_source_t ts) {
    int wp_k = 0;
    int wp_s = ts.size;

    while (wp_k < wp_s) {
        char s = ts.u.cstr[0];
        if (s != 32 && s != 9 && s != 10 && s != 13) break;
        ++ts.u.cstr; --ts.size;
    }

    return ts;
}

void PPPIConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        /* Determine parameter */
        tuple_cell res1;
        res1.set_eos();
        xsd::NCName name;

        if (at_name == NULL) {
            name = getNCNameParameter(qname);
        } else {
            stmt_str_buf res;
            collapse_string_normalization(at_name, res); /* Perform name normalization */
            name = xsd::NCName::check(res.get_tuple_cell().get_str_mem(), false);
        }

        if (charset_handler->matches(name.getValue(), "^(?i:xml)$")) {
            throw XQUERY_EXCEPTION(XQDY0064);
        }

        text_source_t content_value = getTextContent(at_value, content, false);

        if (content_value.type != text_source_t::text_mem) {
            throw USER_EXCEPTION(SE2024);
        }

        /* TRICKY This is just a search for "?>" or "--", that should not be in content (IT) */
        if (strm.parse(content_value.u.cstr, content_value.size, NULL, NULL) != 0) {
            throw XQUERY_EXCEPTION(XQDY0026);
        }

        /* Skip heading whitespaces */
        content_value = skipWS(content_value);

        /* Attribute insertion */
        xptr new_pi;
        if (constructorContext.d.parentElementIndir == XNULL || deep_copy) {
            new_pi = insert_pi(XNULL, XNULL, constructorContext.virtual_root,
                               name.getValue(), strlen(name.getValue()), content_value.u.cstr, content_value.size);
        } else {
            if (constructorContext.d.leftElementIndir !=XNULL) {
                new_pi = insert_pi(indirectionDereferenceCP(constructorContext.d.leftElementIndir),
                    XNULL, XNULL, name.getValue(), strlen(name.getValue()), content_value.u.cstr, content_value.size);
            } else {
                new_pi = insert_pi(XNULL, XNULL,
                    indirectionDereferenceCP(constructorContext.d.parentElementIndir),
                    name.getValue(), strlen(name.getValue()), content_value.u.cstr, content_value.size);
            }
            constructorContext.d.count++;
            constructorContext.d.leftElementIndir = get_last_mo_inderection();
        }

        /* Result */
        t.copy(tuple_cell::node(new_pi));
    } else {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPPIConstructor::do_copy(dynamic_context *_cxt_)
{
    PPPIConstructor *res ;
    if (at_name!=NULL)
    {
        if (at_value!=NULL) res = se_new PPPIConstructor(_cxt_, info, at_name,at_value, deep_copy);
        else res = se_new PPPIConstructor(_cxt_, info, at_name,content, deep_copy);
    }
    else
    {
        if (at_value!=NULL) res = se_new PPPIConstructor(_cxt_, info, qname,at_value, deep_copy);
        else res = se_new PPPIConstructor(_cxt_, info, qname, content, deep_copy);
    }
    if (at_name==NULL)res->qname.op = qname.op->copy(_cxt_);
    if (at_value==NULL)res->content.op = content.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPTextConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPTextConstructor::PPTextConstructor(dynamic_context *_cxt_,
                                     operation_info _info_,
                                     PPOpIn _content_,
                                     bool _deep_copy) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                        content(_content_)
{
    at_value=NULL;

}

PPTextConstructor::PPTextConstructor(dynamic_context *_cxt_,
                                     operation_info _info_,
                                     const char* value,
                                     bool _deep_copy) : PPConstructor(_cxt_, _info_, _deep_copy)
{
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
}
PPTextConstructor::~PPTextConstructor()
{

    if (at_value!=NULL)
        delete [] at_value;
    else
    {
        delete content.op;
        content.op = NULL;
    }
}

void PPTextConstructor::do_open ()
{
    checkInitial();
    if (at_value==NULL) content.op->open();
    first_time = true;
}

void PPTextConstructor::do_reopen()
{
    if (at_value==NULL) content.op->reopen();
    first_time = true;
}

void PPTextConstructor::do_close()
{
    if (at_value==NULL) content.op->close();
}

void PPTextConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;
        text_source_t text_source = getTextContent(at_value, content, false);

        /* from  http://www.w3.org/TR/xquery/#id-textConstructors:

            It is possible for a text node constructor to construct a text node containing a zero-length string.
            However, if used in the content of a constructed element or document node, such a text node will be deleted or merged with another text node.


        if (tsGetActualSize(text_source) == 0) {
            first_time = true;
            t.set_eos();
            return;
        }
        */

        xptr newText;

        if (constructorContext.d.parentElementIndir == XNULL || deep_copy || text_source.size == 0) {
            newText = insert_text(XNULL, XNULL, constructorContext.virtual_root, text_source);
        } else {
            if (constructorContext.d.leftElementIndir != XNULL) {
                newText = insert_text(indirectionDereferenceCP(constructorContext.d.leftElementIndir), XNULL, XNULL, text_source);
            } else {
                newText = insert_text(XNULL, XNULL, indirectionDereferenceCP(constructorContext.d.parentElementIndir), text_source);
            }

            constructorContext.d.count++;
            constructorContext.d.leftElementIndir = get_last_mo_inderection();
        }

        //Result
        t.copy(tuple_cell::node(newText));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPTextConstructor::do_copy(dynamic_context *_cxt_)
{
    PPTextConstructor *res ;
    if (at_value!=NULL) res = se_new PPTextConstructor(_cxt_, info, at_value, deep_copy);
    else
    {
        res = se_new PPTextConstructor(_cxt_, info, content, deep_copy);
        res->content.op = content.op->copy(_cxt_);
    }
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDocumentConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPDocumentConstructor::PPDocumentConstructor(dynamic_context *_cxt_,
                                             operation_info _info_,
                                             PPOpIn _content_): PPConstructor(_cxt_, _info_, false),
                                                                content(_content_)
{


}

PPDocumentConstructor::~PPDocumentConstructor()
{

    delete content.op;
    content.op = NULL;
}

void PPDocumentConstructor::do_open ()
{
    content.op->open();
    first_time = true;
}

void PPDocumentConstructor::do_reopen()
{
    content.op->reopen();
    first_time = true;
}

void PPDocumentConstructor::do_close()
{
    content.op->close();
}

void PPDocumentConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;
        tuple_cell res;

        xptr new_doc = insert_document("tmp", false);
        cxt->add_temporary_doc_node(new_doc);
        xptr indir = nodeGetIndirection(new_doc);

        /* Save context */
        constructorContextStack.push(constructorContext.d);
        constructor_context_t::dynamic_part context = constructorContext.d;
        constructorContext.d.clear();
        constructorContext.d.parentElementIndir = indir;

        int count = constructorContext.d.count;

        sequence at_vals(1);
        while (true) {
            content.op->next(t);

            if (t.is_eos()) break;

            tuple_cell tc=t.cells[0];
            if (tc.is_atomic()) {
                at_vals.add(t);
            } else {
                if (process_atomic_values(constructorContext.d.leftElementIndir, indir, at_vals)) {
                    continue;
                }

                Node node(tc.get_node());
                t_item nodeType = node.checkp().getNodeType();

                switch (nodeType) {
                  case text: {
                      if (CommonTextNode(node).isEmpty()) {
                          continue;
                      }
                    } break;

                  case attribute: {
                    throw XQUERY_EXCEPTION2(XPTY0004, "Attribute node in the document constructor content sequence");
                  }

                  case virtual_root: {
                    throw XQUERY_EXCEPTION2(SE1003, "Virtual root node type in the document constructor content sequence");
                  }

                  case xml_namespace: {
                    throw XQUERY_EXCEPTION2(XPTY0004, "Namespace node in the document constructor content sequence");
                  }

                  default: break;
                }

                // Check if we've already inserted the node to this node as a context
                if (count < constructorContext.d.count) {
                    count = constructorContext.d.count;
                    constructorContext.d.leftElementIndir = node.getIndirection();
                } else if (nodeType == document) {
                    xptr left = copy_node_content(indir, node.getPtr(), constructorContext.d.leftElementIndir,
                                                  NULL, cxt->get_static_context()->get_construction_mode());
                    if (left != XNULL) {
                        constructorContext.d.leftElementIndir = left;
                    }
                } else {
                    /* depth 1 means, that this is not an update operation copying */
                    constructorContext.d.leftElementIndir =
                        deep_copy_node_ii(constructorContext.d.leftElementIndir, XNULL, indir, node.getPtr(),
                                          NULL, cxt->get_static_context()->get_construction_mode(), 1);
                }
            }
        }

        process_atomic_values(constructorContext.d.leftElementIndir, indir, at_vals);

        constructorContext.d = constructorContextStack.top();
        constructorContextStack.pop();

        t.copy(tuple_cell::node_indir(indir));
    } else {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDocumentConstructor::do_copy(dynamic_context *_cxt_)
{
    PPDocumentConstructor *res ;
    res = se_new PPDocumentConstructor(_cxt_, info, content);
    res->content.op = content.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPVisitor Acceptors
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


void PPElementConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (el_name==NULL) qname.op->accept(v);
    content.op->accept(v);
    v.pop();
}

void PPAttributeConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (at_name==NULL)  qname.op->accept(v);
    if (at_value==NULL) content.op->accept(v);
    v.pop();
}

void PPNamespaceConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (at_value==NULL) content.op->accept(v);
    v.pop();
}

void PPCommentConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (at_value==NULL) content.op->close();
    v.pop();
}

void PPPIConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (at_name==NULL)  qname.op->accept(v);
    if (at_value==NULL) content.op->accept(v);
    v.pop();
}

void PPDocumentConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    content.op->accept(v);
    v.pop();
}

void PPTextConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (at_value==NULL) content.op->close();
    v.pop();
}

void PPVirtualConstructor::do_accept (PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (el_name==NULL) qname.op->accept(v);
    content.op->accept(v);
    v.pop();
}
