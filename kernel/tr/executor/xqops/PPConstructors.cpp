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
#include "tr/structures/producer.h"
#include "tr/executor/base/SCElementProducer.h"

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

    bool xmlNs = ns->same_prefix("xml");
    bool xmlUri = ns->same_uri("http://www.w3.org/XML/1998/namespace");

    return !(xmlNs ^ xmlUri);
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

/*
 * Global state shared between all constructors and some classes
 */

struct constructor_context_t {
    IElementProducer * producer;
    IElementProducer * virtualRoot;

    IElementProducer * getParent(bool deep_copy) {
        if (producer == NULL || deep_copy) {
            return virtualRoot;
        } else {
            return producer;
        }
    }
};

static constructor_context_t constructorContext = { NULL, NULL };

void PPConstructor::checkInitial()
{
    if (constructorContext.virtualRoot == NULL) {
        constructorContext.virtualRoot = SCElementProducer::createVirtualRoot(XNULL);
    }
}

/*
 * Clears global state of constructors.
 * It's called in kernel statement end in trn.
 */
void PPConstructor::clear_virtual_root()
{
    if (constructorContext.virtualRoot != NULL) {
        SCElementProducer::deleteVirtualRoot();
        constructorContext.virtualRoot = NULL;
    }
}

Node PPConstructor::getVirtualRoot()
{
    checkInitial();

    return dynamic_cast<SCElementProducer *>(constructorContext.virtualRoot)->getNode();
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
        portal::VirtualElementWriter elementProducer(cxt);

        U_ASSERT(false);
        // TODO: implement

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
        bool preserveType = cxt->get_static_context()->get_construction_mode();

        scoped_ptr<IElementProducer> producer
            (constructorContext.getParent(deep_copy)->
                addElement(name, preserveType ? xs_anyType : xs_untyped));

        /* Save context */
        IElementProducer * oldParent = constructorContext.producer;
        constructorContext.producer = producer.get();

        /* Insert proper namespace node */
        if (nsHandler.getNamespace() != NULL_XMLNS) {
            producer->addNS(nsHandler.getNamespace());
        }

        /* Process content nodes */
        tuple contentTuple(content.ts); /* Content iterator, used throughout the function */
        content.op->next(contentTuple);
        while (!contentTuple.is_eos()) {
            tuple_cell tc = contentTuple.cells[0];
            do { // To save the final cycle part
                if (tc.is_atomic()) {
                    producer->addAtomic(tc);
                } else {
                    Node node = Node(tc.get_node()).checkp();
                    t_item nodeType = node.getNodeType();

                    U_ASSERT(nodeType != virtual_root);

                    if (nodeType == xml_namespace) {
                        xmlns_ptr ns = NSNode(node).getNamespaceLocal();
                        nsHandler.add(ns);

                        if (!ns->has_prefix()) {
                            // Default namespace should only appear in inner_ns_node, so pass it
                            continue;
                        }
                    }

                    if (!producer->hasNode(tc)) {
                        producer->addNode(tc, preserveType);
                    }
                }
            } while (false);
            content.op->next(contentTuple);
        }

        nsHandler.clear();
        constructorContext.producer = oldParent;

        /* Result */
        tuple_cell result = producer->close();
        t.copy(result);
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

static
xmlns_ptr swizzle_context_namespace(dynamic_context * cxt, xmlns_ptr ns) {
    U_ASSERT(ns != NULL_XMLNS);

    xmlns_ptr cns = cxt->get_xmlns_by_prefix(ns->get_prefix(), true);

    if (cns == NULL_XMLNS || cns == ns) {
        return NULL_XMLNS;
    } else {
        return swizzle_context_namespace(cxt,
            generate_prefix(ns->has_prefix() ? ns->get_prefix() : "new", ns->get_uri()));
    }

    return NULL_XMLNS;
};


void PPAttributeConstructor::do_next (tuple &t)
{
    if (first_time) {
        first_time = false;
        xsd::QName name = resolveQName(at_name, qname, cxt);

        if (name.getXmlNs() != NULL_XMLNS && !name.getXmlNs()->has_prefix()) {
          /* TRICKY, REVIEW . From here : http://www.w3.org/TR/xquery/#id-attributes
            <<  If the attribute name has no namespace prefix, the attribute is in no namespace. >>
            But this is only valid for "direct" attribute constructors */
            if (at_name != NULL) {
                // A direct constructor
                name = xsd::QName::createNsN(NULL_XMLNS, name.getLocalName());
            } else {
                // A computed constructor
          /* TRICKY:
            << If the expanded QName returned by the atomized name expression has a namespace URI but
            has no prefix, it is given an implementation-dependent prefix. >> */
                name = xsd::QName::createNsN(
                    swizzle_context_namespace(cxt,
                        generate_prefix("new", name.getXmlNs()->get_uri())),
                    name.getLocalName());
            }
        }

        text_source_t value = getTextContent(at_value, content, true);

        IElementProducer * parent = constructorContext.getParent(deep_copy);

        /* Swizzle namespace if needed */
        if (name.getXmlNs() != NULL_XMLNS) {
            xmlns_ptr newns = swizzle_context_namespace(cxt, name.getXmlNs());

            if (newns != NULL_XMLNS) {
                parent->addNS(newns);
                name = xsd::QName::createNsN(newns, name.getLocalName());
            }
        }

        tuple_cell result = parent->addAttribute(name, value, xs_untypedAtomic);

        /* Result */
        t.copy(result);
    } else {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPAttributeConstructor::do_copy(dynamic_context *_cxt_)
{
    PPAttributeConstructor *res;
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

        tuple_cell result = constructorContext.getParent(deep_copy)->addNS(ns);

        t.copy(result);
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

        text_membuf_t textbuf(getTextContent(at_value, content, false));

        int rst = strm.parse(textbuf.getCstr(), textbuf.getSize(), NULL,NULL);

        if (rst == 1 || (textbuf.getSize() > 0 && textbuf.getCstr()[textbuf.getSize()-1] == '-')) {
            throw XQUERY_EXCEPTION(XQDY0072);
        }

        tuple_cell result = constructorContext.getParent(deep_copy)->addComment(textbuf.getTextSource());

        t.copy(result);
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

        text_membuf_t textbuf(getTextContent(at_value, content, false));

        /* TRICKY This is just a search for "?>" or "--", that should not be in content (IT) */
        if (strm.parse(textbuf.getCstr(), textbuf.getSize(), NULL, NULL) != 0) {
            throw XQUERY_EXCEPTION(XQDY0026);
        }

        /* Skip heading whitespaces */
        tuple_cell result = constructorContext.getParent(deep_copy)->addPI(name.getValue(), skipWS(textbuf.getTextSource()));

        t.copy(result);
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

        tuple_cell result = constructorContext.getParent(deep_copy)->addText(text_source);

        //Result
        t.copy(result);
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
        bool preserveType = cxt->get_static_context()->get_construction_mode();

        scoped_ptr<IElementProducer> producer = SCElementProducer::createTemporaryDocument(xsd::AnyURI::check("untitled"), cxt);

        /* Save context */
        IElementProducer * oldParent = constructorContext.producer;
        constructorContext.producer = producer.get();

        /* Process content nodes */
        tuple contentTuple(content.ts); /* Content iterator, used throughout the function */
        content.op->next(contentTuple);

        while (!contentTuple.is_eos()) {
            tuple_cell tc = contentTuple.cells[0];

            if (tc.is_atomic()) {
                producer->addAtomic(tc);
            } else if (!producer->hasNode(tc)) {
                producer->addNode(tc, preserveType);
            }

            content.op->next(contentTuple);
        }

        constructorContext.producer = oldParent;

        /* Result */
        tuple_cell result = producer->close();
        t.copy(result);
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
