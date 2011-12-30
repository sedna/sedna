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

static
tuple_cell getNameTuple(const PPOpIn &qname, bool quietly)
{
    tuple name(qname.ts);

    qname.op->next(name);
    if (name.is_eos() || name.cells_number != 1)
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

static
xsd::NCName getNCNameParameter(const PPOpIn &ncname, bool quietly)
{
    tuple_cell name = getNameTuple(ncname, quietly);
    xsd::NCName result;

    if (name.is_atomic_type(xs_QName)) {
        if (name.get_xs_qname().getXmlNs() != NULL_XMLNS) {
            if (quietly) {
                return xsd::NCName();
            } else {
                throw XQUERY_EXCEPTION2(XPTY0004, "no prefix supposed for NCName");
            }
        }

        result = xsd::NCName::check(name.get_xs_qname().getLocalName(), quietly);
    } else {
        result = xsd::NCName::check(name.get_str_mem(), quietly);
    }

    return result;
}

void getAtomicSequence(const char * direct_content, PPOpIn content, sequence * a) {
    if (direct_content != NULL) {
        a->add(tuple(tuple_cell::atomic(xs_string, strcpy((char *) malloc(strlen(direct_content) + 1), direct_content))));
    } else {
        tuple value(content.ts);
        content.op->next(value);

        while (!value.is_eos()) {
            if (value.cells_number != 1) {
                throw USER_EXCEPTION2(SE1003, "in PPConstructor");
            }

            a->add(tuple(cast(atomize(value.cells[0]), xs_string)));

            content.op->next(value);
        };
    }
}

void concatSequence(sequence * a) {
    executor_globals::tmp_op_str_buf.clear();

    sequence::iterator it = a->begin();
    while (it != a->end()) {
        executor_globals::tmp_op_str_buf.append(tuple_cell::make_sure_light_atomic((*it).cells[0]));
        it++;
    };
}

text_source_t getTextContent(const char * cstr, PPOpIn op) {
    if (cstr == NULL) {
        sequence atvals(1);

        getAtomicSequence(cstr, op, &atvals);
        concatSequence(&atvals);

        return text_source_strbuf(&(executor_globals::tmp_op_str_buf));
    } else {
        return text_source_mem(cstr, strlen(cstr));
    }
}

static inline
bool isPrefixValid(xsd::QName qname)
{
    const char* name = qname.getLocalName();
    xmlns_ptr ns = qname.getXmlNs();

    if (ns == NULL_XMLNS) {
        return true;
    }

    /* Its namespace prefix is xmlns. */
    if (ns->same_prefix("xmlns") || ns->same_uri("http://www.w3.org/2000/xmlns/")) {
        return false;
    }

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

static
xsd::QName resolveQName(const char* nameString, PPOpIn qname, INamespaceMap * skn) {
    xsd::QName result;

    tuple_cell qnameTuple; /* TRICKY It is defined here, because string, it contains must be used as a parameter in resolve context
     Destructor must not free it earlier that it is needed */

    if (NULL == nameString) {
        qnameTuple = getNameTuple(qname, false);
        if (qnameTuple.is_atomic_type(xs_QName)) {
            result = qnameTuple.get_xs_qname();
            nameString = NULL;
        } else {
            nameString = qnameTuple.get_str_mem();
        }
    }

    if (NULL != nameString) {
        result = xsd::QName::createResolve(nameString, skn, true);
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
        constructorContext.virtualRoot = SCElementProducer::getVirtualRoot(XNULL);
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
        cxt->get_static_context()->getStaticallyKnownNamespaces()->setNamespace(_ns);
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
            cxt->get_static_context()->getStaticallyKnownNamespaces()->setNamespace(xmlns_touch(nsList.top()->get_prefix(), ""));
            nsList.pop();
        }
    }
};

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPElementConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPElementConstructor::PPElementConstructor(
    dynamic_context *_cxt_,
    operation_info _info_,
    PPOpIn _qname_,
    PPOpIn _content_,
    bool _deep_copy,
    int _sknMarker,
    bool _virtualElement) :
        PPConstructor(_cxt_, _info_, _deep_copy),
        qname(_qname_),
        content(_content_),
        el_name(NULL),
        sknSnapshot(NULL),
        sknMarker(_sknMarker),
        virtualElement(_virtualElement)
{
}

PPElementConstructor::PPElementConstructor(
    dynamic_context *_cxt_,
    operation_info _info_,
    const char* name,
    PPOpIn _content_,
    bool _deep_copy,
    int _sknMarker,
    bool _virtualElement):
        PPConstructor(_cxt_, _info_, _deep_copy),
        qname(),
        content(_content_),
        el_name(NULL),
        sknSnapshot(NULL),
        sknMarker(_sknMarker),
        virtualElement(_virtualElement)
{
    el_name = strcpy(se_new char[strlen(name)+1], name);
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

    first_time = true;
}

void PPElementConstructor::do_reopen()
{
    if (el_name==NULL)  qname.op->reopen();
    content.op->reopen();

    first_time = true;
}

void PPElementConstructor::do_close()
{
    if (el_name==NULL) qname.op->close();

    content.op->close();
}

void PPElementConstructor::do_next (tuple &t)
{
    if (first_time) {
        first_time = false;

        // TODO: optimize SKN swizzling in the following name:
        // create a cache of namespace maps for each position in StaticallyKnownNamespaces
        // and
        int save_skn_mark = cxt->get_static_context()->getStaticallyKnownNamespaces()->gotoMark(sknMarker);

        /* Resolve name */
        xsd::QName name = resolveQName(el_name, qname, cxt->get_static_context()->getStaticallyKnownNamespaces());

        if (!name.valid()) {
            throw XQUERY_EXCEPTION(XQDY0074);
        }

        /* Check constraints on prefix */
        if (!isPrefixValid(name)) {
            throw XQUERY_EXCEPTION(XQDY0096);
        }

        bool preserveType = cxt->get_static_context()->get_construction_mode();

        scoped_ptr<IElementProducer> producer
            (constructorContext.getParent(deep_copy)->
                addElement(name, preserveType ? xs_anyType : xs_untyped));

        /* Save context */
        IElementProducer * oldParent = constructorContext.producer;
        constructorContext.producer = producer.get();

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

                    if (!producer->hasNode(tc)) {
                        producer->addNode(tc, preserveType);
                    }
                }
            } while (false);

            content.op->next(contentTuple);
        }

        constructorContext.producer = oldParent;

        /* Result */
        tuple_cell result = producer->close();
        t.copy(result);

        cxt->get_static_context()->getStaticallyKnownNamespaces()->gotoMark(save_skn_mark);
    } else {
        first_time = true;
        t.set_eos();
    }
}


PPIterator* PPElementConstructor::do_copy(dynamic_context *_cxt_)
{
    PPElementConstructor *res ;
    if (el_name!=NULL)
        res = se_new PPElementConstructor(_cxt_, info, el_name,content, deep_copy, sknMarker, virtualElement);
    else
    {
        res = se_new PPElementConstructor(_cxt_, info, qname,content, deep_copy, sknMarker, virtualElement);
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
xmlns_ptr swizzle_context_namespace(StaticallyKnownNamespaces * skn, xmlns_ptr ns) {
    U_ASSERT(ns != NULL_XMLNS);

    xmlns_ptr cns = skn->resolvePrefix(ns->get_prefix());

    if (cns == NULL_XMLNS || cns == ns) {
        return ns;
    } else {
        return swizzle_context_namespace(skn,
            generate_prefix(ns->has_prefix() ? ns->get_prefix() : "new", ns->get_uri()));
    }
};


void PPAttributeConstructor::do_next (tuple &t)
{
    if (first_time) {
        first_time = false;

        // HACK ! This is to be fixed, and work only in assumtion, that 1) element constructor moved SKN,
        // and 2) all att. constructors are before all element constructors
        StaticallyKnownNamespaces * skn = cxt->get_static_context()->getStaticallyKnownNamespaces();
        xsd::QName name = resolveQName(at_name, qname, skn);
        IElementProducer * parent = constructorContext.getParent(deep_copy);

        if (!name.valid()) {
            throw XQUERY_EXCEPTION(XQDY0074);
        }

        if (name.getXmlNs() != NULL_XMLNS) do {
            /* TRICKY, REVIEW . From here : http://www.w3.org/TR/xquery/#id-attributes
              <<  If the attribute name has no namespace prefix, the attribute is in no namespace. >>
              But this is only valid for "direct" attribute constructors
              It should be done BEFORE xmlns name check */

              if (at_name != NULL && !name.getXmlNs()->has_prefix()) {
                  // A direct constructor
                  name = xsd::QName::createNsN(NULL_XMLNS, name.getLocalName());
                  break;
              }

              if (!isPrefixValid(name)) {
                  throw XQUERY_EXCEPTION(XQDY0044);
              }

              if (at_name == NULL && !name.getXmlNs()->has_prefix()) {
                      // A computed constructor
                /* TRICKY:
                  << If the expanded QName returned by the atomized name expression has a namespace URI but
                  has no prefix, it is given an implementation-dependent prefix. >>
                  It should be done AFTER xmlns name check
                */
                  name = xsd::QName::createNsN(
                      swizzle_context_namespace(skn,
                          generate_prefix("new", name.getXmlNs()->get_uri())),
                      name.getLocalName());
              }

              /* Swizzle namespace if needed */
              if (skn->resolvePrefix(name.getXmlNs()->get_prefix()) != name.getXmlNs()) {
                  xmlns_ptr newns = swizzle_context_namespace(skn, name.getXmlNs());

                  if (newns != NULL_XMLNS) {
                      parent->addNS(newns);
                      name = xsd::QName::createNsN(newns, name.getLocalName());
                  }
              }
        } while (false);

        if (name.getXmlNs() == NULL_XMLNS && strcmp(name.getLocalName(), "xmlns") == 0) {
          /* It has no namespace prefix and its local name is xmlns, it's an error. */
            throw XQUERY_EXCEPTION(XQDY0044);
        }

        text_source_t value = getTextContent(at_value, content);

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

        text_source_t content_value = getTextContent(at_value, content);
        xsd::AnyURI uri = xsd::AnyURI::check(content_value.u.cstr);
        xmlns_ptr ns = xmlns_touch(at_name, uri.getValue());

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

        text_membuf_t textbuf(getTextContent(at_value, content));

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

void PPPIConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        /* Determine parameter */
        tuple_cell res1;
        res1.set_eos();
        xsd::NCName name;

        do {
            stmt_str_buf res;
            if (at_name == NULL) {
                name = getNCNameParameter(qname, true);
                if (!name.valid()) { break; }
                collapse_string_normalization(name.getValue(), res);
            } else {
                collapse_string_normalization(at_name, res); /* Perform name normalization */
            }

            name = xsd::NCName::check(res.get_tuple_cell().get_str_mem(), true);
        } while (false);

        if (!name.valid()) {
            throw XQUERY_EXCEPTION(XQDY0041);
        }

        if (charset_handler->matches(name.getValue(), "^(?i:xml)$")) {
            throw XQUERY_EXCEPTION(XQDY0064);
        }

        text_membuf_t textbuf(getTextContent(at_value, content));

        /* TRICKY This is just a search for "?>" or "--", that should not be in content (IT) */
        if (strm.parse(textbuf.getCstr(), textbuf.getSize(), NULL, NULL) != 0) {
            throw XQUERY_EXCEPTION(XQDY0026);
        }

        /* Skip heading whitespaces */
        tuple_cell result = constructorContext.getParent(deep_copy)->addPI(name.getValue(), trimLeft(textbuf.getTextSource()));

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
                                     bool _deep_copy,
                                     bool _cdataflag) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                        content(_content_), cdataflag(_cdataflag)
{
    at_value=NULL;
}

PPTextConstructor::PPTextConstructor(dynamic_context *_cxt_,
                                     operation_info _info_,
                                     const char* value,
                                     bool _deep_copy,
                                     bool _cdataflag) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                        cdataflag(_cdataflag)
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
    tuple_cell result = tuple_cell::eos();

    if (first_time) {
        first_time = false;

        if (cdataflag) {
            cdataflag_hint = cdata_section | cdata_infect;
        }

        if (at_value != NULL) {
            result = constructorContext.getParent(deep_copy)->addText(text_source_cstr(at_value));
        } else {
            sequence atomized_content(1);
            getAtomicSequence(NULL, content, &atomized_content);

            if (atomized_content.size() == 0) {
                result = tuple_cell::eos();
            } else {
                concatSequence(&atomized_content);
                result = constructorContext.getParent(deep_copy)->addText(text_source_strbuf(&executor_globals::tmp_op_str_buf));
            }
        }

        cdataflag_hint = cdata_inherit;
    }

    if (result.is_eos()) {
        first_time = true;
        t.set_eos();
    } else {
        t.copy(result);
    }
}

PPIterator* PPTextConstructor::do_copy(dynamic_context *_cxt_)
{
    PPTextConstructor *res;

    if (at_value!=NULL) {
        res = se_new PPTextConstructor(_cxt_, info, at_value, deep_copy, cdataflag);
    } else {
        res = se_new PPTextConstructor(_cxt_, info, content, deep_copy, cdataflag);
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

