/*
 * File:  dm_accessors.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/structures/nodeinterface.h"
#include "tr/structures/nodeutils.h"
#include "tr/structures/xmlns.h"
#include "tr/executor/base/xs_uri.h"
#include "tr/executor/base/xsd.h"

inline static
xptr dm_base_uri_attribute(Node node, dynamic_context *context)
{
    return getNodeAttribute(node.getPtr(), "base", context->get_static_context()->getStaticallyKnownNamespaces()->resolvePrefix("xml"));
}

inline static
tuple_cell get_context_base_uri(dynamic_context *context) {
    const char * context_uri = context->get_static_context()->get_base_uri();
    if (context_uri != NULL) {
        return tuple_cell::atomic_deep(xs_anyURI, context_uri);
    } else {
        return tuple_cell::eos();
    }
}

static
tuple_cell get_base_uri_deep(Node node, dynamic_context *context)
{
    xptr xml_base = XNULL;

    while (!node.isNull()) {
        xml_base = dm_base_uri_attribute(node, context);

        if (xml_base != XNULL) {
            tuple_cell result = dm_string_value(xml_base);
            Uri::Information nfo;
            bool is_relative = Uri::is_relative(&result, &nfo);

            /* I suppose base URI must be stored in normalized form. */
            if(!nfo.normalized) throw XQUERY_EXCEPTION2(SE1003, "Base URI is not properly normalized");

            if (is_relative) {
                 tuple_cell base_uri = dm_base_uri(node.checkp().getParent(), context);

                 if (!base_uri.is_eos()) {
                     stmt_str_buf resolved_uri(1);
                     base_uri = tuple_cell::make_sure_light_atomic(base_uri);
                     result = tuple_cell::make_sure_light_atomic(result);
                     if (Uri::resolve(result.get_str_mem(), base_uri.get_str_mem(), resolved_uri)) {
						 char *resolved_uri_cstr = resolved_uri.get_str();
						 //FIXME: atomic_deep is not really needed here
                         result = tuple_cell::atomic_deep(xs_anyURI, resolved_uri_cstr);
						 delete[] resolved_uri_cstr;
                     }
                 }
            }

            return cast_primitive_to_xs_anyURI(result);
        }

        node = node.checkp().getActualParent();
    }

    return get_context_base_uri(context);
}

tuple_cell dm_base_uri(Node node, dynamic_context *context)
{
    if (node.isNull()) {
        return get_context_base_uri(context);
    }

    node.checkp();

    /* Works as defined in http://www.w3.org/TR/xpath-datamodel/#acc-summ-base-uri */
    switch (node.getNodeType()) {
        case document :
        case element :
            return get_base_uri_deep(node, context);
        case virtual_root :
            return get_context_base_uri(context);
        case attribute :
        case pr_ins :
            /* SHOULD BE Empty, as defined in http://www.w3.org/TR/xquery/#id-computed-pis */
        case comment :
        case text : {
            Node parent = node.getActualParent();
            if (!parent.isNull()) {
                return dm_base_uri(parent, context);
            }
        } break;
        case xml_namespace :
            break;
        default : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:base-uri");
    }

    return tuple_cell::eos();
}

static
tuple_cell get_pi_name(PINode pi)
{
    size_t target_size = pi.getPITargetSize();
    char * t = new char[target_size + 1];
    tuple_cell qname = tuple_cell::atomic(xs_string, t);

    t[target_size] = '\0';
    pi.copyToBuffer(t, 0, target_size);

    return qname;
}

tuple_cell dm_node_name(Node node)
{
    node.checkp();

    switch (node.getNodeType()) {
        case comment :
        case text :
        case document :
            return tuple_cell::eos();
        case element :
        case attribute : {
            schema_node_cptr snode = node.getSchemaNode();
            return tuple_cell::atomic(xsd::QName::createNsN(snode->get_xmlns(), snode->get_name()));
        }
        case xml_namespace : {
            xmlns_ptr ns = NSNode(node).getNamespaceLocal();
            if (ns->prefix != NULL) {
                return tuple_cell::atomic(xsd::QName::createNsN(NULL_XMLNS, ns->prefix));
            } else {
                return tuple_cell::eos();
            }
        }
        case pr_ins :
            return tuple_cell::atomic(xsd::QName::createNsN(NULL_XMLNS, get_pi_name(node).get_str_mem()));
        default : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:node-name");
    }
}

tuple_cell se_node_local_name(Node node)
{
    node.checkp();

    switch (node.getNodeType()) {
        case comment :
        case text :
        case document :
            return tuple_cell::eos();
        case element :
        case attribute :
            return tuple_cell::atomic_deep(xs_NCName, node.getSchemaNode()->get_name());
        case xml_namespace : {
            xmlns_ptr ns = NSNode(node).getNamespaceLocal();
            if (ns->prefix != NULL) {
                return tuple_cell::atomic_deep(xs_NCName, ns->prefix);
            } else {
                return tuple_cell::eos();
            }
        }
        case pr_ins :
            return tuple_cell::atomic(xs_NCName, get_pi_name(node).get_str_mem());
        default : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to se_node_local_name");
    }
}


tuple_cell se_node_namespace_uri(Node node)
{
    node.checkp();

    switch (node.getNodeType()) {
        case xml_namespace :
        case pr_ins :
        case comment :
        case text :
        case document :
            return tuple_cell::eos();
        case element :
        case attribute : {
            xmlns_ptr xmlns = node.getSchemaNode()->get_xmlns();
            if (xmlns != NULL_XMLNS) {
                U_ASSERT(xmlns->uri != NULL);
                return tuple_cell::atomic_deep(xs_anyURI, xmlns->uri);
            } else {
                return tuple_cell::eos();
            }
        }
        default : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:node-name");
    }
}

tuple_cell dm_parent(Node node)
{
    Node parent = node.getActualParent();
    return parent.isNull() ? tuple_cell::eos() : tuple_cell::node(parent);
}

/*******************************************************************************
 * dm:string-value works as follows. The result is a atomic value, which type
 * is xs:string. The value is constructed from content of the node.
 * if node is a text node or a attribute node, then value is the value of a
 * node. The resulting tuple represents descriptor of the resulting atomic
 * value. Data remains the same.
 *
 * In the case of element the result value is the concatenation of all
 * descendant _text_ nodes. New data constructed only if concatenation is
 * needed (when element has several descendant text nodes).
 ******************************************************************************/

enum dm_string_value_result_type {dsvrt_empty, dsvrt_pstr_both, dsvrt_e_str};

// FIXME: rewrite this code with stmt_str_buf when it appears
struct dm_string_value_result
{
    dm_string_value_result_type type;
    estr_buf buf;
    xptr p;
    int size;

    void init()
    {
        type = dsvrt_empty;
        buf.reinit();
        p = XNULL;
        size = 0;
    }
};

static
dm_string_value_result dsvr;

inline static
void dm_string_value_traverse(Node node)
{
    node.checkp();

    switch (node.getNodeType()) {
        case document :
        case element : {
            xptr p = getFirstChild(node.getPtr());

            while (p != XNULL) {
                dm_string_value_traverse(p);

                CHECKP(p);
                p = nodeGetRightSibling(p);
            }
            return;
        }
        case text: {
            TextNode text(node);
            strsize_t size = text.getTextSize();
            xptr data = text.getTextPointer();
            text.checkp();

            switch (dsvr.type) {
                case dsvrt_empty    : dsvr.type = dsvrt_pstr_both;
                                      dsvr.p = data;
                                      dsvr.size = size;
                                      break;
                case dsvrt_pstr_both: dsvr.type = dsvrt_e_str;
                                      dsvr.buf.append_pstr(dsvr.p, dsvr.size);
                                      dsvr.buf.append_pstr(data, size);
                                      break;
                case dsvrt_e_str    : dsvr.buf.append_pstr(data, size);
                                      break;
                default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of dsvr passed to dm_string_value_traverse");
            }
        }
        default: break;
    }
}

inline static
tuple_cell dm_string_value_call_traverse(Node node)
{
    dsvr.init();
    dm_string_value_traverse(node.getPtr());
    switch (dsvr.type)
    {
        case dsvrt_empty    : return EMPTY_STRING_TC;
        case dsvrt_pstr_both: return tuple_cell::atomic_pstr(xs_string, dsvr.size, dsvr.p);
        case dsvrt_e_str    : return dsvr.buf.content();
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of dsvr passed to dm:string-value");
    }
}

tuple_cell dm_string_value(Node node)
{
    node.checkp();

    switch (node.getNodeType()) {
        case element: {
            xmlscm_type type = ElementNode(node).getType();

            if (type == xs_untyped || type == xs_anyType) {
                return dm_string_value_call_traverse(node);
            } else {
                xptr p = getFirstChild(node.getPtr());
                if (p == XNULL)
                    return EMPTY_STRING_TC;
                else
                    return dm_string_value(p);
            }
        }
        case document: {
            return dm_string_value_call_traverse(node);
        }
        case text:
        case comment:
        case attribute: {
            return tuple_cell::atomic_text(CommonTextNode(node));
        }
        case xml_namespace: {
            return tuple_cell::atomic_deep(xs_string, NSNode(node).getNamespaceLocal()->uri);
        }
        case pr_ins: {
            return tuple_cell::atomic_pi(PINode(node));
        }
        default:
            throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:string-value");
    }
}

/*******************************************************************************
 * dm:typed-value works as follows. The result is a atomic value or nothing(?).
 * If the result is value, then s_dsc is constructed.
 * If kind of a node is element then its type is considered. If element has
 * SimpleType, then the resulting atomic value has the same type, else type
 * is untypedAtomic. Typed value of a attribute node is the same
 * as node has.
 ******************************************************************************/
tuple_cell dm_typed_value(Node node)
{
    node.checkp();

    switch (node.getNodeType()) {
        case document :
            return cast(dm_string_value(node), xs_untypedAtomic);
        case element : {
            xmlscm_type type = ElementNode(node).getType();

            if (type == xs_untyped || type == xs_anyType) {
                tuple_cell res = dm_string_value(node);
                res.set_xtype(xs_untypedAtomic);
                return res;
            } else {
                return cast(dm_string_value(node), type);
            }
        }
        case attribute : {
            xmlscm_type type = AttributeNode(node).getType();
            return cast(dm_string_value(node), type);
        }
        case xml_namespace  :
        case pr_ins :
        case comment :
            return dm_string_value(node);
        case text : {
            tuple_cell res = dm_string_value(node);
            res.set_xtype(xs_untypedAtomic);
            return res;
        }
        default :
            throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:typed-value");
    }
}

const char* xmlscm_type2c_str(xmlscm_type type)
{
    switch (type)
    {
        // Abstract base types
        case xs_anyType             : return "xs:anyType";
        case xs_anySimpleType       : return "xs:anySimpleType";
        case xs_anyAtomicType       : return "xs:anyAtomicType";

        // Built-in simple, non-atomic types
        case xs_IDREFS              : return "xs:IDREFS";
        case xs_NMTOKENS            : return "xs:NMTOKENS";
        case xs_ENTITIES            : return "xs:ENTITIES";

        // Built-in complex types
        case xs_untyped             : return "xs:untyped";

        // Built-in atomic types (Primitive types)
        case xs_untypedAtomic       : return "xs:untypedAtomic";
        case xs_dateTime            : return "xs:dateTime";
        case xs_date                : return "xs:date";
        case xs_time                : return "xs:time";
        case xs_duration            : return "xs:duration";
        case xs_yearMonthDuration   : return "xs:yearMonthDuration";
        case xs_dayTimeDuration     : return "xs:dayTimeDuration";
        case xs_float               : return "xs:float";
        case xs_double              : return "xs:double";
        case xs_string              : return "xs:string";
        case xs_decimal             : return "xs:decimal";
        case xs_integer             : return "xs:integer";
        case xs_gYearMonth          : return "xs:gYearMonth";
        case xs_gYear               : return "xs:gYear";
        case xs_gMonthDay           : return "xs:gMonthDay";
        case xs_gDay                : return "xs:gDay";
        case xs_gMonth              : return "xs:gMonth";
        case xs_boolean             : return "xs:boolean";
        case xs_base64Binary        : return "xs:base64Binary";
        case xs_hexBinary           : return "xs:hexBinary";
        case xs_anyURI              : return "xs:anyURI";
        case xs_QName               : return "xs:QName";
        case xs_NOTATION            : return "xs:NOTATION";

        // Special Sedna type
        case se_separator           : return "se:separator";

        // Types derived from xs:string
        case xs_normalizedString    : return "xs:normalizedString";
        case xs_token               : return "xs:token";
        case xs_language            : return "xs:language";
        case xs_NMTOKEN             : return "xs:NMTOKEN";
        case xs_Name                : return "xs:Name";
        case xs_NCName              : return "xs:NCName";
        case xs_ID                  : return "xs:ID";
        case xs_IDREF               : return "xs:IDREF";
        case xs_ENTITY              : return "xs:ENTITY";

        // Types derived from xs:integer
        case xs_nonPositiveInteger  : return "xs:nonPositiveInteger";
        case xs_negativeInteger     : return "xs:negativeInteger";
        case xs_long                : return "xs:long";
        case xs_int                 : return "xs:int";
        case xs_short               : return "xs:short";
        case xs_byte                : return "xs:byte";
        case xs_nonNegativeInteger  : return "xs:nonNegativeInteger";
        case xs_unsignedLong        : return "xs:unsignedLong";
        case xs_unsignedInt         : return "xs:unsignedInt";
        case xs_unsignedShort       : return "xs:unsignedShort";
        case xs_unsignedByte        : return "xs:unsignedByte";
        case xs_positiveInteger     : return "xs:positiveInteger";

        default                     : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema type passed to dm:type");
    }
}

tuple_cell dm_nilled(Node node)
{
    node.checkp();

    switch (node.getNodeType()) {
        case document :
        case attribute :
        case xml_namespace :
        case pr_ins :
        case comment :
        case text :
            return tuple_cell::eos();
        case element :
            return tuple_cell::atomic(false);
        default :
            throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:nilled");
    }
}

tuple_cell dm_document_uri(Node node)
{
    node.checkp();

    switch (node.getNodeType()) {
        case element :
        case attribute :
        case xml_namespace :
        case pr_ins :
        case comment :
        case text :
            return tuple_cell::eos();
        case document : {
            CommonTextNode doc = node;
            U_ASSERT(!doc.isPstrLong());
            strsize_t size = doc.getTextSize();
            doc.checkp();
            if (size == 0) {
                return tuple_cell::eos();
            } else {
                char *t = new char[size + 1];
                t[size] = '\0';
                doc.copyToBuffer(t, 0, size);
                return tuple_cell::atomic(xs_anyURI, t);
            }
        }
        default :
            throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:document-uri");
    }
}

typedef std::map<std::string,xmlns_ptr> nms_map;

static
void se_get_in_scope_namespaces(Node node, std::vector<xmlns_ptr> &result, dynamic_context *cxt)
{
    /* As soon as child namespaces override parent ones, we should at first collect all
      namespaces and then apply them */

    /* First part --- collection  */

    std::stack<xmlns_ptr> nsStack;
    std::stack<xmlns_ptr> attributeNsStack;

    while (!node.isNull())
    {
        node.checkp();
        schema_node_cptr scm = node.getSchemaNode();

        //0. self namespace
        if (scm->get_xmlns() != NULL_XMLNS) {
            nsStack.push(scm->get_xmlns());
        }

        //1. namespace nodes
        Node attr = getFirstChildByType(node.getPtr(), xml_namespace);
        while (!attr.isNull())
        {
            nsStack.push(NSNode(attr.checkp()).getNamespaceLocal());
            attr = attr.getNextSameSort();
        }

        //3. attributes
        attr = getFirstChildByType(node.getPtr(), attribute);

        //3.1 filling set
        while (!attr.isNull())
        {
            attributeNsStack.push(attr.checkp().getSchemaNode()->get_xmlns());
            attr = getNextAttribute(attr.getPtr());
        }

        node = node.getActualParent();
    }

    /* Second part --- application */

    nms_map mp;
    mp["xml"] = cxt->get_static_context()->getStaticallyKnownNamespaces()->resolvePrefix("xml");

    while (!nsStack.empty())
    {
        xmlns_ptr ns = nsStack.top();

        U_ASSERT(ns->get_prefix() != NULL);

        if (!ns->empty_uri()) {
            mp[ns->get_prefix()] = ns;
        } else {
            mp.erase(ns->get_prefix());
        }

        nsStack.pop();
    }

    int ctr = 0;

    while (!attributeNsStack.empty())
    {
        do {
            xmlns_ptr ns = attributeNsStack.top();

            U_ASSERT(ns->has_prefix());
            U_ASSERT(!ns->empty_uri());

            while (mp.find(ns->get_prefix()) != mp.end()) {
                if (mp[ns->get_prefix()] == ns) {
                    break;
                }

                ns = generate_prefix(ctr++, ns->get_uri(), cxt);
            };

            mp[ns->get_prefix()] = ns;
        } while (false);

        attributeNsStack.pop();
    }

    nms_map::const_iterator it = mp.begin();

    while (it != mp.end())
    {
        result.push_back(it->second);
        ++it;
    }
}

tuple_cell se_node_local_name(xptr node)
{
    CHECKP(node);

    switch (getNodeType(node)) {
        case comment        :
        case text           :
        case document       : return tuple_cell::eos();

        case element        :
        case attribute      : return tuple_cell::atomic_deep(xs_NCName, getSchemaNode(node)->get_name());

        case xml_namespace  : {
                                  const NSNode nsn(node);

                                  if (nsn.isNullPrefix()) {
                                      return tuple_cell::eos();
                                  } else {
                                      return tuple_cell::atomic_deep(xs_NCName, nsn.getNamespaceLocal()->prefix);
                                  }
                              }

        case pr_ins         : {
                                  const PINode pin(node);
                                  const size_t target = pin.getPITargetSize();
                                  char * t = new char[target + 1];
                                  t[target] = '\0';
                                  pin.copyToBuffer(t, 0, target);
                                  return tuple_cell::atomic(xs_NCName, t);
                              }

        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to se_node_local_name");
    }
}

tuple_cell se_node_namespace_uri(xptr node)
{
    CHECKP(node);

    switch (getNodeType(node)) {
        case xml_namespace  :
        case pr_ins         :
        case comment        :
        case text           :
        case document       : return tuple_cell::eos();

        case element        :
        case attribute      : {
                                  xmlns_ptr xmlns = getSchemaNode(node)->get_xmlns();
                                  if (xmlns != NULL_XMLNS) {
                                      U_ASSERT(xmlns->uri);
                                      return tuple_cell::atomic_deep(xs_anyURI, xmlns->uri);
                                  }
                                  return tuple_cell::eos();
                              }
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:node-name");
    }
}

