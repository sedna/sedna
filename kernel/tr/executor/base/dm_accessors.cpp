/*
 * File:  dm_accessors.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/strings/e_string.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/pstr/pstr.h"
#include "common/errdbg/d_printf.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/xs_uri.h"

tuple_cell dm_base_uri(xptr node, dynamic_context *cxt)
{
    CHECKP(node);

    t_item node_type = GETSCHEMENODE(XADDR(node))->type;
    if (node_type == xml_namespace)
        return tuple_cell::eos();

    if (node_type == attribute || node_type == pr_ins || node_type == comment || node_type == text)
    {
        xptr parent = GETPARENTPOINTER(node);
        if (parent == XNULL)
            return tuple_cell::eos();

        CHECKP(parent);
        node = *((xptr*)XADDR(parent));

        CHECKP(node);
        if (GETSCHEMENODE(XADDR(node))->type != document || GETSCHEMENODE(XADDR(node))->type != element)
            return tuple_cell::eos();
    }

    xptr xml_base_node = getBaseUri(node);

    if (xml_base_node != XNULL)
    {
        tuple_cell tc = dm_string_value(xml_base_node);
        Uri::Information nfo;
        bool is_relative = Uri::is_relative(&tc, &nfo);

        /// I suppose base usi must be stored in normalized form. (IS)
        if(!nfo.normalized) throw XQUERY_EXCEPTION2(SE1003, "Base URI is not properly normalized");

        /// If URI is relative and static base uri is defined we should perform resolving. (IS)
        if(is_relative && cxt->st_cxt->get_base_uri())
        {
             stmt_str_buf result(1);
             tc = tuple_cell::make_sure_light_atomic(tc);
             if(!Uri::resolve(tc.get_str_mem(), cxt->st_cxt->get_base_uri(), result))
                 return cast_primitive_to_xs_anyURI(tc);
             return tuple_cell::atomic(xs_anyURI, result.get_str());
        }
        else
            return cast_primitive_to_xs_anyURI(tc);
    }

    /* xml_base_node == NULL here */
    if (IS_TMP_BLOCK(node) && cxt->st_cxt->get_base_uri())
        return tuple_cell::atomic_deep(xs_anyURI, cxt->st_cxt->get_base_uri());

    return tuple_cell::eos();
}

tuple_cell dm_node_name(xptr node)
{
    CHECKP(node);

    switch (GETSCHEMENODE(XADDR(node))->type)
    {
        case document       : return tuple_cell::eos();
        case element        :
        case attribute      : {
                                  xmlns_ptr xmlns = GETSCHEMENODE(XADDR(node))->get_xmlns();
                                  const char *n = GETSCHEMENODE(XADDR(node))->name;
                                  char *qname = xs_QName_create(xmlns, n, malloc);
                                  return tuple_cell::atomic(xs_QName, qname);
                              }
        case xml_namespace  : {
                                  ns_dsc *ns = NS_DSC(node);
                                  if (ns->ns->prefix)
                                  {
                                      char *qname = xs_QName_create(NULL_XMLNS, ns->ns->prefix, malloc);
                                      return tuple_cell::atomic(xs_QName, qname);
                                  }
                                  else
                                      return tuple_cell::eos();
                              }
        case pr_ins         : {
                                  pi_dsc *pi = PI_DSC(node);
                                  shft target = pi->target;
                                  xptr data = getTextPtr(pi);

                                  char *qname;

                                  CHECKP(node);
                                  if (isPstr(pi)) {
                                      CHECKP(data);
                                      char *t = se_new char[target + 1];
                                      t[target] = '\0';
                                      estr_copy_to_buffer(t, data, target);
                                      qname = xs_QName_create(NULL_XMLNS, t, malloc);
                                      delete [] t;
                                  } else {
                                      qname = xs_QName_create(NULL_XMLNS, (char *) XADDR(data), malloc);
                                  }

                                  return tuple_cell::atomic(xs_QName, qname);
                              }
        case comment        : return tuple_cell::eos();
        case text           : return tuple_cell::eos();
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:node-name");
    }
}

tuple_cell se_node_local_name(xptr node)
{
    CHECKP(node);

    switch (GETSCHEMENODE(XADDR(node))->type)
    {
        case document       : return tuple_cell::eos();
        case element        :
        case attribute      : {
                                  const char *local_name = GETSCHEMENODE(XADDR(node))->name;
                                  return tuple_cell::atomic_deep(xs_NCName, local_name);
                              }
        case xml_namespace  : {
                                  ns_dsc *ns = NS_DSC(node);
                                  if (ns->ns->prefix)
                                  {
                                      return tuple_cell::atomic_deep(xs_NCName, ns->ns->prefix);
                                  }
                                  else
                                      return tuple_cell::eos();
                              }
        case pr_ins         : {
                                  pi_dsc *pi = PI_DSC(node);
                                  shft target = pi->target;
                                  xptr data = getTextPtr(pi);

                                  char *t = se_new char[target + 1];
                                  t[target] = '\0';

                                  CHECKP(node);

                                  if (isPstr(pi)) {
                                      estr_copy_to_buffer(t, data, target);
                                  } else {
                                      memcpy(t, (char *) XADDR(data), target);
                                  }

                                  return tuple_cell::atomic(xs_NCName, t);
                              }
        case comment        : return tuple_cell::eos();
        case text           : return tuple_cell::eos();
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to se_node_local_name");
    }
}

tuple_cell se_node_namespace_uri(xptr node)
{
    CHECKP(node);

    switch (GETSCHEMENODE(XADDR(node))->type)
    {
        case document       : return tuple_cell::eos();
        case element        :
        case attribute      : {
                                  xmlns_ptr xmlns = GETSCHEMENODE(XADDR(node))->get_xmlns();
                                  if (xmlns != NULL_XMLNS) {
                                    U_ASSERT(xmlns->uri);
                                    return tuple_cell::atomic_deep(xs_anyURI, xmlns->uri);
                                  }
                                  return tuple_cell::eos();
                              }
        case xml_namespace  :
        case pr_ins         :
        case comment        :
        case text           : return tuple_cell::eos();
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:node-name");
    }
}

xptr get_parent_node(xptr node)
{
    CHECKP(node);

    xptr p = GETPARENTPOINTER(node);

    if (p == XNULL) return p;
    else
    {
        CHECKP(p);
        return *(xptr*)XADDR(p);
    }
}

tuple_cell dm_parent(xptr node)
{
    xptr p = get_parent_node(node);
    return (p == XNULL) ? tuple_cell::eos() : tuple_cell::node(p);
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

static dm_string_value_result dsvr;

void dm_string_value_traverse(xptr node)
{
    CHECKP(node);
    xmlscm_type node_type = GETSCHEMENODE(XADDR(node))->type;

    switch (node_type)
    {
        case document:
        case element:   {
                            xptr p = first_child(node);

                            while (p != XNULL)
                            {
                                dm_string_value_traverse(p);

                                CHECKP(p);
                                p = GETRIGHTPOINTER(p);
                            }

                            return;
                        }
        case text:      {
                            strsize_t size = getTextSize(T_DSC(node));
                            xptr data = getTextPtr(T_DSC(node));

                            switch (dsvr.type)
                            {
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
    }
}

inline tuple_cell dm_string_value_call_traverse(xptr node)
{
    // it is assumed that CHECKP is already called on node
    dsvr.init();
    dm_string_value_traverse(node);
    switch (dsvr.type)
    {
        case dsvrt_empty    : return EMPTY_STRING_TC;
        case dsvrt_pstr_both: return tuple_cell::atomic_pstr(xs_string, dsvr.size, dsvr.p);
        case dsvrt_e_str    : return dsvr.buf.content();
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of dsvr passed to dm:string-value");
    }
}

tuple_cell dm_string_value(xptr node)
{
    CHECKP(node);

    switch (GETSCHEMENODE(XADDR(node))->type) {
        case element: {
            xmlscm_type type = E_DSC(node)->type;

            if (type == xs_untyped || type == xs_anyType) {
                return dm_string_value_call_traverse(node);
            } else {
                xptr p = first_child(node);
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
            return tuple_cell::atomic_text(node);
        }
        case xml_namespace: {
            return tuple_cell::atomic_deep(xs_string, NS_DSC(node)->ns->uri);
        }
        case pr_ins: {
            return tuple_cell::atomic_pi(node);
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
tuple_cell dm_typed_value(xptr node)
{
    CHECKP(node);

    switch (GETSCHEMENODE(XADDR(node))->type)
    {
        case document       : return cast(dm_string_value(node), xs_untypedAtomic);
        case element        : {
                                  xmlscm_type type = E_DSC(node)->type;
                                  if (type == xs_untyped || type == xs_anyType)
                                  {
                                      tuple_cell res = dm_string_value(node);
                                      res.set_xtype(xs_untypedAtomic);
                                      return res;
                                  }
                                  else
                                      return cast(dm_string_value(node), type);
                              }
        case attribute      : {
                                  xmlscm_type type = A_DSC(node)->type;
                                  return cast(dm_string_value(node), type);
                              }
        case xml_namespace  : return dm_string_value(node);
        case pr_ins         : return dm_string_value(node);
        case comment        : return dm_string_value(node);
        case text           : {
                                  tuple_cell res = dm_string_value(node);
                                  res.set_xtype(xs_untypedAtomic);
                                  return res;
                              }
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:typed-value");
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
/*
tuple_cell dm_type_name(xptr node)
{
    CHECKP(node);

    switch (GETSCHEMENODE(XADDR(node))->type)
    {
        case document       : return tuple_cell::eos();
        case element        : return tuple_cell::atomic_deep(xs_QName, xmlscm_type2c_str(E_DSC(node)->type));
        case attribute      : return tuple_cell::atomic_deep(xs_QName, xmlscm_type2c_str(A_DSC(node)->type));
        case xml_namespace  : return tuple_cell::eos();
        case pr_ins         : return tuple_cell::eos();
        case comment        : return tuple_cell::eos();
        case text           : return tuple_cell::atomic_xs_QName_deep("xs", "untypedAtomic");
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:type-name");
    }
}
*/
tuple_cell dm_nilled(xptr node)
{
    CHECKP(node);

    switch (GETSCHEMENODE(XADDR(node))->type)
    {
        case document       : return tuple_cell::eos();
        case element        : return tuple_cell::atomic(false);
        case attribute      : return tuple_cell::eos();
        case xml_namespace  : return tuple_cell::eos();
        case pr_ins         : return tuple_cell::eos();
        case comment        : return tuple_cell::eos();
        case text           : return tuple_cell::eos();
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:nilled");
    }
}

tuple_cell dm_document_uri(xptr node)
{
    CHECKP(node);

    switch (GETSCHEMENODE(XADDR(node))->type)
    {
        case document       : {
                                  d_dsc *d = D_DSC(node);
                                  int size = getTextSize(d);
                                  if (size == 0) return tuple_cell::eos();
                                  xptr data = getTextPtr(d);
                                  char *t = se_new char[size + 1];
                                  t[size] = '\0';
                                  estr_copy_to_buffer(t, data, size);
                                  return tuple_cell::atomic(xs_anyURI, t);
                              }
        case element        : return tuple_cell::eos();
        case attribute      : return tuple_cell::eos();
        case xml_namespace  : return tuple_cell::eos();
        case pr_ins         : return tuple_cell::eos();
        case comment        : return tuple_cell::eos();
        case text           : return tuple_cell::eos();
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:nilled");
    }
}

dm_node_kind_type dm_node_kind(xptr node)
{
    CHECKP(node);

    switch (GETSCHEMENODE(XADDR(node))->type)
    {
        case element    : return nk_element;
        case text       : return nk_text;
        case attribute  : return nk_attribute;
        case document   : return nk_document;
        default         : throw USER_EXCEPTION2(SE1003, "Unexpected type of node passed to dm:node-kind");
    }
}

