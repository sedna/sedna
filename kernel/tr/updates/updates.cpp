/*
 * File:  updates.cpp
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "common/sedna.h"
#include "common/utils.h"

#include "tr/updates/updates.h"
#include "tr/mo/mo.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/nid/numb_scheme.h"
#include "tr/executor/xqops/PPConstructors.h"
#include "tr/mo/indirection.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers.h"
#endif

#include "tr/structures/nodeutils.h"

#define IGNORE_UPDATE_ERRORS

#ifdef SE_ENABLE_FTSEARCH
std::map<ft_index_cell_xptr,update_history*> ft_updates;
#endif

#ifdef SE_ENABLE_FTSEARCH
void clear_ft_sequences()
{
    std::map<ft_index_cell_xptr,update_history*>::iterator it=ft_updates.begin();
    while (it!=ft_updates.end())
    {
        delete it->second;
        it++;
    }
    ft_updates.clear();
}
void execute_modifications()
{
	//XXX: what indirectionGetRollbackMode() returns when called from rcvRecoverFtIndexes?
    if (indirectionGetRollbackMode()) return;
    std::map<ft_index_cell_xptr,update_history*>::iterator it=ft_updates.begin();
    while (it!=ft_updates.end())
    {
        it->first->update_index(it->second);
        it++;
    }

	clear_ft_sequences();
}
void update_insert_sequence(xptr node,ft_index_cell_cptr icell)
{
    std::map<ft_index_cell_xptr,update_history*>::iterator it=ft_updates.find(icell.ptr());
    if (it==ft_updates.end())
    {
        update_history *h=se_new update_history();
		h->add_insert_node(node);
        ft_updates[icell.ptr()]=h;
    }
    else
        it->second->add_insert_node(node);

}
void update_update_sequence(xptr node,ft_index_cell_cptr icell)
{
    std::map<ft_index_cell_xptr,update_history*>::iterator it=ft_updates.find(icell.ptr());
    if (it==ft_updates.end())
    {
        update_history *h=se_new update_history();
        h->add_update_node(node);
        ft_updates[icell.ptr()]=h;
    }
    else
        it->second->add_update_node(node);
}
void update_delete_sequence(xptr node,ft_index_cell_cptr icell)
{
    std::map<ft_index_cell_xptr,update_history*>::iterator it=ft_updates.find(icell.ptr());
    if (it==ft_updates.end())
    {
        update_history* h=se_new update_history();
        h->add_delete_node(node);
        ft_updates[icell.ptr()]=h;
    }
    else
        it->second->add_delete_node(node);
}

void update_insert_sequence(xptr node,schema_node_cptr icell)
{
    cat_list<ft_index_cell_xptr>::item* obj=icell->ft_index_list->first;
    if (obj == NULL) return;
    CHECKP(node);
    xptr ind=nodeGetIndirection(node);
    while (obj!=NULL)
    {
        update_insert_sequence(ind,ft_index_cell_cptr(obj->object));
        obj=obj->next;
    }
    CHECKP(node);
}

void update_update_sequence(xptr node,schema_node_cptr icell)
{
    cat_list<ft_index_cell_xptr>::item* obj=icell->ft_index_list->first;
    if (obj==NULL) return;
    CHECKP(node);
    xptr ind=nodeGetIndirection(node);
    while (obj!=NULL)
    {
        update_update_sequence(ind,ft_index_cell_cptr(obj->object));
        obj=obj->next;
    }
    CHECKP(node);
}

void update_delete_sequence(xptr node,schema_node_cptr icell)
{
    cat_list<ft_index_cell_xptr>::item* obj=icell->ft_index_list->first;
    if (obj==NULL) return;
    CHECKP(node);
    xptr ind=nodeGetIndirection(node);
    while (obj!=NULL)
    {
        update_delete_sequence(ind,ft_index_cell_cptr(obj->object));
        obj=obj->next;
    }
    CHECKP(node);
}

void init_ft_sequences (const xptr& left, const xptr& right, const xptr& parent)
{
    if (IS_TMP_BLOCK(left)||IS_TMP_BLOCK(right)||IS_TMP_BLOCK(parent)) return;
    xptr tmp;
    schema_node_cptr scn = XNULL;
    if (parent!=XNULL)
    {
        tmp=parent;
        CHECKP(parent);
        scn = getSchemaNode(parent);
    }
    else
    {
        tmp=(left==XNULL)?right:left;
        CHECKP(tmp);
        scn = getSchemaNode(tmp)->parent;
    }

    if (scn->root == scn.ptr() || scn->root->full_ft_index_list->empty()) return;

    while (scn.found())
    {
        cat_list<ft_index_cell_xptr>::item* obj=scn->ft_index_list->first;
        if (obj!=NULL)
        {
            tmp=getNodeAncestorIndirectionByScheme(tmp,scn);
            while (obj!=NULL)
            {
                update_update_sequence(tmp,ft_index_cell_cptr(obj->object));
                obj=obj->next;
            }
            tmp=indirectionDereferenceCP(tmp);
        }
        scn=scn->parent;
    }
}
#endif

xptr copy_node_content(xptr new_node_i, xptr node, xptr left_node_i, upd_ns_map** nsupdmap, bool save_types, unsigned short depth) {
    xptr left_node = XNULL;
    xptr childi = getIndirectionSafeCP(getFirstChild(checkp(node)));

    while (childi != XNULL) {
        left_node = deep_copy_node_i(left_node_i, XNULL, new_node_i, indirectionDereferenceCP(childi), nsupdmap, save_types, depth + 1);

        /* due to : MG: deep_temp_copy can return XNULL if a trigger canceled
        * the insertion and there were now any left sibling */
        if (left_node != XNULL) {
            left_node_i = getIndirectionSafeCP(left_node);
        }

        childi = nodeiGetRightSiblingIndirection(childi);
    }

    return left_node_i;
}

void replaceNamespace(xmlns_ptr & ns, upd_ns_map*& updmap)
{
    if (updmap == NULL) updmap = se_new upd_ns_map;
    upd_ns_map::const_iterator it = updmap->find(ns);
    if (it == updmap->end()) {
        ns = xmlns_touch(ns->prefix, ns->uri);
    } else {
        ns = it->second;
    }
}

/*
  Creates and returns new namespace if {new_ns} is ambigious
  in context of namespaces of {node}.
*/
xmlns_ptr swizzle_namespace(xptr node, xmlns_ptr new_ns) {
/*
    if (new_ns == NULL_XMLNS && getSchemaNode(node)->get_xmlns() != NULL_XMLNS) {
          return swizzle_namespace(node, generate_prefix("new", new_ns->get_uri()));
    }
*/

    NSNode ns = getFirstChildByType(node, xml_namespace);

    if (new_ns->is_reserved_prefix()) {
        return NULL_XMLNS;
    }

    /* If attribute has default namespace and it differs from the
      namespace of parent node, replace it anyway */
    if (!new_ns->has_prefix()) {
        const xmlns_ptr cns = getSchemaNode(node)->get_xmlns();
        if (cns != new_ns) {
            return swizzle_namespace(node, generate_prefix("new", new_ns->get_uri()));
        } else {
            return NULL_XMLNS;
        }
    }

    while (!ns.isNull()) {
        ns.checkp();
        const xmlns_ptr cns = ns.getNamespaceLocal();

        if (cns->get_prefix() == new_ns->get_prefix()) {
            if (cns == new_ns) {
                return NULL_XMLNS;
            } else {
                return swizzle_namespace(node, generate_prefix(new_ns->get_prefix(), new_ns->get_uri()));
            }
        }
        ns = ns.getNext();
    }

    return new_ns;
}

xptr deep_copy_node(xptr left, xptr right, xptr parent, xptr node, upd_ns_map** nsupdmap, bool save_types, unsigned short depth)
{
    xptr result;
    xptr node_indir;
    schema_node_cptr scmnode;
    xmlscm_type scm_type;

    /* Rollback transaction if timeout value's been exceeded */
    CHECK_TIMER_FLAG

#ifdef SE_ENABLE_FTSEARCH
    if (!depth) init_ft_sequences(left,right,parent);
#endif
#ifdef SE_ENABLE_TRIGGERS
    if (parent == XNULL) {
        if (left != XNULL) {
            CHECKP(left);
            parent = nodeGetParent(left);
        } else {
            CHECKP(right);
            parent = nodeGetParent(right);
        }
    }

    node = apply_per_node_triggers(node, XNULL, parent, XNULL, TRIGGER_BEFORE,  TRIGGER_INSERT_EVENT);

    if (node == XNULL) {
        if (left == XNULL) { return XNULL; }
        CHECKP(left);
        return left;
    }
#endif

    node_indir = getIndirectionSafeCP(node);
    scmnode = getSchemaNode(node);

    switch (scmnode->type) {
        case element: {
            xptr result_indir;
            xmlns_ptr ns = scmnode->get_xmlns();

            if (nsupdmap != NULL && ns != NULL_XMLNS) {
                replaceNamespace(ns, *nsupdmap);
            }

            scm_type = (save_types) ? getScmType(node) : xs_untyped;
            result = insert_element(left, right, parent, scmnode->name, scm_type, ns);
            result_indir = get_last_mo_inderection();

            copy_node_content(result_indir, indirectionDereferenceCP(node_indir), XNULL, nsupdmap, save_types, depth);

            result = indirectionDereferenceCP(result_indir);
            CHECKP(result);
                      }
                      break;

        case text: {
            if (CommonTextNode(node).isEmpty()) {
                if (IS_DATA_BLOCK(node)) {
                    throw SYSTEM_EXCEPTION("BAD DATA!!!");
                } else {
                    return left;
                }
            } else {
                result = insert_text(left, right, parent, text_source_node(node));
            }
                   }
                   break;

        case comment: {
            text_membuf_t buf(text_source_node(node));
            result = insert_comment(left, right, parent, buf.getCstr(), buf.getSize());
                      }
                      break;

        case pr_ins: {
            size_t tsize = PINode(node).getPITargetSize();
            text_membuf_t buf(text_source_node(node));
            result = insert_pi(left, right, parent, buf.getCstr(), tsize, buf.getCstr() + tsize + 1, buf.getSize() - tsize - 1);
        }
        break;

        case attribute: {
            xmlns_ptr ns = scmnode->get_xmlns();
            text_membuf_t buf(text_source_node(node));

            if (nsupdmap != NULL && ns != NULL_XMLNS) {
                replaceNamespace(ns, *nsupdmap);
            };

            if (depth == 0 && ns != NULL_XMLNS) {
                const xmlns_ptr new_ns = swizzle_namespace(parent, ns);
                if (new_ns != NULL_XMLNS) {
                    insert_namespace(left, right, parent, new_ns);
                    ns = new_ns;
                }
            }

            CHECKP(node);
            scm_type = (save_types) ? AttributeNode(node).getType() : xs_untypedAtomic;
            result = insert_attribute(left, right, parent, scmnode->name, scm_type, buf.getCstr(), buf.getSize(), ns);
        }
        break;

        case xml_namespace: {
            xmlns_ptr ns = NSNode(node).getNamespaceLocal();

            /* That is bad to copy default namespace alone without its node  */
            if (ns->has_prefix() || depth > 0) {
                if (nsupdmap != NULL && ns->has_prefix()) {
                    replaceNamespace(ns, *nsupdmap);
                }

                if (depth == 0) {
                    const xmlns_ptr new_ns = swizzle_namespace(parent, ns);
                    if (new_ns != NULL_XMLNS) {
                        if (nsupdmap != NULL) { (**nsupdmap)[ns] = new_ns; }
                        ns = new_ns;
                    }
                }

                result = insert_namespace(left, right, parent, ns);
            } else {
                return left;
            }
        }
        break;

        default:
            throw SYSTEM_EXCEPTION("Deep copy error: document node copied");
    }

    CHECKP(result);

#ifdef SE_ENABLE_FTSEARCH
    update_insert_sequence(result, getSchemaNode(result));
#endif

#ifdef SE_ENABLE_TRIGGERS
    if (parent==XNULL) parent= nodeGetParentIndirection(left);
    apply_per_node_triggers(result, XNULL, parent, XNULL, TRIGGER_AFTER, TRIGGER_INSERT_EVENT);
#endif

    CHECKP(result);

    return result;
}

xptr copy_to_temp(xptr node)
{
    PPConstructor::checkInitial();
    return deep_copy_node(XNULL,
        XNULL,
        PPConstructor::getVirtualRoot().getPtr(),
        node,
        NULL,
        true);
}

