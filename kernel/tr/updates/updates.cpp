/*
 * File:  updates.cpp
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "common/sedna.h"

#include "tr/updates/updates.h"
#include "tr/mo/mo.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/nid/numb_scheme.h"
#include "tr/executor/xqops/PPConstructors.h"
#include "tr/mo/indirection.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers.h"
#endif

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
    xptr ind=((n_dsc*)XADDR(node))->indir;
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
    xptr ind=((n_dsc*)XADDR(node))->indir;
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
    xptr ind=((n_dsc*)XADDR(node))->indir;
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
        scn=(GETBLOCKBYNODE(parent))->snode;
    }
    else
    {
        tmp=(left==XNULL)?right:left;
        CHECKP(tmp);
        scn=(GETBLOCKBYNODE(tmp))->snode->parent;
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
            tmp=removeIndirection(tmp);
        }
        scn=scn->parent;
    }
}
#endif

xptr copy_node_content(xptr new_node_i, xptr node, xptr left_node_i, upd_ns_map** nsupdmap, bool save_types, unsigned short depth) {
    xptr left_node = XNULL;
    xptr childi = getIndirectionSafeCP(getFirstByOrderChildCP(node));

    while (childi != XNULL) {
        left_node = deep_copy_node_i(left_node_i, XNULL, new_node_i, indirectionDereferenceCP(childi), nsupdmap, save_types, depth + 1);

        /* due to : MG: deep_temp_copy can return XNULL if a trigger canceled
        * the insertion and there were now any left sibling */
        if (left_node != XNULL) {
            left_node_i = getIndirectionSafeCP(left_node);
        }

        childi = getRightSiblingIndirectionCP(indirectionDereferenceCP(childi));
    }

    return left_node_i;
}

void swizzleNamespace (xmlns_ptr & ns, upd_ns_map*& updmap)
{
    if (updmap == NULL) updmap = se_new upd_ns_map;
    upd_ns_map::const_iterator it = updmap->find(ns);
    if (it == updmap->end()) {
        ns = xmlns_touch(ns->prefix, ns->uri);
    } else {
        ns = it->second;
    }
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
            parent = removeIndirection(((n_dsc*) XADDR(left))->pdsc);
        } else {
            CHECKP(right);
            parent = removeIndirection(((n_dsc*) XADDR(right))->pdsc);
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
    scmnode = getBlockHeaderCP(node)->snode;

    switch (scmnode->type) {
        case element: {
            xptr result_indir;
            xmlns_ptr ns = scmnode->get_xmlns();

            if (nsupdmap != NULL && ns != NULL_XMLNS) {
                swizzleNamespace(ns, *nsupdmap);
            }

            scm_type = (save_types) ? E_DSC(node)->type : xs_untyped;
            result = insert_element(left, right, parent, scmnode->name, scm_type, ns);
            result_indir = get_last_mo_inderection();

            copy_node_content(result_indir, indirectionDereferenceCP(node_indir), XNULL, nsupdmap, save_types, depth);

            result = indirectionDereferenceCP(result_indir);
            CHECKP(result);
                      }
                      break;

        case text: {
            if (isTextEmpty(T_DSC(node))) {
                throw SYSTEM_EXCEPTION("BAD DATA!!!");
            } else {
                xptr data = getTextPtr(T_DSC(node));
                CHECKP(node);
                result = insert_text(left, right, parent, &data, getTextSize(T_DSC(node)), text_doc);
            }
                   }
                   break;

        case cdata: {
            text_cptr buf(node);
            result = insert_cdata(left, right, parent, buf.get(), buf.getSize());
                    }
                    break;

        case comment: {
            text_cptr buf(node);
            result = insert_comment(left, right, parent, buf.get(), buf.getSize());
                      }
                      break;

        case pr_ins: {
            size_t tsize = PI_DSC(node)->target;
            text_cptr buf(node);
            result = insert_pi(left, right, parent, buf.get(), tsize, buf.get() + tsize + 1, buf.getSize() - tsize - 1);
                     }
                     break;

        case attribute: {
            xmlns_ptr ns = scmnode->get_xmlns();
            text_cptr buf(node);

            if (nsupdmap != NULL && ns != NULL_XMLNS) {
                swizzleNamespace(ns, *nsupdmap);
            }

            CHECKP(node);
            scm_type = (save_types) ? A_DSC(node)->type : xs_untypedAtomic;
            result = insert_attribute(left, right, parent, scmnode->name, scm_type, buf.get(), buf.getSize(), ns);
                        }
                        break;

        case xml_namespace: {
            xmlns_ptr ns = xmlns_touch(NS_DSC(node)->ns);
            if (nsupdmap != NULL && ns != NULL_XMLNS) {
                swizzleNamespace(ns, *nsupdmap);
            }
            result = insert_namespace(left, right, parent, ns);
                            }
                            break;

        default:
            throw SYSTEM_EXCEPTION("Deep copy error: document node copied");
    }

    CHECKP(result);

#ifdef SE_ENABLE_FTSEARCH
    update_insert_sequence(result, schema_node_cptr((GETBLOCKBYNODE(result))->snode));
#endif

#ifdef SE_ENABLE_TRIGGERS
    if (parent==XNULL) parent=removeIndirection(((n_dsc*)XADDR(left))->pdsc);
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
        PPConstructor::get_virtual_root(),
        node,
        NULL,
        true);
}

