#include "tr/rcv/rcv_test_tr.h"
#include "common/xptr/xptr.h"
#include "tr/structures/schema.h"
#include "tr/mo/indirection.h"
#include "tr/vmm/vmm.h"
#include "tr/pstr/pstr.h"
#include "tr/btree/btree.h"
#include "tr/structures/metadata.h"
#include "tr/idx/indecies.h"
#include "tr/executor/base/PPBase.h"
#include "tr/tr_globals.h"
#include "tr/btree/btintern.h"

#include "tr/executor/xqops/PPTest.h"

#include <map>

// begin of hunk from PPTest.cpp
#define DESC_CONSIST
#define PSTR_CONSIST

static
xptr get_root (xptr node)
{
    CHECKP(node);
    xptr tmp=node;
    while (true)
    {
        xptr parent = nodeGetParent(tmp);
        if (parent == XNULL) {
            return tmp;
        }
        tmp = parent;
    }
}

static
bool is_same_root(xptr x, xptr y)
{
    return get_root(x)==get_root(y);
}

static FILE *logfile = NULL;
static bool isRcvOK = true;

static
void test_document(const char *name, xptr doc_dsc, bool is_throw)
{
    try
    {
       PPTest::checkTreeConsistency(doc_dsc);
       if (!is_throw) fprintf(logfile, "Checked document: %s\n", name);
    }
    catch(SednaException &e)
    {
       if (is_throw) throw;
       elog(EL_ERROR, ("Recovery failed on document: %s, error: %s\n", name, e.what()));
       fprintf(logfile, "Recovery failed on document: %s, error: %s\n", name, e.what());
       isRcvOK = false;
    }
}

static
void test_indexes(cat_list<index_cell_xptr>::item* sc_idx)
{
    cat_list<index_cell_xptr>::item* p = sc_idx;
/*
    while (p != NULL)
    {
        try
        {
//            bt_check_btree(p->object->btree_root);
                fprintf(logfile, "Checked index: %s\n", p->object->index_title);
        }
        catch (SednaException &e)
        {
                elog(EL_ERROR, ("Recovery failed on index: %s, error: %s\n", p->object->index_title, e.what()));
                fprintf(logfile, "Recovery failed on index: %s, error: %s\n", p->object->index_title, e.what());
                isRcvOK = false;
            }

        p = p->next;
    }
*/
}

static
void test_collection(const char *name, col_schema_node_cptr coll)
{
    bt_key key;
    key.setnew(" ");
    bt_cursor cursor = bt_find_gt(coll->metadata, key);

    if (cursor.is_null()) return;

    do
    {
        key = cursor.get_key();
        try
        {
            test_document((char*)key.data(), indirectionDereferenceCP(cursor.bt_next_obj()), true);
            fprintf(logfile, "Checked collection: %s, document: %s\n", name, (char*)key.data());
        }
        catch(SednaException &e)
        {
            elog(EL_ERROR, ("Recovery failed on collection: %s, document: %s, error: %s\n", name, (char*)key.data(), e.what()));
            fprintf(logfile, "Recovery failed on collection: %s, document: %s, error: %s\n", name, (char*)key.data(), e.what());
            isRcvOK = false;
        }
    }
    while(cursor.bt_next_key());

    test_indexes(coll->full_index_list->first);
}

void test_db_after_rcv()
{
    std::string rcv_fname = std::string(SEDNA_DATA) + std::string(tr_globals::db_name) + std::string("_files/rcv_test_result.log");
    metadata_cell_cptr mdc = XNULL;

    logfile = fopen(rcv_fname.c_str(), "at");
    fprintf(logfile, "---------------------------------------------------------------------\n");

    bt_cursor cursor = bt_lm(catalog_get_names(catobj_metadata));
    if (!cursor.is_null()) do
    {
        mdc = cursor.bt_next_obj();

        if (!mdc->is_document())
            test_collection(mdc->get_name(), mdc->get_schema_node());
        else
        {
            xptr blk = mdc->get_schema_node()->bblk;
            CHECKP(blk);
            xptr doc_dsc = getFirstBlockNode(blk);
            test_document(mdc->get_name(), doc_dsc, false);
            test_indexes(doc_schema_node_cptr(mdc->get_schema_node())->full_index_list->first);
        }
    }
    while(cursor.bt_next_key());

    fclose(logfile);

#ifdef RCV_TEST_CRASH
    rcv_fname = std::string(SEDNA_DATA) + std::string(tr_globals::db_name) + std::string("_files");

    if (isRcvOK)
        rcv_fname += std::string("/rcv_ok");
    else
        rcv_fname += std::string("/rcv_failed");

    r_fh = uCreateFile(rcv_fname.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_NO_BUFFERING, NULL, NULL);
    if (r_fh == U_INVALID_FD)
        fprintf(stderr, "Cannot create rcv result file\n");
    uCloseFile(r_fh, NULL);
#endif
}
