#include <string>
#include <map>
#include "tr/vmm/vmm.h"
#include "tr/cat/catenum.h"
#include "tr/cat/catjournal.h"

using namespace std;

#define NameTree map<string, xptr>
#define NameTreeIterator map<string, xptr>::iterator

xptr catalog_complex_iterator::get_object() const
{
    return (* (NameTreeIterator *) this->it)->second;
}

const char * catalog_complex_iterator::get_name() const
{
    return (* (NameTreeIterator *) this->it)->first.c_str();
}

bool catalog_complex_iterator::next()
{
    NameTree * a = (NameTree *) this->tmp_tree;

    if (this->it == NULL) {
        this->it = new NameTreeIterator(a->begin());
    } else {
        (* (NameTreeIterator *) this->it)++;
    }

    return ((* (NameTreeIterator *) this->it) != a->end());
}

void catalog_complex_iterator::build_tree(enum catalog_named_objects ot)
{
    NameTree * a = new NameTree();
    this->tmp_tree = a;

    SafeMetadataSemaphore lock;
    VMMMicrotransaction mtrn;

    mtrn.begin();
    lock.Aquire();

    catalog_update_metadata();

    bt_cursor c = bt_lm(catalog_get_names(ot));

    while (!c.is_null()) {
        (*a)[string((char *) c.get_key().data())] = c.bt_next_obj();
        if (!c.bt_next_key()) break;
    }

    lock.Release();
    mtrn.end();

    catalog_journal_record *r = local_catalog->catalog_journal;

    while (r != NULL) {
        switch (r->type) {
        case catalog_journal_record::add_name:
            if (r->nor.object_type == ot) {
                (*a)[string(r->nor.name_to_save->name)] = r->nor.name_to_save->obj->p;
            }
            break;

        case catalog_journal_record::del_name:
            if (r->nor.object_type == ot) {
                (*a)[string(r->nor.name_to_save->name)] = XNULL;
            }
            break;
        default : ;
        }

        r = r->next;
    }
}

void catalog_complex_iterator::release_tree()
{
    if (this->it != NULL) {
        delete ((NameTreeIterator *) (this->it));
    }
    delete ((NameTree *) this->tmp_tree);
}
