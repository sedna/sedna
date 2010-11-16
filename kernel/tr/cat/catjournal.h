#include "tr/cat/catalog.h"
#include "tr/cat/CatalogXptrHash.h"

#define CATALOG_NAME_TREE(i) (local_catalog->masterdata.trees[i])
#define MAX_ROOT_NID_SIZE 1024

struct catalog_name_record {
    char * name;
    enum catalog_named_objects obj_type;
    catalog_object_header * obj;
    catalog_name_record * next;
    bool name_deleted;
};

struct catalog_name_trees {
    xptr trees[catobj_count];
    xptr htable;
};

struct catalog_header {
    catalog_name_record * names[CCACHE_NAME_BUCKETS];
    FastCatalogXptrHash xptrhash;

    catalog_header() {
        memset(names, 0, CCACHE_NAME_BUCKETS * sizeof(catalog_name_record *));
    }
};

struct catalog_master_record {
    vmm_sm_blk_hdr sm_vmm;  /* sm/vmm parameters */
    catalog_name_trees masterdata;

    int last_nid_size;
    uint8_t last_nid[MAX_ROOT_NID_SIZE];
};

struct catalog_journal_record {
    enum {
        add_name,
        del_name,
        add_htable_record
    } type;

    union {
        struct {
            enum catalog_named_objects object_type;
            catalog_name_record * name_to_save;
        } nor;

        struct {
            enum catalog_named_objects object_type;
            char * name;
            char * data;
        } htr;
    };

    catalog_journal_record * next;
};


struct local_catalog_header : public catalog_header {
    bool initialized;
    bool masterdata_loaded;
    bool masterdata_updated;

    FastPointerArray header_list;
    FastPointerArray object_list;
    FastPointerArray namerecord_list;

    catalog_name_trees masterdata;

    catalog_object_header  * invalid_list;
    catalog_object_header  * invalid_list_tail;

    catalog_journal_record * catalog_journal;
    catalog_journal_record * catalog_journal_tail;

    inline void add_journal_record(catalog_journal_record * cr) {
        if (this->catalog_journal == NULL) {
            this->catalog_journal = cr;
        }

        if (this->catalog_journal_tail != NULL) {
            this->catalog_journal_tail->next = cr;
        }

        catalog_journal_tail = cr;
        cr->next = NULL;
    }

    local_catalog_header() :
        masterdata_updated(false),
        invalid_list(NULL), invalid_list_tail(NULL),
        catalog_journal(NULL), catalog_journal_tail(NULL) { }

    ~local_catalog_header() {
        header_list.destroyAll<catalog_object_header>(NULL);
        object_list.destroyAll<catalog_object>(NULL);
        namerecord_list.destroyAll<catalog_name_record>(NULL);
    };
};

void catalog_update_metadata();

inline uint16_t hash(const xptr &p) { return ((uint32_t) p.getOffs() >> 7) % CCACHE_XPTR_BUCKETS ; };

inline uint16_t hash(const char * a) {
    uint8_t i = 0;
    while (*a != '\0') { i += (* (uint8_t *) a); a++;  }
    return i % CCACHE_NAME_BUCKETS;
};


inline catalog_name_record * catalog_cachetree_find_name(
        enum catalog_named_objects obj_type,
        const char * name)
{
    catalog_name_record * r;

    r = local_catalog->names[hash(name)];
    while ((r != NULL) && ((r->name == NULL) || (strcmp(r->name, name) != 0) || (r->obj_type != obj_type))) { r = r->next; }

    return r;
}

inline catalog_name_record * catalog_cachetree_add_name(
        enum catalog_named_objects obj_type,
        const char * name,
        catalog_object_header * obj)
{
    catalog_name_record ** r = &(local_catalog->names[hash(name)]);
    catalog_name_record * n = new (cat_malloc_context(CATALOG_TEMPORARY_CONTEXT, sizeof(catalog_name_record))) catalog_name_record;

    n->name = cat_strcpy(n, name);
    n->obj = obj;
    n->obj_type = obj_type;
    n->next = *r;
    n->name_deleted = false;
    *r = n;
    local_catalog->namerecord_list.add(n);

    return n;
}
