/*
 * File: catstore.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CATSTORE
#define _CATSTORE

#include "common/sedna.h"
#include "common/xptr/xptr.h"
#include "common/xptr/sm_vmm_data.h"

#include "tr/cat/simplestream.h"

#define CS_CELL_SIZE 256
#define CS_CELL_INS_BITS 8
#define CS_CELL_COUNT (PAGE_SIZE / CS_CELL_SIZE)
#define CS_BITS_PER_INT (sizeof(uint32_t) * 8)
#define CS_CELL_BITMAP_SIZE ((PAGE_SIZE / CS_CELL_SIZE) / CS_BITS_PER_INT)

typedef uint32_t cs_size_t;

void cs_pushp();
void cs_popp();
void cs_initp();

void cs_write(xptr &p, const char * data, cs_size_t data_size);
// void cs_read(const xptr &p, char * data, cs_size_t data_size);

void cs_set_hint(const xptr p);

int       cs_get_magic(const xptr p);
cs_size_t cs_get_size(const xptr p);

void cs_write_part(const xptr p, const char * data, off_t data_offset, cs_size_t data_size);
void cs_read_part(const xptr p, char * data, off_t data_offset, cs_size_t data_size);

void cs_free(xptr p);


class cs_stream : public se_simplestream {
private:
    xptr p;
public:
    enum mode { mode_read, mode_write } _mode;

    cs_stream(const xptr _p, enum mode mode);
    ~cs_stream();

    void commit();
    inline xptr get_xptr() { return p; };
};


#endif
