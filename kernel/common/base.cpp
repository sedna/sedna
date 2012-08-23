/*
 * File:  base.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <fstream>
#include "common/base.h"
#include "u/ugnames.h"
#include "u/uhdd.h"
#include "u/uprocess.h"
#include "common/errdbg/d_printf.h"
#include "u/uutils.h"

using namespace std;

FILE* res_os = stdout; //otput stream of transaction results (result of the user's query)
const char * SEDNA_DATA = NULL;
static char base_path_int[1024] = {};
const char * base_path = base_path_int;

void init_base_path(const char* arg0)
{
    program_name_argv_0 = arg0;
    uGetImageProcPath(base_path_int, __sys_call_error);
}

/* The following chars are allowed:
 * ! (0x21), # (0x23), % (0x25), & (0x26), ( (0x28), ) (0x29),
 * + (0x2B), , (0x2C), - (0x2D), . (0x2E), 0-9 (0x30 - 0x39),
 * ; (0x3B), = (0x3D), @ (0x40), A-Z (0x41-0x5A), [ (5B), ] (5D),
 * ^ (0x5E), _ (0x5F), ` (0x60), a-z (0x61-0x7A), { (0x7B), 
 * } (0x7D), ~ (0x7E) */
static const unsigned char 
database_name_map[16] = {0x00, 0x00, 0x00, 0x00,
                         0x56, 0xDE, 0xFF, 0xD4,
                         0xFF, 0xFF, 0xFF, 0xF7,
                         0xFF, 0xFF, 0xFF, 0xF6};


/* Is char is allowed within a database name  */
#define DATABASE_NAME_ALLOWED_BYTE(byte) \
    ((byte) & 0x80 ? 0 : (database_name_map[((byte) >> 3)] & (0x80 >> ((byte) & 7))))


void check_db_name_validness(const char* name) 
{
    if (NULL == name)  
        throw USER_EXCEPTION2(SE1003, 
            "database name validation failed (null database name was given)");
    
    size_t len = strlen(name), counter = 0;

    /* Name must contain at least one symbol and its length must
     * be less or equal than MAX_DATABASE_NAME_LENGTH */
    if (len < 1 || len > MAX_DATABASE_NAME_LENGTH) 
        throw USER_EXCEPTION2(SE4307, "empty or too long database name");

    while(counter < len)
    {
        unsigned char c = name[counter];
        if(DATABASE_NAME_ALLOWED_BYTE(c))
            counter++;
        else
            throw USER_EXCEPTION2(SE4307, name);
    }
}

