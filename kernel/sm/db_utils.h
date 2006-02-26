#ifndef _DB_UTILS_H
#define _DB_UTILS_H

int cleanup_db(const char* db_name);

bool exist_db(const char* db_name);

int load_metadata_in_database(const char* db_name);
#endif
