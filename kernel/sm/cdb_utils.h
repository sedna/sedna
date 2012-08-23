#ifndef _CDB_UTILS_H
#define _CDB_UTILS_H

lsize_t determineLayerSize();

void createCfgFile();
void createDataDirectory();
void createInitialDbData();
void load_metadata ();
void createDb();

#endif