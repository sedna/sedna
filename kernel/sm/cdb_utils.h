#ifndef _CDB_UTILS_H
#define _CDB_UTILS_H

#include "common/xptr.h"
#include "common/structures/cdb_structures.h"
#include "common/config.h"

lsize_t determineLayerSize(CdbParameters * cdbParams, gov_config_struct * cfg);
void createCfgFile(CdbParameters * cdbParams);
void createDataDirectory(CdbParameters * cdbParams);
void createInitialDbData(CdbParameters * cdbParams);
void load_metadata (CdbParameters * cdbParams, gov_header_struct * cfg);
void createDb(CdbParameters * cdbParams, gov_header_struct * cfg);

#endif