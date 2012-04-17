#ifndef _COMMAND_LINES_H_
#define _COMMAND_LINES_H_

#include <string>
#include "common/ipc_ops.h"
#include "common/structures/sm_structures.h"
#include "common/structures/cdb_structures.h"

string  constructClForSM     (gov_config_struct * cfg, SMInfo * sminfo, char * dbname);
string  constructClForTrn    (gov_header_struct * cfg, char* dbname, int db_id);
string  constructClForCdb    (const std::string& ticket);

#endif