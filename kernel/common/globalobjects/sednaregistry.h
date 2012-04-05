#ifndef _SEDNA_REGISTRY_H_
#define _SEDNA_REGISTRY_H_

#include "common/globalobjects/globalnames.h"

void initSednaGlobalNameRegistry(int osObjectsMinBound, int databaseId, int sessionId);
void releaseSednaGlobalNameRegistry();

global_name createSednaGlobalName(const char * globalNameBase);

#endif /* _SEDNA_REGISTRY_H_ */
