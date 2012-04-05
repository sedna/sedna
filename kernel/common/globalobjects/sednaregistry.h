#ifndef _SEDNA_REGISTRY_H_
#define _SEDNA_REGISTRY_H_

#include "common/globalobjects/globalnames.h"

void initSednaGlobalNameRegistry(int osObjectsMinBound);
void releaseSednaGlobalNameRegistry();

global_name createSednaGlobalName(const char * globalNameBase, int objectId);

#endif /* _SEDNA_REGISTRY_H_ */
