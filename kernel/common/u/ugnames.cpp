#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "common/errdbg/exceptions.h"
#include "ugnames.h"

/* {% globals */

static UGlobalNamesRegistryItem *registry = NULL;
static UGlobalNamesRegistrySearchProc searchProc = NULL;

/* }% */

#ifdef _WIN32
#define IPC_PRIVATE 0
#ifndef snprintf
#define snprintf _snprintf
#endif
#else
#include <sys/types.h>
#include <sys/ipc.h>
#endif

void
UReleaseGlobalNamesRegistry()
{
	registry = NULL;
	searchProc = NULL;
}

static
const UGlobalNamesRegistryItem *
SearchGlobalNamesRegistry(const UGlobalNamesRegistryItem *registry,
						  const char *basename)
{
	assert(registry);
	while (registry->basename && 0!=strcmp(registry->basename,basename)) ++registry;
	return registry->basename ? registry : NULL;
}

static void ThrowSystemException(const char *msg)
{
	throw SYSTEM_EXCEPTION(msg);
}

static int ValidateBaseName(const char *baseName)
{
	/* TODO: implement validation */
	return 1;
}

static int ValidateGlobalName(const char *globalName)
{
	/* TODO: implement validation */
	return 1;
}

static int ValidateCompoundName(const char *compoundName)
{
	/* TODO: implement validation */
	return 1;
}

void
UInitGlobalNamesRegistry(UGlobalNamesRegistryItem *registryParam,
						 UGlobalNamesRegistrySearchProc searchProcParam,
						 int rangeBegin,
						 int rangeEnd)
{
	int rangeSizeMin = 0;
	UGlobalNamesRegistryItem *i = NULL;
	char errorBuf[128];

	assert(registryParam);
	/* first let's estimate the required range size */
	for (i = registryParam; i->basename; ++i)
	{
		if (!ValidateBaseName(i->basename) || i->nObjectsMax<1 || (i->prefix==NULL && i->nObjectsMax > 1))
		{
			snprintf(errorBuf, sizeof errorBuf,
				"UInitGlobalNamesRegistry: bad item '%s'", i->basename);
			errorBuf[(sizeof errorBuf)-1]=0;
			ThrowSystemException(errorBuf);
		}
		rangeSizeMin+=i->nObjectsMax;
	}
	registry = registryParam;
	/* check if the passed range is ok */
	if (rangeEnd - rangeBegin < rangeSizeMin)
		ThrowSystemException("InitGlobalNamesRegistry: range too small");
	/* now initialize per-entry ranges */
	for (i = registry; i->basename; ++i)
	{
		i->rangeBegin = rangeBegin;
		rangeBegin = i->rangeEnd = rangeBegin + i->nObjectsMax;
	}
	/* complete initialization */
	searchProc = (searchProcParam ? searchProcParam : SearchGlobalNamesRegistry);
}

const char *
UCreateGlobalName(const char *basename,
				  int objectId,
				  char *buf,
				  size_t bufSize)
{
	char prefix[32] = "", errorBuf[128];
	int ordinal = 0;
	const UGlobalNamesRegistryItem *item = NULL;
	int stored = 0;

	assert (basename);
	item = searchProc(registry, basename);
	if (!item)
	{
		snprintf(errorBuf, sizeof errorBuf, "CreateGlobalName: '%s' not found", basename);
		errorBuf[(sizeof errorBuf)-1]=0;
		ThrowSystemException(errorBuf);
	}
	ordinal = item->rangeBegin + objectId;
	if (item->prefix)
	{
		stored = snprintf(prefix, sizeof prefix, "%s%d.", item->prefix, objectId);
		if (stored<0 || (size_t)stored>=(sizeof prefix))
			ThrowSystemException("UCreateGlobalName: prefix too long");
	}
	if (ordinal >= item->rangeEnd || ordinal < item->rangeBegin)
		ThrowSystemException("CreateGlobalName: generated ordinal out of range");

	stored = snprintf(buf, bufSize, "SEDNA%d.%s%s@%d", registry->rangeBegin, prefix, basename, ordinal);
	if (stored<0 || (size_t)stored>=bufSize)
		ThrowSystemException("CreateGlobalName: buffer too small");

	return buf;
}

struct GlobalNameComponents
{
	const char *strNameBegin, *strNameEnd;
	int base, ordinal;
};

static
void ParseGlobalName(GlobalNameComponents *components,
					 const char *globalName)
{
	assert(components);
	if (globalName==NULL)
	{
		components->strNameBegin = NULL;
		components->strNameEnd = NULL;
		components->base = 0;
		components->ordinal = 0;
	}
	else
	{
		const char *sep = NULL;
		char *tail = NULL;
		int ordinal = 0, base = 0;

		sep = strchr(globalName,'@');
		if (sep==NULL || (ordinal=strtol(sep+1,&tail,10), *tail!='\0'))
		{
			ThrowSystemException("ParseGlobalName: bad global name syntax");
		}
		components->strNameBegin = globalName;
		components->strNameEnd = sep;
		components->base = base;
		components->ordinal = ordinal;
	}
}

static
const char *StrNameFromGlobalName(const char *globalName,
								  size_t limit,
								  const char *prefix,
								  char *buf,
								  size_t bufSize)
{
	char bufjr[16];
	int partSz = 0, stored = 0;
	GlobalNameComponents components = {NULL};

	assert(prefix);
	ParseGlobalName(&components,globalName);
	if (globalName == NULL)
	{
		buf = NULL;
	}
	else
	{
		partSz = (int)(components.strNameEnd - components.strNameBegin);
		stored = snprintf(buf, bufSize, "%s%.*s", prefix, partSz, components.strNameBegin);
		if (stored<0 || (size_t)stored>=bufSize)
			ThrowSystemException("StrNameFromGlobalName: buffer too small");
		if (limit>0 && (size_t)stored>limit)
		{
			stored = snprintf(bufjr, sizeof bufjr, "~%d", components.ordinal);
			if (stored<0 || (size_t)stored>=sizeof bufjr) ThrowSystemException("StrNameFromGlobalName: internal error");
			if ((size_t)stored>limit) ThrowSystemException("StrNameFromGlobalName: impossible limit");
			strcpy(buf+limit-stored, bufjr);
		}
	}
	return buf;
}

const char *UWinIPCNameFromGlobalName(const char *globalName,
									  char *buf,
									  size_t bufSize)
{
	return StrNameFromGlobalName(globalName, 0, "", buf, bufSize);
}

const char *UPosixIPCNameFromGlobalName(const char *globalName,
										char *buf,
										size_t bufSize)
{
	const char *prefix = "/";
	size_t limit = 0;

#if (defined(FreeBSD) || defined(DARWIN))
	prefix = "/tmp/";
#endif

#if defined(DARWIN)
    limit = 30;
#endif

	return StrNameFromGlobalName(globalName, limit, prefix, buf, bufSize);
}

int USys5IPCKeyFromGlobalName(const char *globalName)
{
	int key = 0;
	GlobalNameComponents components = {NULL};

	ParseGlobalName(&components,globalName);
	if (globalName == NULL)
	{
		key = IPC_PRIVATE;
	}
	else
	{
		key = components.ordinal;
		if (key==IPC_PRIVATE)
			ThrowSystemException("USys5IPCKeyFromGlobalName: bad key");
	}
	return key;
}

const char *
UCreateCompoundName(const char **namesVec,
					size_t namesCount,
					char *buf,
					size_t bufSize)
{
	size_t nameLen = 0;
	const char **nameCur = namesVec, **namesEnd = namesVec+namesCount;
	char *bufpos = buf;
	assert(buf && bufSize>0);
	strcpy(bufpos,"");
	while(nameCur != namesEnd)
	{
		if (nameCur!=namesVec)
		{
			/* checked there is enough room in buf on previous iteration */
			strcpy(bufpos,",");
			++bufpos; bufSize-=1;
		}
		if (!ValidateGlobalName(*nameCur))
			ThrowSystemException("UCreateCompoundName: bad global name syntax");
		nameLen = (*nameCur?strlen(*nameCur):0);
		if (bufSize < nameLen + 2)
			ThrowSystemException("UCreateCompoundName: buffer too small");
		if (*nameCur)
		{
			strcpy(bufpos, *nameCur);
			bufpos+=nameLen;
			bufSize-=nameLen;
		}
		++nameCur;
	}
	return buf;
}

const char *
UGlobalNameFromCompoundName(const char *compoundName,
							int index,
							char *buf,
							size_t bufSize)
{
	const char *pos = NULL, *epos=NULL;
	assert(buf && compoundName);
	if (!ValidateCompoundName(compoundName))
	{
		ThrowSystemException("UGlobalNameFromCompoundName: bad compound name syntax");
	}
	else
	{
		pos = compoundName;
		while(index > 0 && pos)
		{
			pos = strchr(pos,',');
			if (pos) pos+=1;
			--index;
		}
		if (!pos || index<0)
		{
			ThrowSystemException("UGlobalNameFromCompoundName: index out of range");
		}
		epos = strchr(pos,',');
		if (!epos) epos=pos+strlen(pos);
		if (bufSize < (unsigned int) (epos-pos+1))
		{
			ThrowSystemException("UGlobalNameFromCompoundName: buffer too small");
		}
		if (pos == epos)
		{
			buf = NULL;
		}
		else
		{
			memcpy(buf,pos,epos-pos);
			buf[epos-pos]=0;
		}
	}
	return buf;
}
