#include <stdafx.h>
#include <stdlib.h>
#include <string.h>
#if defined(_Windows) && !defined(_INC_WINDOWS)
#   include <windows.h>
#endif
#if !defined(DTSVIEWR_H)
    #include <dtsviewr.h>
#endif
#include <dt_util.h>

#ifdef _MSC_VER
#   pragma warning(disable : 4018)  // signed/unsigned comparison
#endif


///////////////////////////////////////////////////////////////////////
//
//   Constructors and copy functions for the structures used in
//   the external viewer API
//


dtsBookMark::dtsBookMark()
{   clear();
}

dtsBookMark& dtsBookMark::copy(const dtsBookMark& other)
{   memmove(this, &other, sizeof(dtsBookMark));
    return *this;
}

void dtsBookMark::clear()
{   memset(this, 0, sizeof(dtsBookMark));
}

dtsTextBlock::dtsTextBlock()
{   textLen = 0;
    bufferSize = 0;
    buf = 0;
    fractionRead = 0;
}

void dtsTextBlock::copy(const dtsTextBlock& other)
{   int toCopy = other.textLen;
    if (toCopy > bufferSize)
        toCopy = bufferSize;
    if (toCopy > 0)
        memmove(buf, other.buf, toCopy);
    textLen = toCopy;
    fractionRead = other.fractionRead;
    bookMark.copy(other.bookMark);
};

dtsInputStream::dtsInputStream()
{   initStruct<dtsInputStream>(*this);
}

// sizeof dtsInputStream before structSize was added
const int oldInputStreamSize = 74;

dtsInputStream& dtsInputStream::copy(const dtsInputStream& other)
{	copyStruct<dtsInputStream>(*this, other, oldInputStreamSize);
    return *this;
}

int dtsInputStream::isValid()
{
#ifdef _Windows
    return !(IsBadCodePtr((FARPROC) seek) || IsBadCodePtr((FARPROC) read));
#else
    return 1;
#endif
}

void dtsInputStream::clear()
{	clearStruct<dtsInputStream>(*this, oldInputStreamSize);
}

// Size of dtsFileInfo before structSize was added
static const int oldFileInfoSize = 1622;

dtsFileInfo::dtsFileInfo()
{	initStruct<dtsFileInfo>(*this);
}

dtsFileInfo& dtsFileInfo::copy(const dtsFileInfo& other)
{	copyStruct<dtsFileInfo>(*this, other, oldFileInfoSize);
    return *this;
}

void dtsFileInfo::clear()
{	clearStruct<dtsFileInfo>(*this, oldFileInfoSize);
}

dtsRecognitionSignature::dtsRecognitionSignature()
{   clear();
}

dtsRecognitionSignature& dtsRecognitionSignature::copy(const dtsRecognitionSignature& other)
{   memmove(this, &other, sizeof(dtsRecognitionSignature));
    return *this;
}

void dtsRecognitionSignature::clear()
{   memset(this, 0, sizeof(dtsRecognitionSignature));
}

dtsViewerInfo::dtsViewerInfo()
{   clear();
}

dtsViewerInfo& dtsViewerInfo::copy(const dtsViewerInfo& other)
{   memmove(this, &other, sizeof(dtsViewerInfo));
    return *this;
}

void dtsViewerInfo::clear()
{   memset(this, 0, sizeof(dtsViewerInfo));
}

dtsViewerSetupInfo::dtsViewerSetupInfo()
{   memset(this, 0, sizeof(dtsViewerSetupInfo));
}

dtsContainerItemInfo::dtsContainerItemInfo()
{   clear();
}

void dtsContainerItemInfo::clear()
{   dtsFileInfo::clear();
    memset(nameInContainer, 0, sizeof nameInContainer);
    indexInContainer = 0;
}

dtsContainerItemInfo& dtsContainerItemInfo::copy(const dtsContainerItemInfo& other)
{   dtsFileInfo::copy(other);
    memmove(nameInContainer, other.nameInContainer, sizeof nameInContainer);
    indexInContainer = other.indexInContainer;
    return *this;
}

dtsMakeViewerParams::dtsMakeViewerParams()
{	initStruct<dtsMakeViewerParams>(*this);
}

//////////////////////////////////////////////////////////////////////
//
//   dtsInputStreamReader provides a wrapper around dtsInputStream for
//   convenient access.
//

dtsInputStreamReader::dtsInputStreamReader()
{   source.clear();
    pos = 0;
}

dtsInputStreamReader::dtsInputStreamReader(dtsInputStream& aSource)
{   source.clear();
    pos = 0;
    attach(aSource);
}

dtsInputStreamReader::~dtsInputStreamReader()
{
}

void dtsInputStreamReader::attach(dtsInputStream& aSource)
{   if (aSource.isValid())
        source.copy(aSource);
    else
        source.clear();
    pos = 0;
}

dtsInputStream& dtsInputStreamReader::getSource()
{   return source;
}

void dtsInputStreamReader::seek(long aPos)
{   pos = aPos;
    source.seek(source.pData, pos);
}

long dtsInputStreamReader::read(void *dest, long bytes)
{   if (bytes > source.size - pos)
        bytes = source.size - pos;
    unsigned long bytesRead = 0;
    if (bytes > 0)
        bytesRead = source.read(source.pData, dest, bytes);
    return bytesRead;
}


long dtsInputStreamReader::getSize() const
{   return source.size;
}

const char *dtsInputStreamReader::getName() const
{   return source.filename;
}

dtsFileDate& dtsInputStreamReader::getModified()
{   return source.modified;
}

dtsFileDate& dtsInputStreamReader::getCreated() 
{   return source.created;
}

long dtsInputStreamReader::tell() const
{   return pos;
}

int dtsInputStreamReader::getFractionRead(int scale)
{	if (source.getFractionRead)
		return source.getFractionRead(source.pData, scale);
	else
		return MulDiv(tell(), getSize(), 1000);
}


static void strncpyz(char *dest, const char *source, int maxLen)
{   strncpy(dest, source, maxLen-1);
    dest[maxLen-1] = '\0'; // make sure it is null-terminated
}

void dtsInputStreamReader::getFileInfo(dtsFileInfo& fi)
{
    strncpyz(fi.filename, source.filename, sizeof fi.filename);
    if (source.displayName)
        strncpyz(fi.displayName, source.displayName, sizeof fi.displayName);
    else
        strncpyz(fi.filename, source.filename, sizeof fi.filename);

    fi.modified.copy(source.modified);
    fi.created.copy(source.created);
    fi.size = source.size;
    strcpy(fi.title, "");
    };

dtsDataSource::dtsDataSource()
{	initStruct<dtsDataSource>(*this);
}

void dtsDataSource::copy(const dtsDataSource& other)
{	copyStruct<dtsDataSource>(*this, other);
}

dtsDataSourceDocExtractor::dtsDataSourceDocExtractor()
{	initStruct<dtsDataSourceDocExtractor>(*this);
}

dtsEngineFunctions::dtsEngineFunctions()
{	initStruct<dtsEngineFunctions>(*this);
}


#ifndef __UNIX__

#ifdef UNICODE
static FARPROC __GetProcAddress(HINSTANCE hi, const char *fn)
{
#ifdef UNDER_CE
    wchar_t wcfn[256];
    mbstowcs(wcfn, fn, 256);
    return GetProcAddress(hi, wcfn);
#else
    return ::GetProcAddress(hi, fn);
#endif
}
#else
#define __GetProcAddress        GetProcAddress
#endif

#pragma warning(disable:4311) // truncation of pointer from FARPROC to DWORD

// This is a static rather than a member function so that dtsviewr.h does not
// need to know what an HINSTANCE is (bringing in windows.h)
static void getDtsProc(HINSTANCE dllHandle, const char far *funcName, short ordinal, LPDWORD ptr)
{   FARPROC n;
    char fname2[80];
    n = __GetProcAddress(dllHandle, funcName);
    if (!n) {
        strcpy(fname2, "_");
        strcat(fname2, funcName);
        n = __GetProcAddress(dllHandle, fname2);
        }
#ifndef UNDER_CE
    char fname[80];
    if (!n) {
        strcpy(fname, funcName);
        strupr(fname);
        n = __GetProcAddress(dllHandle, fname);
        }
    if (!n) {
        strupr(fname2);
        n = __GetProcAddress(dllHandle, fname2);
        }
#endif
    if (!n) {
        funcName = (const char far *) ordinal;
        n = __GetProcAddress(dllHandle, funcName);
        }

    *ptr = (DWORD) n;
}

#define Ordinal_dtsRegisterViewer             1
#define Ordinal_dtssCreateSearcher            2
#define Ordinal_dtssDebugLog                  3
#define Ordinal_dtssDeleteIndex               4
#define Ordinal_dtssDeleteSearcher            5
#define Ordinal_dtssDoGetFileText             6
#define Ordinal_dtssDoIndexJob                7
#define Ordinal_dtssDoInit                    8
#define Ordinal_dtssDoSearchJob               9
#define Ordinal_dtssDoShutDown                10
#define Ordinal_dtssGetIndexInfo              11
#define Ordinal_dtssGetOptions                12
#define Ordinal_dtssRunScript                 13
#define Ordinal_dtssSetOptions                14
#define Ordinal_dtssVersion                   15
#define Ordinal_dtssGetIndexList              16
#define Ordinal_dtssConvertFile               17
#define Ordinal_dtssAddToLog                  18
#define Ordinal_dtssDebugLogEx                19
#define Ordinal_dtssMergeIndexes              20
#define Ordinal_dtssVersionEx                 21
#define Ordinal_dtssMapHitsInFile             22
#define Ordinal_dtssConvertFile2              23

dtsEngineFunctions::dtsEngineFunctions(void far *aHandle)
{   HINSTANCE dllHandle = (HINSTANCE) aHandle;
    memset(this, 0, sizeof (dtsEngineFunctions));
    structSize = sizeof(dtsEngineFunctions);
    getDtsProc(dllHandle, "dtssDoIndexJob", Ordinal_dtssDoIndexJob, (LPDWORD) &dtssDoIndexJob);
    getDtsProc(dllHandle, "dtssMergeIndexes", Ordinal_dtssMergeIndexes, (LPDWORD) &dtssMergeIndexes);
    getDtsProc(dllHandle, "dtssDoSearchJob", Ordinal_dtssDoSearchJob, (LPDWORD) &dtssDoSearchJob);
    getDtsProc(dllHandle, "dtssGetIndexInfo", Ordinal_dtssGetIndexInfo, (LPDWORD) &dtssGetIndexInfo);
    getDtsProc(dllHandle, "dtssVersion", Ordinal_dtssVersion, (LPDWORD) &dtssVersion);
    getDtsProc(dllHandle, "dtssVersionEx", Ordinal_dtssVersionEx, (LPDWORD) &dtssVersionEx);
    getDtsProc(dllHandle, "dtssDoInit", Ordinal_dtssDoInit, (LPDWORD) &dtssDoInit);
    getDtsProc(dllHandle, "dtssDoShutDown", Ordinal_dtssDoShutDown, (LPDWORD) &dtssDoShutDown);
    getDtsProc(dllHandle, "dtssDoGetFileText", Ordinal_dtssDoGetFileText, (LPDWORD) &dtssDoGetFileText);
    getDtsProc(dllHandle, "dtssDebugLog", Ordinal_dtssDebugLog, (LPDWORD) &dtssDebugLog);
    getDtsProc(dllHandle, "dtssDebugLogEx", Ordinal_dtssDebugLogEx, (LPDWORD) &dtssDebugLogEx);
    getDtsProc(dllHandle, "dtssAddToLog", Ordinal_dtssAddToLog, (LPDWORD) &dtssAddToLog);
    getDtsProc(dllHandle, "dtssSetOptions", Ordinal_dtssSetOptions, (LPDWORD) &dtssSetOptions);
    getDtsProc(dllHandle, "dtssGetOptions", Ordinal_dtssGetOptions, (LPDWORD) &dtssGetOptions);
    getDtsProc(dllHandle, "dtssDeleteIndex", Ordinal_dtssDeleteIndex, (LPDWORD) &dtssDeleteIndex);
    getDtsProc(dllHandle, "dtssRunScript", Ordinal_dtssRunScript, (LPDWORD) &dtssRunScript);
    getDtsProc(dllHandle, "dtssMapHitsInFile", Ordinal_dtssMapHitsInFile, (LPDWORD) &dtssMapHitsInFile);
    getDtsProc(dllHandle, "dtssConvertFile", Ordinal_dtssConvertFile, (LPDWORD) &dtssConvertFile);
    getDtsProc(dllHandle, "dtssConvertFile2", Ordinal_dtssConvertFile2, (LPDWORD) &dtssConvertFile2);
    getDtsProc(dllHandle, "dtssGetIndexList", Ordinal_dtssGetIndexList, (LPDWORD) &dtssGetIndexList);
}

void dtsEngineFunctions::copy(const dtsEngineFunctions& other)
{   memmove(this, &other, sizeof(dtsEngineFunctions));
}
#endif

dtsHighlightDataRequest::dtsHighlightDataRequest()
{	initStruct<dtsHighlightDataRequest>(*this);
}

dtsDataSourceFileInfo::dtsDataSourceFileInfo()
{	initStruct<dtsDataSourceFileInfo>(*this);
}


dtsDataSourceFileInfo::dtsDataSourceFileInfo(const dtsDataSourceFileInfo& other)
{	initStruct<dtsDataSourceFileInfo>(*this);
	copy(other);
}

void dtsDataSourceFileInfo::copy(const dtsDataSourceFileInfo& other)
{	copyStruct<dtsDataSourceFileInfo>(*this, other);
}

void dtsDataSourceFileInfo::clear()
{	clearStruct<dtsDataSourceFileInfo>(*this);
}
