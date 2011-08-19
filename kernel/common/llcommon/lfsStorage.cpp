/*
 * File:  lfsStorage.cpp - LFS (linear file storage) subsystem
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * This is an implementation of lfs storage; The main unit is a chan of files such as:
 * <prefix>.5.<ext>, <prefix>.6.<ext>, <prefix>.7.<ext>, and so on. Numbers are always continuous, and can range from 1 to
 * a UINT64MAX - 1. However, actual chain may start from any number (5, for example), because chain is truncated from
 * time to time (by user request).
 *
 * From the user point of view a chain is a linear space. He addresses records by specifying LSN number (uint64_t), and he
 * receives such numbers when he puts records into storage.
 *
 * This storage can be shared among some processes. So, processes can write to it concurrently. Synchronization is implemented
 * within lfs subsystem. So, there is no need to provide synchronization on the user level.
 *
 * Buffering is implemented within lfs subsystem. There are two kinds of buffers: for write and read operations. Write-buffer
 * is shared among users. All records are placed in this buffer with lfsAppendRecord function. From time to time this buffer
 * is flushed on disk, either automatically (not enough free space) or by user request. Read buffer is of read-ahead type.
 * It is local to a current process (it is not a shared one). The size of each buffer is specified in lfsInit function; also
 * each process can specify its own size of read buffer in lfsConnect function.
 *
 * This storage doesn't know anything about semantics of user-data. It just stores chunks of data provided by the user.
 * User, however, can specify its own header (for lfs it is also a bunch of bytes). This header is stored in a special way.
 * First sector of each lfs file contains lfs-owned header plus user-specified header. So, the size of the user specified
 * header cannot exceed sector_size(usually 512) - sizeof(lfsHeader_t) bytes. All data become permanently stored only
 * when user writes header with lfsWriteHeader call.
 *
 * At the current implementation all errors are processed by ProcessError function, which simply throws an exception.
 *
 */

#include "common/llcommon/lfsStorage.h"
#include "common/u/uhdd.h"
#include "common/u/ushm.h"
#include "common/u/usem.h"
#include "common/base.h"
#include "common/u/uutils.h"

#include "common/errdbg/exceptions.h"
#include <assert.h>

#include <string>
#include <stdint.h>

using namespace std;

// LFS runtime info shared between processes
struct lfsInfo_t
{
	LSN NextLSN;           // pointer to the free space in storage
	uint64_t FirstFileNum; // number of the first file
	uint64_t LastFileNum;  // number of the last file
	uint64_t ChunkSize;    // maximum size of log file

	unsigned int BufSize;           // buffer size in bytes
    unsigned int BufStart;          // begin of buffer records
    unsigned int BufOffset;         // current place to write in buffer
    unsigned int BufKeepBytes;      // number of keep bytes
};

// LFS-specific header of each file
struct lfsHeader_t
{
	LSN NextLSN;            // pointer to the free space in file
	uint64_t FirstFileNum;  // number of the first file (to truncate unnecessary ones at the init stage)
	uint64_t ChunkSize;     // maximum size of a log file
};

// structure for file-descriptors cache
struct lfsFileInfo_t
{
	UFile FileDsc;
	uint64_t FileNum;
};

static lfsInfo_t *lfsInfo   = NULL; // pointer to lfsInfo shared in memory
static void *lfsWriteBuffer = NULL; // write buffer (in shared memory)

static void *lfsReadBuf  = NULL;    // local read buffer
static unsigned int ReadBufSize = 0;         // size of read buffer (can variate depending on the process)
static LSN StartReadLSN = 0, EndReadLSN = 0; // boundaries for info in read buffer

static USemaphore SyncSem;  // to synchronize multiple processes
static UShMem     ShDsc;    // descriptor for shared memory buffer

static string ChainPath;    // path for files chain
static string Prefix;    	// name of the prefix (file name: <prefix>.<number>.<ext>, specified in lfsInit)
static string Ext;          // name of the extension

static unsigned int lfsSectorSize = 512; // size of disc sector

// cache for open descriptors (it's just a simple hash; file_number % LFS_CACHE_SIZE)
#define LFS_CACHE_SIZE 16
static lfsFileInfo_t lfsDescCache[LFS_CACHE_SIZE];


#define LFS_ERROR(err_msg) _lfsProcessError(__FILE__, __SE_FUNCTION__,  __LINE__, (err_msg), NULL)
#define LFS_ERROR2(err_msg, adds) _lfsProcessError(__FILE__, __SE_FUNCTION__,  __LINE__, (err_msg), (adds))
#define LFS_WARN(err_msg) _lfsProcessWarning(__FILE__, __SE_FUNCTION__,  __LINE__, (err_msg), NULL)

// processes error (throws exception for now)
static void _lfsProcessError(const char *file, const char *func, int line, const char *lfsErrorMsg, const char *adds)
{
	string err_msg = string(lfsErrorMsg);

	if (adds != NULL)
 		err_msg += ": " + string(adds);

#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
	err_msg += " - (" + string(file) + ':' + string(func) + ':' + int2string(line) + ')';
#endif

	throw SYSTEM_EXCEPTION(err_msg.c_str());
}

// processes warnings (logs with EL_WARN for now)
static void _lfsProcessWarning(const char *file, const char *func, int line, const char *lfsErrorMsg, const char *adds)
{
    string err_msg = string(lfsErrorMsg);

    if (adds != NULL)
        err_msg += ": " + string(adds);

#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
    err_msg += " - (" + string(file) + ':' + string(func) + ':' + int2string(line) + ')';
#endif

    elog(EL_WARN, ("%s", err_msg.c_str()));
}

// simple procedure to make string file name from file num
inline
static string _lfsGetFileName(uint64_t num)
{
	char buf[50];

	return ChainPath + Prefix + "." + string(u_ui64toa(num, buf, 10)) + "." + Ext;
}

// synchro primitives
inline
static void lfsLock()
{
	if (USemaphoreDown(SyncSem, __sys_call_error) != 0)
		throw SYSTEM_EXCEPTION("Cannot down semaphore: SEDNA_LFS_SEM_NAME");
}

inline
static void lfsUnlock()
{
	if (USemaphoreUp(SyncSem, __sys_call_error) != 0)
		throw SYSTEM_EXCEPTION("Cannot up semaphore: SEDNA_LFS_SEM_NAME");
}

// inits file descriptors cache
// synchronizaion must be provided by the caller!
static int _lfsInitFileCache()
{
	for (int i = 0; i < LFS_CACHE_SIZE; i++)
		lfsDescCache[i].FileDsc = U_INVALID_FD;

	return 0;
}

// closes all files in file descriptors cache
// synchronizaion must be provided by the caller!
static int _lfsCloseAllCachedFiles()
{
	for (int i = 0; i < LFS_CACHE_SIZE; i++)
		if (lfsDescCache[i].FileDsc != U_INVALID_FD)
		{
		   	if (uCloseFile(lfsDescCache[i].FileDsc, __sys_call_error) == 0)
		   	{
	   			LFS_ERROR("lfs error: cannot close file");
	   			return -1;
	   		}
			lfsDescCache[i].FileDsc = U_INVALID_FD;
		}

	return 0;
}

// closes cached descriptor
static int _lfsCloseCachedDesc(uint64_t fileNum)
{
    unsigned int CacheNum;

	CacheNum = (unsigned int)(fileNum % LFS_CACHE_SIZE);

	if (lfsDescCache[CacheNum].FileNum == fileNum && lfsDescCache[CacheNum].FileDsc != U_INVALID_FD)
	{
	   	if (uCloseFile(lfsDescCache[CacheNum].FileDsc, __sys_call_error) == 0)
	   	{
   			LFS_ERROR2("lfs error: cannot close file", _lfsGetFileName(fileNum).c_str());
   			return -1;
   		}

		lfsDescCache[CacheNum].FileDsc = U_INVALID_FD;
	}

	return 0;
}

// opens file and returns descriptor;
// if it finds descriptor in cache it returns stored descriptor;
// if not, then opens files and stores its descriptor
// synchronizaion must be provided by the caller!
static UFile _lfsOpenAndCacheFile(uint64_t fileNum)
{
	unsigned int CacheNum;
	UFile fileDsc;
	string fName;

	assert(fileNum > 0 && fileNum < UINT64_MAX);

	CacheNum = (unsigned int)(fileNum % LFS_CACHE_SIZE);

	if (lfsDescCache[CacheNum].FileNum == fileNum && lfsDescCache[CacheNum].FileDsc != U_INVALID_FD)
		return lfsDescCache[CacheNum].FileDsc;

	// close previous file
	if (lfsDescCache[CacheNum].FileDsc != U_INVALID_FD)
	{
	   	if (uCloseFile(lfsDescCache[CacheNum].FileDsc, __sys_call_error) == 0)
	   	{
   			LFS_ERROR2("lfs error: cannot close file", _lfsGetFileName(lfsDescCache[CacheNum].FileNum).c_str());
   			return U_INVALID_FD;
   		}
	}

	fName = _lfsGetFileName(fileNum);

    if ((fileDsc = uOpenFile(fName.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE,
    		U_WRITE_THROUGH, __sys_call_error)) == U_INVALID_FD)
    {
    	LFS_ERROR2("lfs error: critical error in lfs chain: cannot open file", fName.c_str());
    	return U_INVALID_FD;
    }

	lfsDescCache[CacheNum].FileDsc = fileDsc;
	lfsDescCache[CacheNum].FileNum = fileNum;

	return fileDsc;
}

// Reads file header from the specified file
static int _lfsGetHeaderFromFile(uint64_t filenum, void *HeaderBuf, unsigned int HeaderSize)
{
	UFile fileDsc;
	unsigned int readbytes;
    int res;

	assert(filenum > 0 && filenum < UINT64_MAX && HeaderBuf != NULL);

	fileDsc = _lfsOpenAndCacheFile(filenum);

	if (uSetFilePointer(fileDsc, 0, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
	{
    	LFS_ERROR2("lfs error: critical error in lfs chain: cannot set file pointer", _lfsGetFileName(filenum).c_str());
    	return -1;
    }

	res = uReadFile(fileDsc, HeaderBuf, HeaderSize, &readbytes, __sys_call_error);

	if (res == 0 || readbytes != HeaderSize)
	{
    	LFS_ERROR2("lfs error: critical error in lfs chain: cannot read user header", _lfsGetFileName(filenum).c_str());
    	return -1;
    }

	return 0;
}

static int _lfsGetLfsHeaderFromFile(uint64_t fileNum, lfsHeader_t *lfsHeader)
{
    void *SectorBuf;

	if ((SectorBuf = malloc(lfsSectorSize)) == NULL)
	{
		LFS_ERROR("lfs error: cannot allocate memory");
		return -1;
	}

	if (_lfsGetHeaderFromFile(fileNum, SectorBuf, lfsSectorSize) != 0)
	{
		LFS_ERROR2("lfs error: cannot read file header", _lfsGetFileName(fileNum).c_str());
		return -1;
	}

	memcpy(lfsHeader, SectorBuf, sizeof(lfsHeader_t));

	free(SectorBuf);

	return 0;
}

// string representation of uint64 -> variable
static uint64_t atoui64(const char *str)
{
	char *endptr;
	uint64_t res;

	errno = 0;

#ifdef _WIN32
	res = _strtoui64(str, &endptr, 10);
#else
	res = strtoull(str, &endptr, 10);
#endif

	if (*endptr != '\0')
		return UINT64_MAX;

	return res;
}

// get file size of the FileNum log file
// synchronization must be provided by the caller!
static uint64_t _lfsGetFileSize(uint64_t FileNum)
{
	UFile fileDsc;
	int64_t ressize;

	assert(FileNum > 0 && FileNum < UINT64_MAX);

	if ((fileDsc = _lfsOpenAndCacheFile(FileNum)) == U_INVALID_FD)
		return UINT64_MAX;

	if (uGetFileSize(fileDsc, &ressize, __sys_call_error) == 0)
	{
    	LFS_ERROR2("lfs error: critical error in lfs chain: cannot get file size", _lfsGetFileName(FileNum).c_str());
    	return UINT64_MAX;
    }

	return ressize;
}

// scans ChainPath for first and last files
static int _lfsGetFirstLastFileNums(uint64_t *FirstNum, uint64_t *LastNum)
{
	UDir lfs_dir;
	UFindDataStruct find_data;
	uint64_t CurFileNum, i;
	int res = -1;
	string fName;
	lfsHeader_t lfsHeader;

	*FirstNum = *LastNum = 0;

	lfs_dir = uFindFirstFile(ChainPath.c_str(), &find_data, __sys_call_error);

	if (lfs_dir == U_INVALID_DIR) // there is no lfs directory
	{
		LFS_ERROR2("lfs error: error in chain path definition", ChainPath.c_str());
		return -1;
	}

	// search for the last file (file with maximum <number> part)
	while (res)
	{
		fName = find_data.fname;
		// check if it is an lfs file and if it contains <number> part
		if (fName.size() > Prefix.size() + Ext.size() + 2 && fName.substr(fName.size() - Ext.size(), Ext.size()) == Ext)
		{
		   	fName.erase(0, Prefix.size() + 1);
	 		fName.erase(fName.size() - Ext.size() - 1, Ext.size() + 1);

			if ((CurFileNum = atoui64(fName.c_str())) == UINT64_MAX || CurFileNum == 0)
       		{
       			LFS_ERROR("lfs internal error: incorrect file format");
       			return -1;
       		}

			if (CurFileNum > *LastNum) *LastNum = CurFileNum;
		}

		if ((res = uFindNextFile(lfs_dir, &find_data, __sys_call_error)) == -1)
        	break;
    }

	if (res != 0 || *LastNum == 0) return -1;

	if (_lfsGetLfsHeaderFromFile(*LastNum, &lfsHeader) != 0)
	{
		LFS_ERROR2("lfs error: cannot read file header", _lfsGetFileName(*LastNum).c_str());
		return -1;
	}

	*FirstNum = lfsHeader.FirstFileNum;

	// check consistency of chain (existence of all files from FirstNum to LastNum)
	for (i = *FirstNum; i < *LastNum; i++)
	{
		fName = _lfsGetFileName(i);

		if (!uIsFileExist(fName.c_str(), __sys_call_error))
		{
		   	LFS_ERROR2("lfs error: critical error in lfs chain (in case of hot-backup restoration check"
    					" if all files have been copied): cannot open file", fName.c_str());
			return -1;
		}
	}

	return 0;
}

// init synchronization stuff (semaphore and shared memory)
static int _lfsInitSync(int BufSize)
{
	int res;

	// create semaphore for synchronization purposes
	res = USemaphoreCreate(&SyncSem, 1, 1, SEDNA_LFS_SEM_NAME, NULL, __sys_call_error);
	if (res != 0)
	{
		LFS_ERROR("lfs error: cannot create semaphore");
		return -1;
	}

	// create shared memory
	res = uCreateShMem(&ShDsc, SEDNA_LFS_SHARED_MEM_NAME, sizeof(lfsInfo_t) + BufSize, NULL, __sys_call_error);

	if (res != 0)
	{
		LFS_ERROR("lfs error: cannot create shared memory");
		return -1;
	}

	// init lfs shared memory pointer
	lfsInfo = (lfsInfo_t *)uAttachShMem(&ShDsc, NULL, 0, __sys_call_error);

	if (lfsInfo == NULL)
	{
		LFS_ERROR("lfs error: cannot attach shared memory");
		return -1;
	}

	return 0;
}

// creates another file in the chain with the given number and writes first HeaderSize bytes into it
// this function is not atomic! there is a possibility of creating a file without a header (due to a crash)
// synchronization (if any) must be provided by the caller!
static int _lfsCreateAnotherFile(uint64_t filenum, void *HeaderBuf, unsigned int HeaderSize)
{
	string fName;
	USECURITY_ATTRIBUTES *sa;
    UFile fdsc;
    int res;
    unsigned int written;

	if(uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error) != 0)
	{
		LFS_ERROR("lfs error: cannot create sa");
		return -1;
	}

	fName = _lfsGetFileName(filenum);

	fdsc = uCreateFile(fName.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_WRITE_THROUGH, sa, __sys_call_error);

	if (fdsc == U_INVALID_FD)
	{
		LFS_ERROR("lfs error: cannot create new chain");
		return -1;
	}

	if (uReleaseSA(sa, __sys_call_error) != 0)
	{
		LFS_ERROR("lfs error: cannot release sa");
		return -1;
	}

	res = uWriteFile(fdsc, HeaderBuf, HeaderSize, &written, __sys_call_error);

	if (res == 0 || written != HeaderSize)
	{
		LFS_ERROR("lfs error: cannot create new chain");
		return -1;
	}

   	if (uCloseFile(fdsc, __sys_call_error) == 0)
   	{
	   	LFS_ERROR("lfs error: cannot close file");
	   	return -1;
	}

	return 0;
}

// extend lfs chain
// return: -1 - error; 0 - all ok
static int _lfsExtend()
{
	lfsHeader_t *lfsHeader;
	void *HeaderBuf;

	assert(lfsInfo != NULL);

	if ((HeaderBuf = malloc(lfsSectorSize)) == NULL)
	{
		LFS_ERROR("lfs error: cannot allocate memory");
		return -1;
	}

	// get header from the last file to write it to the next one
	_lfsGetHeaderFromFile(lfsInfo->LastFileNum, HeaderBuf, lfsSectorSize);

	lfsHeader = (lfsHeader_t *)HeaderBuf;

	lfsHeader->NextLSN = lfsInfo->LastFileNum * lfsInfo->ChunkSize + lfsSectorSize;
	lfsHeader->FirstFileNum = lfsInfo->FirstFileNum;
	lfsHeader->ChunkSize = lfsInfo->ChunkSize;

	if (_lfsCreateAnotherFile(lfsInfo->LastFileNum + 1, HeaderBuf, lfsSectorSize) != 0)
		return -1;

	// close descriptor for the previous last file
	// we need this file to be free during truncate procedure
	_lfsCloseCachedDesc(lfsInfo->LastFileNum);

	lfsInfo->LastFileNum++;

	lfsInfo->NextLSN = lfsHeader->NextLSN;

	free(HeaderBuf);

	return 0;
}

// main flush procedure; flushes buffer until specified ulsn;
// synchronization must be provided by a caller
// if ulsn is greater than the last in the buffer, the entire buffer will be flushed
// lsn isn't checked for consistency; correct lsn should be provided by the caller
// return: -1 - error; 0 - all ok
static int _lfsFlushBufLSN(LSN ulsn)
{
	unsigned int written, portion;
    int res;
    UFile fileDsc;
    uint64_t fileOffs, toflush;

	assert(lfsInfo != NULL);

	if (ulsn <= lfsInfo->NextLSN || lfsInfo->BufKeepBytes == 0) // already flushed
		return 0;

	if (lfsInfo->NextLSN + lfsInfo->BufKeepBytes <= ulsn) // need to flush the entire buffer
		toflush = lfsInfo->BufKeepBytes;
	else
		toflush = ulsn - lfsInfo->NextLSN;

	assert(toflush <= UINT32_MAX);

    // open file for writing
	fileDsc = _lfsOpenAndCacheFile(lfsInfo->LastFileNum);

	// file offset
	fileOffs = lfsInfo->NextLSN % lfsInfo->ChunkSize;

	assert(fileOffs >= lfsSectorSize && fileOffs < lfsInfo->ChunkSize);

	if (uSetFilePointer(fileDsc, fileOffs, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
	{
    	LFS_ERROR("lfs error: critical error in lfs chain: cannot set file pointer");
    	return -1;
    }

	if (lfsInfo->BufStart + toflush <= lfsInfo->BufSize) // we have one continuous part here
	{
		//write toflush bytes
		res = uWriteFile(fileDsc, (char *)lfsWriteBuffer + lfsInfo->BufStart, (unsigned int)toflush, &written, __sys_call_error);
		if (res == 0 || written != toflush)
		{
	    	LFS_ERROR("lfs error: critical error in lfs chain: cannot write into file");
	    	return -1;
	    }

		lfsInfo->BufStart += (unsigned int)toflush;
		if (lfsInfo->BufStart == lfsInfo->BufSize) lfsInfo->BufStart = 0;
	}
	else // we have two continuous portions here: from BufStart to the end, and from the start to BufOffset
	{
		portion = lfsInfo->BufSize - lfsInfo->BufStart;

		// write first portion
		res = uWriteFile(fileDsc, (char *)lfsWriteBuffer + lfsInfo->BufStart, portion, &written, __sys_call_error);
		if (res == 0 || written != portion)
		{
	    	LFS_ERROR("lfs error: critical error in lfs chain: cannot write into file");
	    	return -1;
	    }

		portion = (unsigned int)toflush - written;

		// write second portion
		res = uWriteFile(fileDsc, (char *)lfsWriteBuffer, portion, &written, __sys_call_error);
		if (res == 0 || written != portion)
		{
	    	LFS_ERROR("lfs error: critical error in lfs chain: cannot write into file");
	    	return -1;
	    }

	    lfsInfo->BufStart = portion;
	}

	// correct buffer and lfs metainfo
	assert(lfsInfo->BufKeepBytes >= (unsigned int)toflush);
    lfsInfo->BufKeepBytes -= (unsigned int)toflush;

	lfsInfo->NextLSN += toflush;

	return 0;
}

// truncates beginning of an lfs file chain (chain continuity is presumed)
static int _lfsTruncateChain(uint64_t FirstNum)
{
	uint64_t num = FirstNum;
	string fName;
	int res = 1;

    while (res && --num >= 1)
	{
		fName = _lfsGetFileName(num);
		res = uIsFileExist(fName.c_str(), __sys_call_error);

        if (res)
        {
        	_lfsCloseCachedDesc(num);
        	if (uDeleteFile(fName.c_str(), __sys_call_error) == 0)
        	{
        		LFS_ERROR("lfs error: cannot delete file");
        		return -1;
        	}
        }
    }

    return 0;
}

// init lfs subsystem
int lfsInit(const char *cDataPath, const char *cPrefix, const char *cExt, int WriteBufferSize, int ReadBufferSize)
{
	uint64_t FirstNum, LastNum;
	int res;
	lfsHeader_t lfsHeader;

	// copy file path, prefix and extension
	ChainPath = cDataPath;
	ChainPath.append("/");
	Prefix = cPrefix;
	Ext = cExt;

	// init local cache for file descriptors
	if (_lfsInitFileCache() != 0)
		return -1;

	// take first and last file numbers for chain
	res = _lfsGetFirstLastFileNums(&FirstNum, &LastNum);
	if (res != 0)
	{
		LFS_ERROR("lfs error: file chain does not exist");
		return -1;
	}

	// get file header from the last file
	res = _lfsGetLfsHeaderFromFile(LastNum, &lfsHeader);
	if (res != 0)
	{
		LFS_ERROR("lfs error: cannot read file header");
		return -1;
	}

	// create semaphore for synchronization purposes and allocate shared memory
	res = _lfsInitSync(WriteBufferSize);
	if (res != 0)
	{
		LFS_ERROR("lfs error: cannot initiate synchronizing primitives");
		return -1;
	}

	// init lfsInfo structure
	lfsInfo->NextLSN = lfsHeader.NextLSN;
	lfsInfo->LastFileNum = LastNum;
	lfsInfo->FirstFileNum = FirstNum;
	lfsInfo->ChunkSize = lfsHeader.ChunkSize;
	lfsInfo->BufStart = 0;
	lfsInfo->BufOffset = 0;
	lfsInfo->BufSize = WriteBufferSize;
	lfsInfo->BufKeepBytes = 0;

	// write buffer is already allocated in _lfsInitSync
	lfsWriteBuffer = (char *)lfsInfo + sizeof(lfsInfo_t);

	// allocate read buffer in heap, since it is local
	if ((lfsReadBuf = malloc(ReadBufferSize)) == NULL)
	{
		LFS_ERROR("lfs error: cannot allocate local read buffer");
		return -1;
	}
	ReadBufSize = ReadBufferSize;

	// determine sector size
	if (uGetDiskSectorSize(&lfsSectorSize, cDataPath, __sys_call_error) == 0)
	{
        LFS_WARN("lfs warning: cannot obtain sector size: assuming 512 bytes");
		return 0;
	}

	return 0;
}

// release lfs subsystem
int lfsRelease()
{
	free(lfsReadBuf);

	if (_lfsCloseAllCachedFiles() != 0)
		return -1;

	if (uDettachShMem(&ShDsc, lfsInfo, __sys_call_error) != 0)
	{
		LFS_ERROR("lfs error: cannot detach shared memory");
		return -1;
	}

	if (uReleaseShMem(&ShDsc, SEDNA_LFS_SHARED_MEM_NAME, __sys_call_error) != 0)
	{
		LFS_ERROR("lfs error: cannot release shared memory");
		return -1;
	}

	if (USemaphoreRelease(SyncSem, __sys_call_error) != 0)
	{
		LFS_ERROR("lfs error: cannot release semaphore");
		return -1;
	}

	return 0;
}

// connect to existing lfs
int lfsConnect(const char *cDataPath, const char *cPrefix, const char *cExt, int ReadBufferSize)
{
	// copy file path, prefix and extension
	ChainPath = cDataPath;
	ChainPath.append("/");
	Prefix = cPrefix;
	Ext = cExt;

	// init local cache for file descriptors
	if (_lfsInitFileCache() != 0)
		return -1;

	// open semaphore for synchronization purposes
	if (USemaphoreOpen(&SyncSem, SEDNA_LFS_SEM_NAME, __sys_call_error) != 0)
	{
		LFS_ERROR("lfs error: cannot open semaphore");
		return -1;
	}

	// create shared memory
	if (uOpenShMem(&ShDsc, SEDNA_LFS_SHARED_MEM_NAME, __sys_call_error) != 0)
	{
		LFS_ERROR("lfs error: cannot open shared memory");
		return -1;
	}

	// init lfs shared memory pointer
	lfsInfo = (lfsInfo_t *)uAttachShMem(&ShDsc, NULL, 0, __sys_call_error);
	if (lfsInfo == NULL)
	{
		LFS_ERROR("lfs error: cannot attach shared memory");
		return -1;
	}

	// write buffer is already allocated
	lfsWriteBuffer = (char *)lfsInfo + sizeof(lfsInfo_t);

	if ((lfsReadBuf = malloc(ReadBufferSize)) == NULL)
	{
		LFS_ERROR("lfs error: cannot allocate local read buffer");
		return -1;
	}
	ReadBufSize = ReadBufferSize;

	if (uGetDiskSectorSize(&lfsSectorSize, cDataPath, __sys_call_error) == 0)
	{
        LFS_WARN("lfs warning: cannot obtain sector size: assuming 512 bytes");
		return 0;
	}

	return 0;
}

// disconnect from existing lfs
int lfsDisconnect()
{
	free(lfsReadBuf);

	if (_lfsCloseAllCachedFiles() != 0)
		return -1;

	if (USemaphoreClose(SyncSem,  __sys_call_error) != 0)
	{
		LFS_ERROR("lfs error: cannot close semaphore");
		return -1;
	}

	if (uDettachShMem(&ShDsc, lfsInfo, __sys_call_error) != 0)
	{
		LFS_ERROR("lfs error: cannot detach shared memory");
		return -1;
	}

	if (uCloseShMem(&ShDsc, __sys_call_error) != 0)
	{
		LFS_ERROR("lfs error: cannot close shared memory");
		return -1;
	}

	return 0;
}

// create new lfs chain
// return: -1 - error; 0 - all ok
int lfsCreateNew(const char *cDataPath, const char *cPrefix, const char *cExt, uint64_t cChunkSize, void *HeaderBuf, size_t HeaderSize)
{
	lfsHeader_t *lfsHeader;
	void *SectorBuf;

	// copy file path, prefix and extension
	ChainPath = cDataPath;
	ChainPath.append("/");
	Prefix = cPrefix;
	Ext = cExt;

	// determine sector size
	if (uGetDiskSectorSize(&lfsSectorSize, cDataPath, __sys_call_error) == 0)
	{
        LFS_WARN("lfs warning: cannot obtain sector size: assuming 512 bytes");
		return 0;
	}

	if ((SectorBuf = malloc(lfsSectorSize)) == NULL)
	{
		LFS_ERROR("lfs error: cannot allocate memory");
		return -1;
	}

	memset(SectorBuf, 0, lfsSectorSize);
	memcpy((char *)SectorBuf + sizeof(lfsHeader_t), HeaderBuf, HeaderSize);

	lfsHeader = (lfsHeader_t *)SectorBuf;
	lfsHeader->NextLSN = lfsSectorSize;
	lfsHeader->FirstFileNum = 1;
	lfsHeader->ChunkSize = cChunkSize;

	if (_lfsCreateAnotherFile(1, SectorBuf, lfsSectorSize) != 0)
		return -1;

	free(SectorBuf);

	return 0;
}

// gets data from write buffer
// Parameters:
// 		buf - where to copy (must be allocated by the caller)
//      len - how many bytes to copy
//      wb_off - offset from the beginning of the write buffer
static int _lfsGetDataFromBuffer(void *buf, unsigned int len, unsigned int wb_off)
{
	int portion = 0;
	int offs = 0;

	assert(buf != NULL);
	assert(len <= lfsInfo->BufKeepBytes);
	assert(wb_off < lfsInfo->BufKeepBytes);

	// offset must be wrapped over start
	if (lfsInfo->BufStart + wb_off >= lfsInfo->BufSize)
		offs = wb_off - lfsInfo->BufSize + lfsInfo->BufStart;
	else // offset is continuous
		offs = lfsInfo->BufStart + wb_off;

	if (offs + len <= lfsInfo->BufSize) // we have one continuous part here
	{
		//write len bytes
		memcpy(buf, (char *)lfsWriteBuffer + offs, len);
	}
	else // we have two continuous portions here: from BufStart + wb_off to the end, and from the start to the ...
	{
		portion = lfsInfo->BufSize - offs;

		memcpy(buf, (char *)lfsWriteBuffer + offs, portion);
		memcpy((char *)buf + portion, lfsWriteBuffer, len - portion);
	}

	return 0;
}

// get data at RecLSN and write it to the RecBuf;
// RecSize bytes will be written; RecBuf must be allocated by the caller
// return: -1 - error; number of bytes read (0 - lfs is out of bound)
int lfsGetRecord(LSN *RecLSN, void *RecBuf, unsigned int RecSize)
{
	uint64_t fileNum, fileOffs, fileSize;
	UFile fileDsc;
	unsigned int readbytes;
    int res;

	assert(lfsInfo != NULL);

	lfsLock();

	// number of file with record
	fileNum = *RecLSN / lfsInfo->ChunkSize + 1;

	// file offset
	fileOffs = *RecLSN % lfsInfo->ChunkSize;
	if (fileOffs == 0)
	{
		fileOffs = lfsInfo->ChunkSize;
		fileNum--;
	}

	// check if lfs can be read
	if (fileNum == lfsInfo->LastFileNum)
	{
		// check if record is still in the write buffer
		if (*RecLSN >= lfsInfo->NextLSN && *RecLSN + RecSize <= lfsInfo->NextLSN + lfsInfo->BufKeepBytes)
		{
			_lfsGetDataFromBuffer(RecBuf, RecSize, (unsigned int)(*RecLSN - lfsInfo->NextLSN));
			lfsUnlock();
			return RecSize;
		}
	}

	// lsn is way out of reach
	if (fileNum > lfsInfo->LastFileNum)
	{
		lfsUnlock();
		return 0;
	}

    // get file size
    if ((fileSize = _lfsGetFileSize(fileNum)) == UINT64_MAX)
	{
    	lfsUnlock();
   		return -1;
    }

    // jump to the next file if needed
    if (fileOffs >= fileSize)
    {
    	fileNum++;
    	fileOffs = lfsSectorSize + fileOffs - fileSize;
    	*RecLSN = (fileNum - 1) * lfsInfo->ChunkSize + fileOffs;
    }

	// check if lfs can be read
	if (fileNum > lfsInfo->LastFileNum)
	{
    	lfsUnlock();
   		return 0;
    }

	// check if record is in the read buffer
	if (*RecLSN >= StartReadLSN && *RecLSN + RecSize <= EndReadLSN)
	{
		memcpy(RecBuf, (char *)lfsReadBuf + (*RecLSN - StartReadLSN), RecSize);
		lfsUnlock();
		return RecSize;
	}

	// if not in buffers, read from file
	if ((fileDsc = _lfsOpenAndCacheFile(fileNum)) == U_INVALID_FD)
	{
		lfsUnlock();
		return -1;
	}

	if (uSetFilePointer(fileDsc, fileOffs, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
	{
    	LFS_ERROR("lfs error: critical error in lfs chain: cannot set file pointer");
    	lfsUnlock();
    	return -1;
    }

	if (RecSize > ReadBufSize)
	{
		res = uReadFile(fileDsc, RecBuf, RecSize, &readbytes, __sys_call_error);
		if (res == 0 || readbytes != RecSize)
		{
			LFS_ERROR("lfs error: error in reading file");
			lfsUnlock();
			return -1;
		}
	}
	else
	{
		res = uReadFile(fileDsc, lfsReadBuf, ReadBufSize, &readbytes, __sys_call_error);
		if (res == 0)
		{
			LFS_ERROR("lfs error: error in reading file");
			lfsUnlock();
			return -1;
		}

		StartReadLSN = *RecLSN;
		EndReadLSN = *RecLSN + readbytes;

		memcpy(RecBuf, lfsReadBuf, RecSize);
	}

	lfsUnlock();

	return RecSize;
}

// write data from RecBuf; RecSize - size of data in bytes
// data is appended to the end of the storage
// return: LSN after the written data (LSN of record + record size; equal to current lsn pointer); LFS_INVALID_LSN in case of error
LSN lfsAppendRecord(void *RecBuf, unsigned int RecSize)
{
	uint64_t fileOffs;
	int FirstRecPart, SecondRecPart;
/*
	UFile fileDsc;
    uint64_t FilesNum;
*/
	uint64_t BytesLeft;
	LSN resLSN;

	lfsLock();

	assert(lfsInfo != NULL && lfsInfo->BufSize >= RecSize);

	// would be lsn in case of flush of the current buffer
	resLSN = lfsInfo->NextLSN + lfsInfo->BufKeepBytes;

	if ((fileOffs = resLSN % lfsInfo->ChunkSize) == 0)
		fileOffs = lfsInfo->ChunkSize;

	// bytes left in the current file
	BytesLeft = lfsInfo->ChunkSize - fileOffs;

	// check if we need to add another file
	if (BytesLeft < RecSize)
	{
		if (_lfsFlushBufLSN(lfsInfo->NextLSN + lfsInfo->BufKeepBytes) != 0)
		{
			lfsUnlock();
			return LFS_INVALID_LSN;
		}

		if (_lfsExtend() != 0)
		{
			lfsUnlock();
			return LFS_INVALID_LSN;
		}
	}

    // check if buffer is full
    if (lfsInfo->BufSize - lfsInfo->BufKeepBytes < RecSize)
	{
		if (_lfsFlushBufLSN(lfsInfo->NextLSN + lfsInfo->BufKeepBytes) != 0)
		{
			lfsUnlock();
			return LFS_INVALID_LSN;
		}
	}

	if (lfsInfo->BufOffset < lfsInfo->BufStart || lfsInfo->BufSize - lfsInfo->BufOffset >= RecSize)
	{
		memcpy((char *)lfsWriteBuffer + lfsInfo->BufOffset, RecBuf, RecSize);
		lfsInfo->BufOffset += RecSize;
		if (lfsInfo->BufOffset == lfsInfo->BufSize) lfsInfo->BufOffset = 0;
    }
    else
    {
   		FirstRecPart  = lfsInfo->BufSize - lfsInfo->BufOffset;
   		SecondRecPart = RecSize - FirstRecPart;

   		memcpy((char *)lfsWriteBuffer + lfsInfo->BufOffset, RecBuf, FirstRecPart);
   		memcpy(lfsWriteBuffer, (char *)RecBuf + FirstRecPart, SecondRecPart);

		lfsInfo->BufOffset = SecondRecPart;
	}

	resLSN = lfsInfo->NextLSN + lfsInfo->BufKeepBytes;

	lfsInfo->BufKeepBytes += RecSize;

	assert(lfsInfo->BufKeepBytes <= lfsInfo->BufSize);

	lfsUnlock();

	return resLSN;
}

// writes lfs+user header
static int _lfsWriteSectorHeader(uint64_t fileNum, lfsHeader_t lfsHeader, void *HeaderBuf, size_t HeaderSize)
{
	UFile fileDsc;
	unsigned int written;
    int res;
	void *lfsHeaderBuf;

	if (HeaderSize > lfsSectorSize - sizeof(lfsHeader_t))
	{
    	LFS_ERROR("lfs error: critical error in lfs chain: too large user header in lfs");
    	return -1;
    }

	if ((lfsHeaderBuf = malloc(lfsSectorSize)) == NULL)
	{
		LFS_ERROR("lfs error: cannot allocate header buffer");
		return -1;
	}

	// prepare header to flush: two parts, first - lfsHeader, second - user header from HeaderBuf
	memset(lfsHeaderBuf, 0, lfsSectorSize);
	memcpy(lfsHeaderBuf, &lfsHeader, sizeof(lfsHeader_t));
	memcpy((char *)lfsHeaderBuf + sizeof(lfsHeader_t), HeaderBuf, HeaderSize);

	if ((fileDsc = _lfsOpenAndCacheFile(fileNum)) == U_INVALID_FD)
	{
		free(lfsHeaderBuf);
		return -1;
	}

	if (uSetFilePointer(fileDsc, 0, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
	{
		free(lfsHeaderBuf);
    	LFS_ERROR("lfs error: critical error in lfs chain: cannot set file pointer");
    	return -1;
    }

	res = uWriteFile(fileDsc, lfsHeaderBuf, lfsSectorSize, &written, __sys_call_error);

	if (res == 0 || written != lfsSectorSize)
	{
		free(lfsHeaderBuf);
    	LFS_ERROR("lfs error: critical error in lfs chain: cannot read user header");
    	return -1;
    }

	free(lfsHeaderBuf);

	return 0;
}

// archive current log file (switch to another file)
// return: LFS_INVALID_FILE - error; archived log file number - success
uint64_t lfsArchiveCurrentFile(void *HeaderBuf, size_t HeaderSize)
{
	uint64_t res;
	int64_t ressize;
	UFile fileDsc;
	lfsHeader_t lfsHeader;

	lfsLock();

	if (_lfsFlushBufLSN(lfsInfo->NextLSN + lfsInfo->BufKeepBytes) != 0) // flush the entire buffer
	{
		lfsUnlock();
        return LFS_INVALID_FILE;
	}

	// check if need to extend file
	if ((fileDsc = _lfsOpenAndCacheFile(lfsInfo->LastFileNum)) == U_INVALID_FD)
	{
		lfsUnlock();
        return LFS_INVALID_FILE;
	}

	if (uGetFileSize(fileDsc, &ressize, __sys_call_error) == 0)
	{
    	LFS_ERROR("lfs error: critical error in lfs chain: cannot get last file size");
		lfsUnlock();
        return LFS_INVALID_FILE;
	}

	if (ressize > lfsSectorSize) // file contains something except header
	{
		// extend file
		if (_lfsExtend() != 0)
		{
			lfsUnlock();
            return LFS_INVALID_FILE;
		}

		if (HeaderBuf != NULL) // we need to update user header there
		{
			// get header from "old" file
			if (_lfsGetLfsHeaderFromFile(lfsInfo->LastFileNum - 1, &lfsHeader) != 0)
			{
				LFS_ERROR("lfs error: cannot read file header");
				lfsUnlock();
                return LFS_INVALID_FILE;
			}

			// write lfs+provided user header in it
			if (_lfsWriteSectorHeader(lfsInfo->LastFileNum - 1, lfsHeader, HeaderBuf, HeaderSize) != 0)
			{
				LFS_ERROR("lfs error: cannot write file header");
				lfsUnlock();
                return LFS_INVALID_FILE;
			}
		}
	}

	res = lfsInfo->LastFileNum - 1;

	lfsUnlock();

	return res;
}

// write user file header + lfs file header; lfsSectorSize bytes are written
// return: -1 - error; 0 - success;
int lfsWriteHeader(void *HeaderBuf, size_t HeaderSize)
{
	lfsHeader_t lfsHeader;

	lfsLock();

	lfsHeader.NextLSN = lfsInfo->NextLSN;
	lfsHeader.FirstFileNum = lfsInfo->FirstFileNum;
	lfsHeader.ChunkSize = lfsInfo->ChunkSize;

	_lfsWriteSectorHeader(lfsInfo->LastFileNum, lfsHeader, HeaderBuf, HeaderSize);

	lfsUnlock();

	return 0;
}

// read user file header in HeaderBuf; first HeaderSize bytes are fetched
// HeaderBuf must be allocated by the caller
// return: -1 - error; 0 - success
int lfsGetHeader(void *HeaderBuf, size_t HeaderSize)
{
	void *SectorBuf;

	lfsLock();

	if ((SectorBuf = malloc(lfsSectorSize)) == NULL)
	{
		LFS_ERROR("lfs error: cannot allocate memory");
		return -1;
	}

	_lfsGetHeaderFromFile(lfsInfo->LastFileNum, SectorBuf, lfsSectorSize);

	memcpy(HeaderBuf, (char *)SectorBuf + sizeof(lfsHeader_t), HeaderSize);

	lfsUnlock();

	free(SectorBuf);

	return 0;
}

// get previous file number in lsf chain
// return: LFS_INVALID_FILE - previous file doesn't exist; file number - success
uint64_t lfsGetPrevFileNumber(uint64_t FileNum)
{
	uint64_t res;

	lfsLock();

	if (lfsInfo->FirstFileNum >= FileNum)
		res = LFS_INVALID_FILE;
	else
		res = FileNum - 1;

	lfsUnlock();

	return res;
}

// Flush portion of written records until specified lsn.
// Record at specified lsn will not be writen, only data prior to UntilLSN will be.
int lfsFlush(LSN UntilLSN)
{
	int res;

	lfsLock();

	res = _lfsFlushBufLSN(UntilLSN);

	lfsUnlock();

	return res;
}

// Flush all written records to disk.
int lfsFlushAll()
{
	int res;

	assert(lfsInfo != NULL);

	lfsLock();

	// flush the entire buffer, since NextLSN + buffer_length is greater than any existing lsn
	res = _lfsFlushBufLSN(lfsInfo->NextLSN + lfsInfo->BufKeepBytes);

	lfsUnlock();

	return res;
}

// Close all open log files.
int lfsCloseAllFiles()
{
	_lfsCloseAllCachedFiles();

	return 0;
}

// Truncates file chain prior to specified lsn
int lfsTruncate(LSN UntilLSN, uint64_t hint_num)
{
	int res = 0;
	lfsHeader_t *lfsHeader;
	void *HeaderBuf;

	assert(lfsInfo != NULL);

	lfsLock();

	assert(hint_num == 0 || (lfsInfo->FirstFileNum <= hint_num && lfsInfo->LastFileNum >= hint_num));

	lfsInfo->FirstFileNum = UntilLSN / lfsInfo->ChunkSize + 1;

	// if hint was provided, use it
	if (hint_num != 0 && lfsInfo->FirstFileNum > hint_num)
		lfsInfo->FirstFileNum = hint_num;

	if ((HeaderBuf = malloc(lfsSectorSize)) == NULL)
	{
		LFS_ERROR("lfs error: cannot allocate memory");
		return -1;
	}

	_lfsGetHeaderFromFile(lfsInfo->LastFileNum, HeaderBuf, lfsSectorSize);

	lfsHeader = (lfsHeader_t *)HeaderBuf;

	lfsHeader->NextLSN = lfsInfo->NextLSN;
	lfsHeader->FirstFileNum = lfsInfo->FirstFileNum;
	lfsHeader->ChunkSize = lfsInfo->ChunkSize;

	_lfsWriteSectorHeader(lfsInfo->LastFileNum, *lfsHeader, (char *)HeaderBuf + sizeof(lfsHeader_t),
		lfsSectorSize - sizeof(lfsHeader_t));

	res = _lfsTruncateChain(lfsInfo->FirstFileNum);

	lfsUnlock();

	free(HeaderBuf);

	return res;
}

// Returns number of lfs files
uint64_t lfsGetNumberOfFiles()
{
	uint64_t res;

	assert(lfsInfo != NULL);

	lfsLock();

	res = lfsInfo->LastFileNum - lfsInfo->FirstFileNum + 1;

	lfsUnlock();

	return res;
}

// Returns higher LSN boundary (lsn >= this don't exist)
LSN lfsGetHighLSNBoundary()
{
    LSN res;

    assert(lfsInfo != NULL);

    lfsLock();

    res = lfsInfo->NextLSN + lfsInfo->BufKeepBytes;

    lfsUnlock();

    return res;
}
