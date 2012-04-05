/*
 * File:  lfsStorage.h - LFS (linear file storage) subsystem.
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Only the user interface is specified here. For further information refer to lfsStorage.cpp file.
 *
 */

#ifndef _LFS_STORAGE_
#define _LFS_STORAGE_

#include "lfsGlobals.h"

// Init lfs subsystem; this function must be called only once to initialze lfs subsystem.
// All additional users must call lfsConnect function.
// Parameters:
//     cDataPath - full path where files are stored; file format: <prefix>.<some_number>.<extension>
//     cPrefix   - prefix for files
//     cExt      - extension
//     WriteBufferSize - size of the shared write-buffer
//     ReadBufferSize  - size of the read-buffer on the current process
// Returns:
// 	   0 - all ok; -1 - some error
int lfsInit(const char *cDataPath, const char *cPrefix, const char *cExt, int WriteBufferSize, int ReadBufferSize);

// Release existing lfs. This should be used only as a pair to lfsInit function. 
// Returns:
// 	   0 - all ok; -1 - some error
int lfsRelease();

// Create new lfs chain; This function mus be called prior to any using of lfs chain (lfsInit call).
// Parameters:
//     cDataPath - full path where files are stored; file format: <prefix>.<some_number>.<extension>
//     cPrefix   - prefix for files
//     cExt      - extension
//     cChunkSize - maximum size of lfs file in the chain (chunk of file chain)
// Returns: 
//     -1 - error; 0 - all ok
int lfsCreateNew(const char *cDataPath, const char *cPrefix, const char *cExt, uint64_t cChunkSize, void *HeaderBuf, size_t HeaderSize);

// Connect to existing lfs subsytem. lfsInit must be called on some other process prior to lfsConnect.
// Parameters:
//     cDataPath - full path where files are stored; file format: <prefix>.<some_number>.<extension>
//     cPrefix   - prefix for files
//     cExt      - extension
//     ReadBufferSize  - size of the read-buffer on the current process
// Returns:
// 	   0 - all ok; -1 - some error
int lfsConnect(const char *cDataPath, const char *cPrefix, const char *cExt, int ReadBufferSize);

// Disconnect from existing lfs. This should be used only as a pair to lfsConnect function.
// Returns:
// 	   0 - all ok; -1 - some error
int lfsDisconnect();

// Get data at RecLSN and write it to the RecBuf;
// RecSize bytes will be written; RecBuf must be allocated by the caller.
// Important: RecLSN may be corrected during execution to point on requested record (this may happen during sequential scan).
// Parameters:
//     RecLSN  - pointer to lsn of needed record
//     RecBuf  - allocated buffer to write a record
//     RecSize - size of a record
// Return: 
//     -1 - error; number of bytes read (0 - lfs is out of bound)
int lfsGetRecord(LSN *RecLSN, void *RecBuf, unsigned int RecSize);

// Write data from RecBuf; RecSize - size of data in bytes
// Data can be stored in memory. To guarantee write on disk, lfsFlush should be used.
// Parameters:
// Parameters:
//     RecBuf  - allocated buffer to write a record
//     RecSize - size of a record
// Returns: 
//     LSN of the first byte after the written data (new position of cursor, lsn of record would be: returned lsn - RecSize); 
//     LFS_INVALID_LSN in case of error;
LSN lfsAppendRecord(void *RecBuf, unsigned int RecSize);

// Archive current log file (switch to another file). This guarantees that all current written data will be flushed
// on disk, and that all files so far will not be written again (example of use would be hot-backup procedure)
// The returned value can be used to copy archived part of file chain during hot-backup process, see lfsGetPrevFileNumber also.
//
// Parameters:
//	   HeaderBuf - buffer containing header to write in last archived file
//	   HeaderSize - size of header
// Return: 
//     LFS_INVALID_FILE - error; 
//     archived log file number - success;
uint64_t lfsArchiveCurrentFile(void *HeaderBuf, size_t HeaderSize);

// Flush portion of written records until specified lsn. 
// Record at specified lsn will not be writen, only data prior to UntilLSN will be.
// Parameters:
//     UntilLSN - cursor position specifying boundary of flushing.
// Returns:
//     0  - all ok;
//     -1 - some error;
int lfsFlush(LSN UntilLSN);

// Flush all written records to disk.
// Returns:
//     0  - all ok;
//     -1 - some error;
int lfsFlushAll();

// Close all open log files.
// Returns:
//     0  - all ok;
//     -1 - some error;
int lfsCloseAllFiles();

// Truncates file chain prior to specified lsn.
// Record at the specified lsn will stay alive, but all data prior to it will be purged.
// Should be used for file-chain maintenance to trunncate too long file-chains.
// Parameters:
//     UntilLSN - cursor position specifying boundary of truncating.
//     hint_num - hint: don't truncate beyond this file
// Returns:
//     0  - all ok;
//     -1 - some error;
int lfsTruncate(LSN UntilLSN, uint64_t hint_num);

// Write user file header from HeaderBuf; HeaderSize bytes will be written.
// Parameters:
//     HeaderBuf  - header 
//     HeaderSize - size of a header
//     hint_num   - number of file where to write a header (if 0, the in the last)
// Returns:
//     0  - all ok;
//     -1 - some error;
// return: -1 - error; 0 - success;
int lfsWriteHeader(void *HeaderBuf, size_t HeaderSize);

// Read file header in HeaderBuf; first HeaderSize bytes are fetched.
// HeaderBuf must be allocated by the caller.
// Parameters:
//     HeaderBuf  - buffer to write a header to
//     HeaderSize - size of a header
// Returns:
//     0  - all ok;
//     -1 - some error;
int lfsGetHeader(void *HeaderBuf, size_t HeaderSize);

// Get previous file number in lsf chain. Can be used to determine part of file chain to backup during hot-backup process.
// Should be used in collaboration with lfsArchiveCurrentFile.
// Parameters:
//     FileNum - file number
// Returns:
//     file number - success;
//     -1 - previous file doesn't exist;
uint64_t lfsGetPrevFileNumber(uint64_t FileNum);

// Returns number of lfs files (can be used to make decision about truncating)
// Returns:
//     number of files
uint64_t lfsGetNumberOfFiles();

// Returns higher LSN boundary (lsn >= this don't exist)
// Parameters:
//      None
// Returns:
//      high boundary LSN
LSN lfsGetHighLSNBoundary();

#endif
