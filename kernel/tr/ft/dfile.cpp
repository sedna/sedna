#include <stdafx.h>
#ifndef DFILE_H
#	include <dfile.h>
#endif
#include <limits.h>
#if defined(__UNIX__)
	#include <unistd.h>
    #include <fcntl.h>
    #include <sys/stat.h>
    #include <time.h>
    #define O_BINARY 0   //  Not used
    #include <utime.h>
    #include <sys/mman.h>
#else
	#include <stdlib.h>
	#include <io.h>
    #include <fcntl.h>
    #include <sys/stat.h>
    #include <time.h>
	#include <share.h>
    #include <critsec.h>

#endif

#include <errno.h>

#ifndef _WIN32
    #define INVALID_HANDLE_VALUE  ((t_FileHandle)(-1))
#endif

#ifdef USE_DTSEARCH_NAMESPACE
using namespace dtSearch;
#endif

#pragma unmanaged

void *DMemoryMappedFile::mapro()
{	if (m_pData)
		return m_pData;
#ifdef _WIN32
	m_hMapping = CreateFileMapping(this->handle, 0, PAGE_READONLY, 0, 0, 0);
	if (m_hMapping)
		m_pData = MapViewOfFile(m_hMapping, FILE_MAP_READ, 0, 0, 0);
	else {
		CloseHandle(m_hMapping);
		m_hMapping = 0;
		}
#else
	m_pData = mmap(0, getLength(), PROT_READ, MAP_PRIVATE, handle, 0);
#endif
	return m_pData;
}

void DMemoryMappedFile::unmap()
{	if (!m_pData)
		return;
#ifdef _WIN32
	UnmapViewOfFile(m_pData);
	CloseHandle(m_hMapping);
	m_hMapping = 0;
#else
	munmap(m_pData, getLength());
#endif
	m_pData = 0;
}

#if defined(_WIN32) || defined(_WIN32_UNIX)
void DFileDate::convertTo(SYSTEMTIME& st, long fMakeLocal) const
{	memset(&st, 0, sizeof st);
	st.wDay = day;
	st.wHour = hour;
	st.wMinute = minute;
	st.wMonth = month;
	st.wSecond = second;
	st.wYear = year;

	if (fMakeLocal) {
		FILETIME utcft, localft;
		memset(&utcft, 0, sizeof utcft);
		memset(&localft, 0, sizeof localft);
		SystemTimeToFileTime(&st, &utcft);
		FileTimeToLocalFileTime(&utcft, &localft);
		FileTimeToSystemTime(&localft, &st);
		}
}

void DFileDate::convertTo(FILETIME& ft, long fMakeLocal) const
{	SYSTEMTIME st;
	convertTo(st, false);
	memset(&ft, 0, sizeof ft);
	if (fMakeLocal) {
		FILETIME utcft;
		memset(&utcft, 0, sizeof utcft);
		SystemTimeToFileTime(&st, &utcft);
		FileTimeToLocalFileTime(&utcft, &ft);
		}
	else
		SystemTimeToFileTime(&st, &ft);
}


void DFileDate::convertFrom(FILETIME& ft)
{   SYSTEMTIME st;
    FileTimeToSystemTime(&ft, &st);
    convertFrom(st);
}

void DFileDate::convertFrom(SYSTEMTIME& st)
{   day = (char) st.wDay;
    month = (char) st.wMonth;
    year = st.wYear;
    hour = (char) st.wHour;
    minute = (char) st.wMinute;
    second = (char) st.wSecond;
}
#endif


#ifdef MfcAvailable
CTime DFileDate::convertToCTime(long fMakeLocal) const
{	SYSTEMTIME st;
	convertTo(st, fMakeLocal);
	CTime ret(st);
	return ret;
}

void DFileDate::formatDate(CString& s, long fMakeLocal) const
{	SYSTEMTIME st;
	convertTo(st, fMakeLocal);

	const int bufSize = 40;
	TCHAR *buf = s.GetBuffer(bufSize);
	GetDateFormat(LOCALE_USER_DEFAULT, DATE_SHORTDATE, &st, 0, buf, bufSize);
	s.ReleaseBuffer();
}

void DFileDate::formatTime(CString& s, long fMakeLocal) const
{	SYSTEMTIME st;
	convertTo(st, fMakeLocal);

	const int bufSize = 40;
	TCHAR *buf = s.GetBuffer(bufSize);
	GetTimeFormat(LOCALE_USER_DEFAULT, 0, &st, 0, buf, bufSize);
	s.ReleaseBuffer();

}
#endif

void DFileDate::formatISO(DString& dest) const
{	char s[80];
	sprintf(s, "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2dZ", year, month, day, hour, minute, second);
	dest << s;
}

void DFileDate::parseISO(const char *s)
{	clear();
	DStringSet temp;
	temp.tokenize(s, " /:-Z");
	if (temp.getCount() == 6) {
		year = (char) atol(temp.getString(0));
		month = (char) atol(temp.getString(1));
		day = (char) atol(temp.getString(2));
		hour = (char) atol(temp.getString(3));
		minute = (char) atol(temp.getString(4));
		second = (char) atol(temp.getString(5));
		}
}

void DFileDate::convertTo(struct tm & t) const
{	memset(&t, 0, sizeof t);
	t.tm_hour = hour;
	t.tm_min = minute;
	t.tm_sec = second;
	t.tm_year = year;
	t.tm_mon = month - 1;  // tm uses 0-11 for months
	t.tm_mday = day;
}

void DFileDate::convertFrom(struct tm & t)
{	hour = (char) t.tm_hour;
	minute = (char) t.tm_min;
	second = (char) t.tm_sec;
	year = (unsigned short) t.tm_year;
	month = (char) t.tm_mon + 1;
	day = (char) t.tm_mday;
}

void DFileDate::utcToLocal()
{
#ifdef _WIN32
	SYSTEMTIME st;
	convertTo(st, true);
	convertFrom(st);
#else
	struct tm t;
	convertTo(t);
	time_t tt = mktime(&t);
	struct tm *pt = localtime_r(&tt, &t);
	if (pt)
		convertFrom(*pt);
#endif
}

void DFileDate::nowUTC() {
#if defined(_WIN32) && (_MSC_VER > 1200)
	__time64_t t = _time64(0);
	struct tm *gmt = _gmtime64(&t);
	convertFrom(*gmt);
#else
	time_t tt = time(0);
	struct tm *pt = gmtime(&tt);
	if (pt)
		convertFrom(*pt);
#endif
}

void DFileDate::now() {
#if defined(_WIN32) && (_MSC_VER > 1200)
	__time64_t t = _time64(0);
	struct tm *gmt = _localtime64(&t);
	convertFrom(*gmt);
#else
    time_t tt = time(0);
    struct tm *pt = localtime(&tt);
    if (pt)
        convertFrom(*pt);
#endif
	}

void DFileDate::formatLocal(DString& dest) const
{	DFileDate localDate;
	localDate.copy(*this);
	localDate.utcToLocal();

	struct tm t;
	localDate.convertTo(t);

	char temp[80];
	temp[0] = '\0';
	strftime(temp, sizeof temp, "%c", &t);
	dest = temp;
}

static long closeCount = 0;
static long openCount = 0;


DFile::DFile()
{   clear();
}

int DFile::getLastError()
{   return error;
}

DFile::~DFile()
{
    close();
}

int DFile::flushFileBuffers()
{
#ifdef _WIN32
	if (good()) {
		bool ok = (FlushFileBuffers(handle) ? true : false);
		if (ok)
			return SUCCESS;
		saveError();
		}
	return FAIL;
#else
	fsync(handle);
	return SUCCESS;
#endif
}


void DFile::clear()
{   handle = DFile_BadHandle;
	bShouldCloseHandle = false;
	bShouldDeleteOnClose = false;
    rwFlags = 0;
    shareMode = 0;
    memset(&flags, 0, sizeof flags);
    error = 0;
    where = 0;
    savedPosition = 0;
    length = 0;
    name.clear();
    accessMode = 0;
}

char *DFile::getName()
{   return name.str();
}

DFilePosition DFile::tell()
{   return where;
}

void DFile::attach(t_FileHandle aHandle, int access, bool fShouldClose)
{   clear();
    handle = aHandle;
    bShouldCloseHandle = fShouldClose;
    storeAccessFlags(access);
    if (good())
        getHandleInfo();
}

void DFile::storeAccessFlags(int access)
{   flags.read = (access & F_READ? 1 : 0);
    flags.write = (access & F_WRITE? 1 : 0);
    flags.create = (F_IsCreate(access) ? 1 : 0);
    flags.trunc = (access & F_TRUNC? 1 : 0);
    flags.restoreLastAccess = (access & F_NO_CHANGE_LASTACCESS? 1 : 0);

    flags.rshare = 0;
    flags.wshare = 0;
    if (!flags.write)
        flags.rshare = true;
    if (access & F_SH_ALLOWWRITE)
        flags.wshare = true;
    if (access & F_SH_ALLOWREAD)
        flags.rshare = true;
    if (access & F_CONCURRENT) {
        flags.rshare = true;
        if (!flags.write)
            flags.wshare = true;
        }

    // If not opening for write, force sharing of read access
    if (!(access & F_WRITE))
        flags.rshare = 1;
    accessMode = access;
}

static const char *TempFilePrefix = "~~dtSearchTemp";
int DFile::createTemp(const char *namesuffix, bool bDeleteOnClose) {
	DString n;
	n << TempFilePrefix << "_" << namesuffix;
#ifdef __UNIX__	
	n.hex((long) getpid());
#else
	n.hex((long) GetCurrentProcessId());
#endif
	n << "_";
	__int64 t = (long) time(0);
	__int64 c = (long) clock();
	__int64 num = 100000 * (t % 100000) + (c % 100000);
	n.hex((long) this);
	n.hex(num);
	DFilename tmpname;
	tmpname.makeTempName(n);
	tmpname.setExt(".tmp");
	bShouldDeleteOnClose = bDeleteOnClose;
	return openU8(tmpname, F_TRUNC | F_ANY | F_SH_ALLOWREAD);
	}



int DFile::open(const char *to_open, int access)
{   name = to_open;

    storeAccessFlags(access);

    openHandle();

    if (good()) {
        where = 0L;
        if (flags.write && (access & F_TRUNC))
            setLength(0);
        return SUCCESS;
        }
    else
        return FAIL;
}

int DFile::openUi(const TCHAR *uiName, int access) {
	return openU8(UiToUtf8(uiName), access);
}

void DFile::close()
{
    closeHandle();
    
    if (bShouldDeleteOnClose && flags.write && name.contains(TempFilePrefix))
		DeleteFileU8(name);

    flags.is_frozen = false;
    accessMode = 0;
}

int DFile::isOpen(void)
{
    return good();
}

DFilePosition DFile::read64(void *vbuf, DFilePosition len)
{	char *buf = (char *) vbuf;
    DFilePosition v;
    if (where >= length) {
        memset(buf, 0, (size_t) len);
        return FAIL;
        }

    if (flags.read) {
        // If we are reading past the end of the DFile
        // then fill the remainder with NULLs
        if (where + len > length) {
            DFilePosition readLen = length - where;  // has to be positive

            v = os_read(buf, readLen);
            if (readLen < len)
                memset(buf + readLen, 0, (size_t) (len - readLen));
            }
        else
            v = os_read(buf, len);
        if (v < 0)
            saveError();
        else
			where += v;
        return v;
        }
    else {
        memset(buf, 0, (size_t) len);
        return FAIL;
        }
}

DFilePosition DFile::write64(const void *buf, DFilePosition len)
{   if (flags.write) {
        if (handle < 0)
            return FAIL;
		DFilePosition bytesWritten = os_write(buf, len);
        if (bytesWritten < 0)
            saveError();
        else
			where += bytesWritten;
        if (length < where)
            length = where;
        return bytesWritten;
        }
    else
        return FAIL;
}


DFilePosition DFile::getLength()
{   return length;
}

///////////////////////////////////////////////////////////////////
//
//  Environment-dependent interfaces
//

void DFile::saveError()
{
#ifdef _WIN32
    error = GetLastError();
#ifndef UNDER_CE
    errno = error;
#endif
#else
    error = errno;
#endif
}

int DFile::good()
{   return handle != (t_FileHandle) INVALID_HANDLE_VALUE;
}

#if defined(_WIN32)

void DFile::openHandle()
{   closeHandle();

    rwFlags = 0;
    if (flags.read)
        rwFlags |= GENERIC_READ;
    if (flags.write)
        rwFlags |= GENERIC_WRITE;
    shareMode = 0;
    if (flags.rshare)
        shareMode |= FILE_SHARE_READ;
    if (flags.wshare)
        shareMode |= FILE_SHARE_WRITE;

    int createMode;
    if (flags.trunc && flags.create)
        createMode = CREATE_ALWAYS;
    else if (flags.trunc)
        createMode = TRUNCATE_EXISTING;
    else if (flags.create)
        createMode = OPEN_ALWAYS;
    else
        createMode = OPEN_EXISTING;

    handle = CreateFileU8(name, rwFlags, shareMode, 0, createMode,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS, 0);

    if (good()) {
        openCount++;
        bShouldCloseHandle = true;
        getHandleInfo();
        }
    else
        saveError();

}

#else

void DFile::openHandle()
{
    closeHandle();

    // Sharing flags
    shareMode = 0;
#ifndef __UNIX__
    if (!flags.rshare && !flags.wshare)
        shareMode = SH_DENYRW ;
    else if (!flags.rshare)
        shareMode = SH_DENYRD;
    else if (!flags.wshare)
        shareMode = SH_DENYWR;
    else
        shareMode = SH_DENYNO;
#endif

    // Read/write flags
    rwFlags = 0;
    if (flags.read && flags.write)
        rwFlags = O_RDWR;
    else if (flags.read)
        rwFlags = O_RDONLY;
    else if (flags.write)
        rwFlags = O_WRONLY;

    if (flags.create)
        rwFlags |= O_CREAT;
    if (flags.trunc)
        rwFlags |= O_TRUNC;
    int permission = 0;
    if (flags.create)
    	permission = ( S_IREAD | S_IWRITE | S_IRGRP | S_IROTH); // 644 permission

#ifdef LARGEFILES
    rwFlags |= O_LARGEFILE;
    handle = ::open64(name, O_BINARY | rwFlags | shareMode, permission);
#else
    handle = ::open(name, O_BINARY | rwFlags | shareMode, permission);
#endif

    if (good()) {
        openCount++;
        bShouldCloseHandle = true;
        getHandleInfo();
        }
    else
        saveError();

}

#endif

DFilePosition DFile::getLengthFromHandle()
{	if (!good())
        return -1;

    #if defined(_WIN32)

    BY_HANDLE_FILE_INFORMATION bhfi;
    memset(&bhfi, 0, sizeof bhfi);
    if (!GetFileInformationByHandle(handle, &bhfi)) {
        saveError();
        return -1;
        }
    else {
		__int64 ret = ((__int64)(bhfi.nFileSizeHigh)) * (0x100000000i64) +
                       (__int64)(bhfi.nFileSizeLow);
        return ret;
        }

    #elif defined(LARGEFILES)
    struct stat64 s;
    fstat64(handle, &s);
	return s.st_size;
    #else
            #warning (Using 32-bit file offsets)
    struct stat s;
    fstat(handle, &s);
	return s.st_size;
	#endif
}

void DFile::getHandleInfo()
{   if (!good())
        return;

#if defined(_WIN32)

    BY_HANDLE_FILE_INFORMATION bhfi;
    memset(&bhfi, 0, sizeof bhfi);
    if (!GetFileInformationByHandle(handle, &bhfi))
        saveError();
    else {
        length = ((__int64)(bhfi.nFileSizeHigh)) * (0x100000000i64) +
                       (__int64)(bhfi.nFileSizeLow);

        createdDate.convertFrom(bhfi.ftCreationTime);
        modifiedDate.convertFrom(bhfi.ftLastWriteTime);
        accessTime.dwLowDateTime = bhfi.ftLastAccessTime.dwLowDateTime;
        accessTime.dwHighDateTime = bhfi.ftLastAccessTime.dwHighDateTime;
        }

#else

    #if defined(LARGEFILES)
    struct stat64 s;
    fstat64(handle, &s);
    #else
        #warning (Using 32-bit file offsets)
    struct stat s;
    fstat(handle, &s);
    #endif
    length = s.st_size;
    struct tm *t = localtime(&s.st_mtime);
    modifiedDate.second = t->tm_sec;
    modifiedDate.hour = t->tm_hour;
    modifiedDate.minute = t->tm_min;
    modifiedDate.day= t->tm_mday;
    modifiedDate.month = t->tm_mon+1;
    modifiedDate.year = t->tm_year + 1900;
#endif
}

int DFile::seek(DFilePosition pos)
{    where = pos;
#ifdef _WIN32
	if (SetFilePointer64(handle, pos, SEEK_SET))
		return SUCCESS;
	else
		return FAIL;
#else
    #ifdef LARGEFILES
        return lseek64(handle, pos, SEEK_SET);
    #else
        #warning (Using 32-bit file offsets)
        return lseek(handle, pos, SEEK_SET);
    #endif
#endif
}


DFilePosition DFile::os_write(const void *buf, DFilePosition len)
{   unsigned long bytesWritten = 0;
#ifdef _WIN32
    if (!WriteFile(handle, buf, (int) len, &bytesWritten, 0)) {
        bytesWritten = 0;
        saveError();
        }
#else
    bytesWritten = ::write(handle, buf, (int) len);
#endif
    return bytesWritten;
}

DFilePosition DFile::os_read(void *buf, DFilePosition len)
{   unsigned long bytesRead = 0;
#ifdef _WIN32
    if (!ReadFile(handle, buf, (int) len, &bytesRead, 0)) {
        bytesRead = 0;
        saveError();
        }
#else
    bytesRead = ::read(handle, buf, (int) len);
#endif
	if (bytesRead < len) {
		memset((char *)buf + bytesRead, 0, (size_t) (len-bytesRead));
		}
    return bytesRead;
}

int DFile::getOpenFileCount()
{   return openCount - closeCount;
}


#ifdef _WIN32
//     Method of not changing the access date when reading from a file.
static void RestoreAccessTime(const char *filename, FILETIME origTime)
{

    HANDLE hFile = DFile::CreateFileU8(filename, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_WRITE | FILE_SHARE_READ, NULL, OPEN_EXISTING, NULL, NULL );
    if (hFile != INVALID_HANDLE_VALUE) {
        // put back the original last accessed date and time
        SetFileTime( hFile, NULL, &origTime, NULL );
        CloseHandle( hFile );
        }
    else {
        //File is possibly READONLY, try taking it off and try again.
        DWORD dwFA = DFile::GetFileAttributesU8(filename);
        if ((dwFA != 0xFFFFFFFF) && (dwFA & FILE_ATTRIBUTE_READONLY)) {
            //Got good file attributes and ReadOnly
            dwFA &= (~FILE_ATTRIBUTE_READONLY); //Remove the attribute
            if (DFile::SetFileAttributesU8(filename, dwFA)) {
                hFile = DFile::CreateFileU8(filename, GENERIC_WRITE, FILE_SHARE_WRITE | FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
                if (hFile != INVALID_HANDLE_VALUE) {
                     SetFileTime(hFile, NULL, &origTime, NULL);
                     CloseHandle(hFile);
                    }
                dwFA |= FILE_ATTRIBUTE_READONLY; //Put attribute back on
                DFile::SetFileAttributesU8(filename, dwFA);
               }

            }
         }
}
#endif


int DFile::setModifiedDate(dtsFileDate& d)
{
#ifdef _WIN32
	DFileDate fileDate = d;
	FILETIME ft;
	fileDate.convertTo(ft, false);
	if (SetFileTime(handle, NULL, NULL, &ft))
		return SUCCESS;
	else
		return FAIL;
#else
	return FAIL;
#endif
}

int DFile::setDates(dtsFileDate& created, dtsFileDate& modified)
{
#ifdef _WIN32
    DFileDate fdCreated = created;
	DFileDate fdModified = modified;
	FILETIME ftCreated, ftModified;
	fdCreated.convertTo(ftCreated);
	fdModified.convertTo(ftModified);
	if (SetFileTime(handle, &ftCreated, NULL, &ftModified))
		return SUCCESS;
	else
		return FAIL;
#else
    return FAIL;
#endif
}


int DFile::save()
{
    closeHandle();
    // Don't want to re-truncate the file when we reopen it
    int oldTrunc = flags.trunc;
    flags.trunc = false;
    openHandle();
    flags.trunc = oldTrunc;

    if (good())
        return SUCCESS;
    else
        return FAIL;
}

void DFile::closeHandle()
{   if (!good())
        return;
	if (!bShouldCloseHandle) {
		handle = (t_FileHandle) INVALID_HANDLE_VALUE;
		return;
		}

    closeCount++;

#ifdef _WIN32
    if (!CloseHandle(handle))
        saveError();
    if (flags.restoreLastAccess)
        RestoreAccessTime(name, accessTime);

#else
    ::close(handle);
#endif
    handle = (t_FileHandle) INVALID_HANDLE_VALUE;
	bShouldCloseHandle = false;
}

//
//   setLength truncates a DFile to a certain length.
//   If we have write access, it will truncate the
//   actual DFile on disk.  Otherwise it will
//   just set a limit on its own access to the DFile.
//
int DFile::setLength(DFilePosition len)
{
    if (!flags.write)
        return FAIL;

#ifdef _WIN32
    seek(len);
    if (!SetEndOfFile(handle)) {
        saveError();
        return FAIL;
        }
#elif __UNIX__
    #ifdef LARGEFILES
        if (ftruncate64(handle, len) != 0)
            return FAIL;
    #else
        #warning (Using 32-bit file offsets)
        if (ftruncate(handle, len) != 0)
            return FAIL;
    #endif
#else
    if (chsize(handle, len))
    	return FAIL;
#endif
    length = len;
    return SUCCESS;
}

// Close and reopen handle
void DFile::flush()
{	closeHandle();
	// Don't want to re-truncate the file when we reopen it
    int oldTrunc = flags.trunc;
	int oldCreate = flags.create;
	DFilePosition savePos = tell();
    flags.trunc = false;
	flags.create = false;
	openHandle();
    flags.trunc = oldTrunc;
	flags.create = oldCreate;
	seek(savePos);
}


#ifdef _WIN32

struct CUnicodeWinApi {
    CUnicodeWinApi::CUnicodeWinApi();
    void init();
    HANDLE (WINAPI *pCreateFile)(  const wchar_t * fileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes,
          DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile);
    DWORD (WINAPI *pGetFileAttributes)(const wchar_t * fn);
    BOOL (WINAPI *pSetFileAttributes)(const wchar_t * fn, DWORD fa);
    BOOL (WINAPI *pDeleteFile)(const wchar_t *fn);

    // Functions to use under Win9x
    static HANDLE WINAPI CreateFile95(  const wchar_t * fileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes,
          DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile);
    static DWORD WINAPI GetFileAttributes95(const wchar_t *  fn);
    static BOOL WINAPI SetFileAttributes95(const wchar_t *  fn, DWORD fa);
    static BOOL WINAPI DeleteFile95(const wchar_t * fn);
	int bIsUnicodeSystem;
	void FixLongFilename(DWString& s);
	static bool IsUnicodeSystem();
    };

CUnicodeWinApi::CUnicodeWinApi()
{	init();
}


bool CUnicodeWinApi::IsUnicodeSystem()
{
	static bool bCheckedIsUnicode = false;
	static bool bIsUnicode = false;
	if (!bCheckedIsUnicode) {
		OSVERSIONINFO Osv ;
		memset(&Osv, 0, sizeof Osv);
		Osv.dwOSVersionInfoSize = sizeof(OSVERSIONINFO) ;
		GetVersionEx(&Osv);

		bIsUnicode = (Osv.dwPlatformId != VER_PLATFORM_WIN32_WINDOWS);
		bCheckedIsUnicode = true;
		}
	return bIsUnicode;
}

void CUnicodeWinApi::init()
{
    bIsUnicodeSystem = IsUnicodeSystem();

	if (bIsUnicodeSystem){
	    pCreateFile = CreateFileW;
        pGetFileAttributes = GetFileAttributesW;
        pSetFileAttributes = SetFileAttributesW;
        pDeleteFile = DeleteFileW;
        }
    else {
    	pCreateFile = CreateFile95;
        pGetFileAttributes = GetFileAttributes95;
        pSetFileAttributes = SetFileAttributes95;
        pDeleteFile = DeleteFile95;
        }
};


HANDLE WINAPI CUnicodeWinApi::CreateFile95(  const wchar_t *  fileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes,
          DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile)
{
    DString ansiString;
    ansiString.storeUnicodeAsAnsi(fileName);
    return CreateFileA(ansiString, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
}

DWORD WINAPI CUnicodeWinApi::GetFileAttributes95(const wchar_t *  fn)
{
    DString ansiString;
    ansiString.storeUnicodeAsAnsi(fn);
    return GetFileAttributesA(ansiString);
}

BOOL WINAPI CUnicodeWinApi::SetFileAttributes95(const wchar_t *  fn, DWORD fa)
{
    DString ansiString;
    ansiString.storeUnicodeAsAnsi(fn);

    return SetFileAttributesA(ansiString, fa);
}

BOOL WINAPI CUnicodeWinApi::DeleteFile95(const wchar_t *  fn)
{
    DString ansiString;
    ansiString.storeUnicodeAsAnsi(fn);
    return DeleteFileA(ansiString);
}



#define LongNamePrefix		L"\\\\?\\"
#define LongNamePrefixUNC	L"\\\\?\\UNC\\"

void CUnicodeWinApi::FixLongFilename(DWString& fn)
{	if (!bIsUnicodeSystem)
		return;
	if (fn.getLength() < 255)
		return;
	if (!wcsncmp(fn, LongNamePrefix, 4))
		return;
	if (DFilename::isUnc(fn)) {
		fn.remove(0, 2);
		fn.insert(LongNamePrefixUNC, 0);
		}
	else
		fn.insert(LongNamePrefix, 0);
}


HANDLE DFile::CreateFileU8(
  const char *fileName,          // pointer to name of the file
  DWORD dwDesiredAccess,       // access (read-write) mode
  DWORD dwShareMode,           // share mode
  LPSECURITY_ATTRIBUTES lpSecurityAttributes,
                               // pointer to security attributes
  DWORD dwCreationDisposition,  // how to create
  DWORD dwFlagsAndAttributes,  // file attributes
  HANDLE hTemplateFile         // handle to file with attributes to
                               // copy
    )
{
    DWString wName;
    wName.storeUtf8AsUnicode(fileName);
    CUnicodeWinApi theApi;
    theApi.FixLongFilename(wName);
    return theApi.pCreateFile(wName, dwDesiredAccess, dwShareMode, lpSecurityAttributes,
        dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
}

void DFile::fixLongFilename(DWString& wName)
{
    CUnicodeWinApi api;
    api.FixLongFilename(wName);
}

DWORD DFile::GetFileAttributesU8(const char *fn)
{   DWString uname;
    uname.storeUtf8AsUnicode(fn);
    CUnicodeWinApi theApi;
    theApi.FixLongFilename(uname);
    return theApi.pGetFileAttributes(uname);
}

BOOL DFile::SetFileAttributesU8(const char *fn, DWORD fa)
{   DWString uname;
    uname.storeUtf8AsUnicode(fn);
    CUnicodeWinApi theApi;
    theApi.FixLongFilename(uname);
    return theApi.pSetFileAttributes(uname, fa);
}


#ifndef INVALID_FILE_ATTRIBUTES
#	define INVALID_FILE_ATTRIBUTES ((DWORD)-1)
#endif

bool DFile::IsFileU8(const char *fn)
{	DWORD attr = GetFileAttributesU8(fn);
	if (attr == INVALID_FILE_ATTRIBUTES)
		return false;
	if (attr & FILE_ATTRIBUTE_DIRECTORY)
		return false;
	return true;
}

bool DFile::ExistsU8(const char *fn)
{	DWORD attr = GetFileAttributesU8(fn);
	if (attr == INVALID_FILE_ATTRIBUTES)
		return false;
	else
		return true;
}

bool DFile::IsDirectoryU8(const char *fn)
{
	DWORD attr = GetFileAttributesU8(fn);
	if (attr == INVALID_FILE_ATTRIBUTES)
		return false;
	if (attr & FILE_ATTRIBUTE_DIRECTORY)
		return true;
	return false;
}



#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER ((DWORD)-1)
#endif

BOOL DFile::SetFilePointer64(HANDLE hFile, DFilePosition pos, DWORD moveMethod)
{
	LARGE_INTEGER li;
	li.QuadPart = pos;
    li.LowPart = SetFilePointer (hFile, li.LowPart, &li.HighPart, moveMethod);
	if ((li.LowPart == INVALID_SET_FILE_POINTER) && (GetLastError() != NO_ERROR))
		return FALSE;
	else
        return TRUE;
}


#endif

BOOL DFile::DeleteFileU8(const char *fn)
{
#ifdef _WIN32
	DWString uname;
    uname.storeUtf8AsUnicode(fn);
    CUnicodeWinApi theApi;
    theApi.FixLongFilename(uname);
    return theApi.pDeleteFile(uname);
#else
	unlink(fn);
#endif
}
