/*
 * File:  uhdd.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#if (defined(__linux__))
#define _LARGEFILE64_SOURCE
#include <fcntl.h>
#endif


#include "uhdd.h"
#include "d_printf.h"
#include "uutils.h"

#ifdef _WIN32
#include <direct.h>
#else
#ifdef LINUX
#define _LINUX_CONFIG_H
#include <linux/unistd.h>
#include <linux/fs.h>
#endif
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <libgen.h>
#endif


#ifdef _WIN32
#if (_MSC_VER <= 1200)

#define INVALID_FILE_ATTRIBUTES ((DWORD)-1)

WINBASEAPI BOOL WINAPI GetFileSizeEx(HANDLE hFile, PLARGE_INTEGER lpFileSize);

WINBASEAPI BOOL WINAPI SetFilePointerEx(HANDLE hFile, LARGE_INTEGER liDistanceToMove, PLARGE_INTEGER lpNewFilePointer, DWORD dwMoveMethod);



#endif
#endif

#ifdef HAVE_DEFAULT_LARGEFILE_FUNCTIONS
#define O_LARGEFILE	0
#define lseek64		lseek
#define fstat64		fstat
#define ftruncate64	ftruncate
#define __off64_t	off_t
#define stat64		stat

#include <machine/param.h>

#endif

#ifdef SunOS
#define __off64_t off64_t
#endif


UFile uCreateFile(const char *name, UShareMode share, UAccess accs, UFlag attr, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
{
#ifdef _WIN32
    UFile fd = CreateFile(name, accs, share, sa, CREATE_NEW, FILE_ATTRIBUTE_NORMAL | attr, NULL);
    if (fd == U_INVALID_FD) 
        sys_call_error("CreateFile");
    return fd;
#else
    int fd;
    USECURITY_ATTRIBUTES file_access_mode = U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK;
    if (sa) file_access_mode = *sa;
    fd = open(name, accs | O_CREAT | O_EXCL | O_LARGEFILE | attr, file_access_mode);
    if (fd == -1)
    {
        sys_call_error("open");
        return -1;
    }
    else if (fchmod(fd, file_access_mode) == -1)
    {
        sys_call_error("fchmod");
        return -1;
    }

    return fd;
#endif
}

UFile uOpenFile(const char *name, UShareMode share, UAccess accs, UFlag attr, sys_call_error_fun fun)
{
#ifdef _WIN32
    UFile fd = CreateFile(name, accs, share, NULL, OPEN_EXISTING, attr, NULL);
    if (fd == U_INVALID_FD) 
        sys_call_error("CreateFile");
    return fd;
#else
    UFile fd = open(name, accs | O_LARGEFILE | attr, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK);
    if (fd == U_INVALID_FD) 
        sys_call_error("open");
    return fd;   
#endif
}

int uCloseFile(UFile fd, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = CloseHandle(fd);
    if (res == 0)
        sys_call_error("CloseHandle");
    return res;
#else
    int res = close(fd);
    if (res == -1)
        sys_call_error("close");
    return (res == -1 ? 0 : 1);
#endif
}

int uDeleteFile(const char *name, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = DeleteFile(name);
    if (res == 0)
        sys_call_error("DeleteFile");
    return res;

#else
    int res = remove(name);
    if (res == -1)
        sys_call_error("remove");
    return (res == -1 ? 0 : 1);
#endif
}

int uDelDir(const char *dir, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = _rmdir(dir);
    if (res != 0)
        sys_call_error("_rmdir");
    return (res != 0 ? 0 : 1);
#else
    int res = rmdir(dir);
    if (res != 0)
        sys_call_error("rmdir");
    return (res != 0 ? 0 : 1);
#endif
}

/* If the function succeeds, the return value is nonzero. If the return value */
/* is nonzero and the number of bytes read is zero, the file pointer was beyond */
/* the current end of the file at the time of the read operation.*/
/* If the function fails, the return value is zero*/
int uReadFile(UFile fd, void *buf, int to_read, int *already_read, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = ReadFile(fd, buf, to_read, (LPDWORD) already_read, NULL);
    if (res == 0)
        sys_call_error("ReadFile");
    return res;
#else
    int res = read(fd, buf, to_read);
    if (res == -1)
        sys_call_error("read");
    else 
        *already_read = res;
    return (res == -1 ? 0 : 1);
#endif
}

/* If the function succeeds, the return value is nonzero.*/
/* If the function fails, the return value is zero.*/
int uWriteFile(UFile fd, const void *buf, int to_write, int *already_written, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = WriteFile(fd, buf, to_write, (LPDWORD) already_written, NULL);
    if (res == 0)
        sys_call_error("WriteFile");
    return res;
#else
    int res = write(fd, buf, to_write);
    if (res == -1)
        sys_call_error("write");
    else 
        *already_written = res;
    return (res == -1 ? 0 : 1);
#endif
}

/* If the function succeeds, the return value is nonzero.*/
/* If the function fails, the return value is zero.*/
int uSetFilePointer(UFile fd, __int64 offs, __int64 * res_pos, UFlag meth, sys_call_error_fun fun)
{
#ifdef _WIN32
    LARGE_INTEGER _offs, _res_pos;
    BOOL res;
    _offs.QuadPart = offs;
    res = SetFilePointerEx(fd, _offs, &_res_pos, meth);
    if (res == 0)
        sys_call_error("SetFilePointerEx");
    if (res_pos)
        *res_pos = _res_pos.QuadPart;
    return res;
#else
    __int64 _res_pos = lseek64(fd, offs, meth);
    if (res_pos)
        *res_pos = _res_pos;
    if (_res_pos == (__off64_t) -1)
    {
        sys_call_error("lseek64");
        return 0;
    }
    return 1;
#endif
}

/* If the function succeeds, the return value is nonzero.*/
/* If the function fails, the return value is zero.*/
int uSetEndOfFile(UFile fd, __int64 offs, UFlag meth, sys_call_error_fun fun)
{
#ifdef _WIN32
    LARGE_INTEGER _offs, _res_pos;
    BOOL res;
    _offs.QuadPart = offs;
    res = SetFilePointerEx(fd, _offs, &_res_pos, meth);
    if (res == 0)
        sys_call_error("SetFilePointerEx");
    else
    {
        res = SetEndOfFile(fd);
        if (res == 0)
            sys_call_error("SetEndOfFile");
    }

    return res;
#else
    if (meth == U_FILE_BEGIN)
    {
        if (ftruncate64(fd, offs) == -1)
        {
            sys_call_error("ftruncate64");
            return 0;
        }
    }
    else if (meth == U_FILE_END)
    {
        struct stat64 buf;
        if (fstat64(fd, &buf) == -1)
        {
            sys_call_error("fstat64");
            return 0;
        }
        if (ftruncate64(fd, buf.st_size + offs) == -1)
        {
            sys_call_error("ftruncate64");
            return 0;
        }
    }
    else if (meth == U_FILE_CURRENT)
    {
        __int64 cur_pos = lseek64(fd, offs, U_FILE_CURRENT);
        if (cur_pos == (__off64_t) - 1)
        {
            sys_call_error("lseek64");
            return 0;
        }
        
        if (ftruncate64(fd, cur_pos + offs) == -1)
        {
            sys_call_error("ftruncate64");
            return 0;
        }
    }
    else return 0;

    return 1;
#endif
}

int uMkDir(const char *name, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
{
    int res;
#ifdef _WIN32
    res = _mkdir(name);
    if (res == -1 && errno != EEXIST)
    {
        sys_call_error("_mkdir");
        return 0;
    }
    return 1;
#else
    USECURITY_ATTRIBUTES dir_access_mode = U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK;
    if (sa) dir_access_mode = *sa;
    
    res = mkdir(name, dir_access_mode);
    if (res == -1 && errno != EEXIST)
    {
        sys_call_error("mkdir");
        return 0;
    }
    return 1;
#endif
}

int uIsFileExist(const char *name, sys_call_error_fun fun)
{
#ifdef _WIN32
    if (GetFileAttributes(name) == INVALID_FILE_ATTRIBUTES)
    {
        sys_call_error("GetFileAttributes");
        return 0;
    }
    return 1;
#else
    struct stat64 buf;
    if (stat64(name, &buf) == -1)
    {
        sys_call_error("stat64");
        return 0;
    }
    return 1;
#endif
}

int uCopyFile(const char *existing_file, const char *new_file, int fail_if_exists, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = CopyFile(existing_file, new_file, fail_if_exists);
    if (res == 0)
        sys_call_error("CopyFile");
    return res;
#else

#define BUFFLEN 32768

    int c = 0;
    int des = 0;
    int src = 0;
    char buf[BUFFLEN];
    __int64 src_file_size = (__int64) 0;

    if (fail_if_exists)
        des = open(new_file, O_CREAT | O_EXCL | O_LARGEFILE | O_WRONLY | O_SYNC | O_TRUNC, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK);
    else
        des = open(new_file, O_CREAT | O_LARGEFILE | O_WRONLY | O_SYNC | O_TRUNC, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK);

    if (des == -1)
    {
        sys_call_error("open");
        return 0;
    }

    src = open(existing_file, O_LARGEFILE | O_RDONLY, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK);
    if (src == -1)
    {
        sys_call_error("open");
        return 0;
    }

    if (uGetFileSize(src, &src_file_size) == 0)
        return 0;
    if (uSetEndOfFile(des, src_file_size, U_FILE_BEGIN) == 0)
        return 0;

    while ((c = read(src, buf, BUFFLEN)) > 0)
    {
        if (write(des, buf, c) != c)
        {
            sys_call_error("write");
            return 0;
        }
    }

    if (c < 0)
    {
        sys_call_error("read");
        return 0;
    }
    return 1;
#endif
}

int uGetFileSize(UFile fd, __int64 * file_size, sys_call_error_fun fun)
{
#ifdef _WIN32
    LARGE_INTEGER size;
    BOOL res = GetFileSizeEx(fd, &size);
    if (res == 0)
        sys_call_error("GetFileSizeEx");
    else
        *file_size = size.QuadPart;
    return res;
#else
    struct stat64 buf;
    if (fstat64(fd, &buf) == -1)
    {
        sys_call_error("fstat64");
        return 0;
    }
    *file_size = buf.st_size;
    return 1;
#endif
}

int uGetDiskSectorSize(int *sector_size, const char *path, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res;
    char buf[4];
    memset(buf, '\0', 4);
    memcpy(buf, path, 2);
    buf[2] = '\\';

    res = GetDiskFreeSpace(buf, NULL, (LPDWORD) sector_size, NULL, NULL);
    if (res == 0)
        sys_call_error("GetDiskFreeSpace");
    return res;
#else
#ifdef PREDEFINED_DISK_SECTOR_SIZE
    *sector_size = PREDEFINED_DISK_SECTOR_SIZE;
    return 1;
#else
#define DSS_BUF_SIZE		100
    int fd;
    struct stat path_buf, dev_buf;
    struct dirent **dir;
    int n = 0, i = 0;
    char buf[DSS_BUF_SIZE];

    if (lstat(path, &path_buf) == -1)
    {
        sys_call_error("lstat");
        return 0;
    }

    n = scandir("/dev", &dir, 0, alphasort);
    if (n == -1)
    {
        sys_call_error("lstat");
        return 0;
    }

    memset(buf, '\0', DSS_BUF_SIZE);
    strcpy(buf, "/dev/");

    for (i = 0; i < n; i++)
    {
        memset(buf + 5, '\0', DSS_BUF_SIZE - 5);
        if (strlen(dir[i]->d_name) > DSS_BUF_SIZE - 6)
        {
            sys_call_error("<buffer overflow error>");

            for (i = 0; i < n; i++)
                free(dir[i]);
            free(dir);

            return 0;
        }
        strcpy(buf + 5, dir[i]->d_name);
        if (lstat(buf, &dev_buf) == -1)
        {
            sys_call_error("lstat");

            for (i = 0; i < n; i++)
                free(dir[i]);
            free(dir);

            return 0;
        }

        if ((major(path_buf.st_dev) == major(dev_buf.st_rdev)) && 
            (minor(path_buf.st_dev) == minor(dev_buf.st_rdev)) && 
            (!S_ISCHR(dev_buf.st_mode)) && 
            (strlen(dir[i]->d_name) > 1) && 
            (dir[i]->d_name[0] == 'h') && 
            (dir[i]->d_name[1] == 'd'))
            break;
    }

    for (i = 0; i < n; i++)
        free(dir[i]);
    free(dir);

    fd = open(buf, 0);
    if (fd == -1)
    {
        sys_call_error("open");
        return 0;
    }

    if (ioctl(fd, BLKSSZGET, sector_size) == -1)
    {
        sys_call_error("ioctl");
        return 0;
    }

    return 1;
#endif
#endif
}

int uGetUniqueFileStruct(const char *directoryName, struct file_struct *fs, int sid, sys_call_error_fun fun)
{
#ifdef _WIN32
    WIN32_FIND_DATA find_data;
    char buf[20];
    UFile tmphandle;
    USECURITY_ATTRIBUTES *sa;

    strcpy(fs->name, directoryName);
    strcat(fs->name, "/tmp.*");

    tmphandle = FindFirstFile(buf, &find_data);
    if (tmphandle == U_INVALID_FD)
    {
        strcpy(fs->name, directoryName);
        strcat(fs->name, "/tmp.1");
    }
    else
    {
        int found = 1;
        while (found)
        {
            found = FindNextFile(tmphandle, &find_data);
        }
        strcpy(fs->name, directoryName);
        strcat(fs->name, "/tmp.");
        int2c_str(atoi(find_data.cFileName + 4) + 1, buf);
        strncat(fs->name, buf, strlen(buf));
        int2c_str(sid, buf);
        strncat(fs->name, buf, strlen(buf));
        if (FindClose(tmphandle) == 0)
            return 0;
    }

    if (uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error) != 0)
        return 0;

    fs->f = uCreateFile(fs->name, 0, U_READ_WRITE, 0, sa, __sys_call_error);
    if (fs->f == U_INVALID_FD)
    {
        return 0;
    }
    if (uReleaseSA(sa, __sys_call_error) != 0)
        return 0;

    return 1;
#else
    char tmp_template[64];
    strcpy(tmp_template, "tmpXXXXXX");
    fs->f = mkstemp(tmp_template);
    if (fs->f == -1)
    {
        sys_call_error("mkstemp");
        return 0;
    }

    if (tmp_template == NULL)
        return 0;               /*failed*/

    strcpy(fs->name, tmp_template);
    
    return 1;
#endif
}


int uGetUniqueFileName(const char *directoryName, char *file_name, sys_call_error_fun fun)
{
#ifdef _WIN32

    WIN32_FIND_DATA find_data;
    struct file_struct fs;
    UFile tmphandle;
    char buf[20];

    strcpy(buf, directoryName);
    strcat(buf, "/tmp.*");
    tmphandle = FindFirstFile(buf, &find_data);
    if (tmphandle == U_INVALID_FD)
    {
        strcpy(file_name, directoryName);
        strcat(file_name, "/tmp.1");
    }
    else
    {
        int found = 1;
        while (found)
        {
            found = FindNextFile(tmphandle, &find_data);
        }
        strcpy(file_name, directoryName);
        strcat(file_name, "/tmp.");
        int2c_str(atoi(find_data.cFileName + 4) + 1, buf);
        strncat(file_name, buf, strlen(buf));
        if (FindClose(tmphandle) == 0)
            return 0;
    }
    return 1;

#else

    char tmp_template[64];
    UFile f;
    strcpy(tmp_template, "tmpXXXXXX");
    f = mkstemp(tmp_template);
    if (f == -1)
    {
        sys_call_error("mkstemp");
        return 0;
    }
    if (tmp_template == NULL)
        return 0;               /*failed*/

    strcpy(file_name, tmp_template);

    uCloseFile(f);
    return 1;
#endif
}

char *uGetAbsoluteFilePath(const char *relPath, char *absPath, int maxLength, sys_call_error_fun fun)
{
#ifdef _WIN32
    char *p = _fullpath(absPath, relPath, maxLength);
    if (p == NULL)
        sys_call_error("_fullpath");
    return p;
#else
    char *p = realpath(relPath, absPath);
    if (p == NULL)
        sys_call_error("realpath");
    return p;
#endif
}

char *uGetCurrentWorkingDirectory(char *buf, int maxLength, sys_call_error_fun fun)
{
#ifdef _WIN32
    char *p = _getcwd(buf, maxLength);
    if (p == NULL)
        sys_call_error("_getcwd");
    return p;
#else
    char *p = getcwd(buf, maxLength);
    if (p == NULL)
        sys_call_error("getcwd");
    return p;

#endif
}

int uChangeWorkingDirectory(const char *path, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = _chdir(path);
    if (res != 0)
        sys_call_error("_chdir");
    return res;
#else
    int res = chdir(path);
    if (res != 0)
        sys_call_error("chdir");
    return res;
#endif
}

char *uGetDirectoryFromFilePath(const char *path, char *buf, int buf_len, sys_call_error_fun fun)
{
#ifdef _WIN32
    char drive[_MAX_DRIVE];
    char dir[_MAX_DIR];
    char *tmp_path = NULL;

    tmp_path = strdup(path);
    _splitpath(tmp_path, drive, dir, NULL, NULL);
    if (strlen(drive) + strlen(dir) + 1 > buf_len)
    {
        free(tmp_path);
        return NULL;
    }
    strcpy(buf, drive);
    strcat(buf, dir);
    free(tmp_path);

    return buf;
#else
    char *tmp_path = NULL, *dir_name = NULL;
    tmp_path = strdup(path);
    dir_name = dirname(tmp_path);
    if (strlen(dir_name) + 1 > buf_len)
    {
        free(tmp_path);
        return NULL;
    }
    strcpy(buf, dir_name);
    free(tmp_path);

    return buf;
#endif
}

char *uGetFileNameFromFilePath(const char *path, char *buf, int buf_len, sys_call_error_fun fun)
{
#ifdef _WIN32
    char fname[_MAX_FNAME];
    char ext[_MAX_EXT];
    char *tmp_path = NULL;

    tmp_path = strdup(path);
    _splitpath(tmp_path, NULL, NULL, fname, ext);
    if (strlen(fname) + strlen(ext) + 1 > buf_len)
    {
        free(tmp_path);
        return NULL;
    }
    strcpy(buf, fname);
    strcat(buf, ext);
    free(tmp_path);

    return buf;
#else
    char *tmp_path = NULL, *file_name = NULL;
    tmp_path = strdup(path);
    file_name = basename(tmp_path);
    if (strlen(file_name) + 1 > buf_len)
    {
        free(tmp_path);
        return NULL;
    }
    strcpy(buf, file_name);
    free(tmp_path);

    return buf;
#endif
}
