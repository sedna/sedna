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


UFile uCreateFile(const char *name, UShareMode share, UAccess accs, UFlag attr, USECURITY_ATTRIBUTES* sa)
{
#ifdef _WIN32
    return CreateFile(name, accs, share, sa, CREATE_NEW, FILE_ATTRIBUTE_NORMAL | attr, NULL);
#else
    int fd;
    USECURITY_ATTRIBUTES file_access_mode = U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK;
    if (sa) file_access_mode = *sa;
    fd = open(name, accs | O_CREAT | O_EXCL | O_LARGEFILE | attr, file_access_mode);
/*    if (fd == -1) return -1;*/
    if (fd == -1 || fchmod(fd, file_access_mode) == -1)
        return -1;
    return fd;
#endif
}

UFile uOpenFile(const char *name, UShareMode share, UAccess accs, UFlag attr)
{
#ifdef _WIN32
    //d_printf2("uOpenFile=%s opened\n", name);
    return CreateFile(name, accs, share, NULL, OPEN_EXISTING, attr, NULL);
#else
    return open(name, accs | O_LARGEFILE | attr, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK);
#endif
}

int uCloseFile(UFile fd)
{
#ifdef _WIN32
    //d_printf1("File Closed\n");
    return CloseHandle(fd);
#else
    return (close(fd) == -1 ? 0 : 1);
#endif
}


int uDeleteFile(const char *name)
{
#ifdef _WIN32
    //d_printf2("uDeleteFile=%s deleted\n", name);
    return DeleteFile(name);
#else
    return (remove(name) == -1 ? 0 : 1);
#endif
}


int uDelDir(const char *dir)
{
#ifdef _WIN32
    int res;
    res = _rmdir(dir);
    if (res != 0)
        return 0;
    else
        return 1;
#else
    int res;
    res = rmdir(dir);
    if (res != 0)
        return 0;
    else
        return 1;

#endif
}

/* If the function succeeds, the return value is nonzero. If the return value */
/* is nonzero and the number of bytes read is zero, the file pointer was beyond */
/* the current end of the file at the time of the read operation.*/
/* If the function fails, the return value is zero*/
int uReadFile(UFile fd, void *buf, int to_read, int *already_read)
{
#ifdef _WIN32
    BOOL res = ReadFile(fd, buf, to_read, (LPDWORD) already_read, NULL);
#ifdef EL_DEBUG
#  if (EL_DEBUG == 1)
    if (res == 0)
    {
        d_printf1("ReadFile failed\n");
        d_printf2("Error %d\n", GetLastError());
    }
#  endif
#endif
    return res;
#else
    int res = read(fd, buf, to_read);
    if (res == -1)
        return 0;
    *already_read = res;
    return 1;
#endif
}

/* If the function succeeds, the return value is nonzero.*/
/* If the function fails, the return value is zero.*/
int uWriteFile(UFile fd, const void *buf, int to_write, int *already_written)
{
#ifdef _WIN32
    BOOL res = WriteFile(fd, buf, to_write, (LPDWORD) already_written, NULL);
#ifdef EL_DEBUG
#  if (EL_DEBUG == 1)
    if (res == 0)
    {
        d_printf1("WriteFile failed\n");
        d_printf2("Error %d\n", GetLastError());
    }
#  endif
#endif
    return res;
#else
    int res = write(fd, buf, to_write);
    if (res == -1)
        return 0;
    *already_written = res;
    return 1;
#endif
}

/* If the function succeeds, the return value is nonzero.*/
/* If the function fails, the return value is zero.*/
int uSetFilePointer(UFile fd, __int64 offs, __int64 * res_pos, UFlag meth)
{
#ifdef _WIN32
    LARGE_INTEGER _offs, _res_pos;
    BOOL res;
    _offs.QuadPart = offs;
    res = SetFilePointerEx(fd, _offs, &_res_pos, meth);
//d_printf2("SetFIlePointer Error=%d\n", GetLastError());
    if (res_pos)
        *res_pos = _res_pos.QuadPart;
    return res;
#else
    __int64 _res_pos = lseek64(fd, offs, meth);
    if (res_pos)
        *res_pos = _res_pos;
    if (_res_pos == (__off64_t) - 1)
        return 0;
    return 1;
#endif
}

/* If the function succeeds, the return value is nonzero.*/
/* If the function fails, the return value is zero.*/
int uSetEndOfFile(UFile fd, __int64 offs, UFlag meth)
{
#ifdef _WIN32
    LARGE_INTEGER _offs, _res_pos;
    BOOL res;
    _offs.QuadPart = offs;
    res = SetFilePointerEx(fd, _offs, &_res_pos, meth);
    if (res == 0)
        return res;
    return SetEndOfFile(fd);
#else
    if (meth == U_FILE_BEGIN)
    {
        /*printf("Calling ftruncate64...\n");*/
        if (ftruncate64(fd, offs) == -1)
            return 0;
    }
    else if (meth == U_FILE_END)
    {
        struct stat64 buf;
        if (fstat64(fd, &buf) == -1)
            return 0;
        if (ftruncate64(fd, buf.st_size + offs) == -1)
            return 0;
    }
    else if (meth == U_FILE_CURRENT)
    {
        __int64 cur_pos = lseek64(fd, offs, U_FILE_CURRENT);
        if (cur_pos == (__off64_t) - 1)
            return 0;
        
        if (ftruncate64(fd, cur_pos + offs) == -1)
            return 0;
    }
    else return 0;

    return 1;
#endif
}

int uMkDir(const char *name, USECURITY_ATTRIBUTES* sa)
{
    int res;
#ifdef _WIN32
    res = _mkdir(name);
    if (res == -1 && errno == ENOENT)
        return 0;
    if (res == -1 && errno == EEXIST)
        return -1;
    else
        return 1;
#else
    USECURITY_ATTRIBUTES dir_access_mode = U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK;
    if (sa) dir_access_mode = *sa;
    
    res = mkdir(name, dir_access_mode);
    if (res == -1 && errno == ENOENT)
        return 0;
    if (res == -1 && errno == EEXIST)
        return -1;
    else
        return 1;
#endif
}

int uIsFileExist(const char *name)
{
#ifdef _WIN32
    return (GetFileAttributes(name) != INVALID_FILE_ATTRIBUTES);
#else
    struct stat64 buf;
    return (stat64(name, &buf) != -1);
#endif
}

int uCopyFile(const char *existing_file, const char *new_file, int fail_if_exists)
{
#ifdef _WIN32
    return CopyFile(existing_file, new_file, fail_if_exists);
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
        return 0;

    src = open(existing_file, O_LARGEFILE | O_RDONLY, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK);
    if (src == -1)
        return 0;

    if (uGetFileSize(src, &src_file_size) == 0)
        return 0;
    if (uSetEndOfFile(des, src_file_size, U_FILE_BEGIN) == 0)
        return 0;

    while ((c = read(src, buf, BUFFLEN)) > 0)
    {
        if (write(des, buf, c) != c)
        {
            d_printf1("uCopyFile: write error\n");
            return 0;
        }
    }

    if (c < 0)
    {
        d_printf1("uCopyFile: read error\n");
        return 0;
    }
    return 1;
#endif
}

int uGetFileSize(UFile fd, __int64 * file_size)
{
#ifdef _WIN32
    LARGE_INTEGER size;
    BOOL res = GetFileSizeEx(fd, &size);
    if (res != 0)
        *file_size = size.QuadPart;
    return res;
#else
    struct stat64 buf;
    if (fstat64(fd, &buf) == -1)
        return 0;
    *file_size = buf.st_size;
    return 1;
#endif
}

int uGetDiskSectorSize(int *sector_size, const char *path)
{
#ifdef _WIN32
    BOOL res;
    char buf[4];
    memset(buf, '\0', 4);
    memcpy(buf, path, 2);
    buf[2] = '\\';

    res = GetDiskFreeSpace(buf, NULL, (LPDWORD) sector_size, NULL, NULL);
    if (res == 0)
        return 0;
    else
        return 1;
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
        printf("lstat error\n");
        perror("lstat");
        return 0;
    }

    n = scandir("/dev", &dir, 0, alphasort);
    if (n == -1)
    {
        printf("scandir error\n");
        perror("scandir");
        return 0;
    }

    memset(buf, '\0', DSS_BUF_SIZE);
    strcpy(buf, "/dev/");

    for (i = 0; i < n; i++)
    {
        memset(buf + 5, '\0', DSS_BUF_SIZE - 5);
        if (strlen(dir[i]->d_name) > DSS_BUF_SIZE - 6)
        {
            printf("buffer overflow error\n");
            return 0;
        }
        strcpy(buf + 5, dir[i]->d_name);
        if (lstat(buf, &dev_buf) == -1)
        {
            printf("lstat error\n");
            perror("lstat");
            return 0;
        }

        if ((major(path_buf.st_dev) == major(dev_buf.st_rdev)) && (minor(path_buf.st_dev) == minor(dev_buf.st_rdev)) && (!S_ISCHR(dev_buf.st_mode)) && strlen(dir[i]->d_name) > 1 && dir[i]->d_name[0] == 'h' && dir[i]->d_name[1] == 'd')
            break;
/*              {*/
/*                  printf("%s    %d/%d\n", buf, major(dev_buf.st_dev), minor(dev_buf.st_dev));*/
/*              }*/
    }

    for (i = 0; i < n; i++)
        free(dir[i]);
    free(dir);

    fd = open(buf, 0);
    if (fd == -1)
    {
        printf("Error opening device\n");
        perror("open");
        return 0;
    }

    if (ioctl(fd, BLKSSZGET, sector_size) == -1)
    {
        printf("Error obtaining sector size\n");
        perror("ioctl");
        return 0;
    }

    return 1;
#endif
#endif
}

int uGetUniqueFileStruct(const char *directoryName, struct file_struct *fs, int sid)
{
#ifdef _WIN32

    WIN32_FIND_DATA find_data;
    char buf[20];
    UFile tmphanldle;
    USECURITY_ATTRIBUTES *sa;

    strcpy(fs->name, directoryName);
    strcat(fs->name, "/tmp.*");

    tmphanldle = FindFirstFile(buf, &find_data);
    if (tmphanldle == U_INVALID_FD)
    {
        strcpy(fs->name, directoryName);
        strcat(fs->name, "/tmp.1");
    }
    else
    {
        int found = 1;
        while (found)
        {
            found = FindNextFile(tmphanldle, &find_data);
        }
        strcpy(fs->name, directoryName);
        strcat(fs->name, "/tmp.");
        int2c_str(atoi(find_data.cFileName + 4) + 1, buf);
        strncat(fs->name, buf, strlen(buf));
        int2c_str(sid, buf);
        strncat(fs->name, buf, strlen(buf));
        if (FindClose(tmphanldle) == 0)
            return 0;
    }

    if (uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0) != 0)
        return 0;

    fs->f = uCreateFile(fs->name, 0, U_READ_WRITE, 0, sa);
    if (fs->f == U_INVALID_FD)
    {
        return 0;
    }
    if (uReleaseSA(sa) != 0)
        return 0;

    return 1;
#else
    char tmp_template[64];
    strcpy(tmp_template, "tmpXXXXXX");
    fs->f = mkstemp(tmp_template);
    if (fs->f == -1)
        return 0;

    if (tmp_template == NULL)
        return 0;               /*failed*/

    strcpy(fs->name, tmp_template);
    
    return 1;
#endif
}


int uGetUniqueFileName(const char *directoryName, char *file_name)
{
#ifdef _WIN32

    WIN32_FIND_DATA find_data;
    struct file_struct fs;
    UFile tmphanldle;
    char buf[20];

    strcpy(buf, directoryName);
    strcat(buf, "/tmp.*");
    tmphanldle = FindFirstFile(buf, &find_data);
    if (tmphanldle == U_INVALID_FD)
    {
        strcpy(file_name, directoryName);
        strcat(file_name, "/tmp.1");
    }
    else
    {
        int found = 1;
        while (found)
        {
            found = FindNextFile(tmphanldle, &find_data);
        }
        strcpy(file_name, directoryName);
        strcat(file_name, "/tmp.");
        int2c_str(atoi(find_data.cFileName + 4) + 1, buf);
        strncat(file_name, buf, strlen(buf));
        if (FindClose(tmphanldle) == 0)
            return 0;
    }
    return 1;

#else

    char tmp_template[64];
    UFile f;
    strcpy(tmp_template, "tmpXXXXXX");
    f = mkstemp(tmp_template);
    if (f == -1)
        return 0;
    if (tmp_template == NULL)
        return 0;               /*failed*/

    strcpy(file_name, tmp_template);

    uCloseFile(f);
    return 1;
#endif
}

char *uGetAbsoluteFilePath(const char *relPath, char *absPath, int maxLength)
{
#ifdef _WIN32
    return _fullpath(absPath, relPath, maxLength);
#else
    return realpath(relPath, absPath);
#endif
}

char *uGetCurrentWorkingDirectory(char *buf, int maxLength)
{
#ifdef _WIN32
    return _getcwd(buf, maxLength);
#else
    return getcwd(buf, maxLength);
#endif
}

int uChangeWorkingDirectory(const char *path)
{
#ifdef _WIN32
    return _chdir(path);
#else
    return chdir(path);
#endif
}

char *uGetDirectoryFromFilePath(const char *path, char *extr_dir, int dirLength)
{
#ifdef _WIN32
    char drive[_MAX_DRIVE];
    char dir[_MAX_DIR];
    char *path2;

    path2 = (char *) malloc(sizeof(char) * (strlen(path) + 1));
    strcpy(path2, path);
    _splitpath(path2, drive, dir, NULL, NULL);

    memcpy(extr_dir, drive, strlen(drive));
    memcpy(extr_dir + strlen(drive), dir, strlen(dir) + 1);
    free(path2);
    return extr_dir;
#else
    char *path2;

    path2 = (char *) malloc(sizeof(char) * (strlen(path) + 1));
    strcpy(path2, path);
    strcpy(extr_dir, dirname(path2));
    free(path2);

    return extr_dir;
#endif
}
