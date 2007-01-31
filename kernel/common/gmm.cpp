/*
 * File:  gmm.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "common/gmm.h"
#include "common/base.h"
#include "common/xptr.h"


static UMMap global_memory_mapping;
static void *global_memory;


void create_global_memory_mapping()
{
    char buf[1024];
    vmm_region_values v;


    global_memory_mapping = uCreateFileMapping(U_INVALID_FD, PAGE_SIZE, SEDNA_GLOBAL_MEMORY_MAPPING, NULL, __sys_call_error);
    if (U_INVALID_FILEMAPPING(global_memory_mapping))
        throw USER_EXCEPTION2(SE4074, "See file FAQ shipped with the distribution");

    global_memory = uMapViewOfFile(global_memory_mapping, NULL, PAGE_SIZE, 0, __sys_call_error);
    if (global_memory == NULL)
        throw USER_EXCEPTION(SE4078);


    memset(global_memory, '\0', PAGE_SIZE);
    *(t_layer*)global_memory = INVALID_LAYER;


    //!!! overflow may happen
    strcpy(buf, SEDNA_DATA);
#ifdef _WIN32
    strcat(buf, "\\data");
#else
    strcat(buf, "/data");
#endif
    if (!uIsFileExist(buf, __sys_call_error))
    {
        if (uMkDir(buf, NULL, __sys_call_error) == 0)
            throw USER_EXCEPTION2(SE4300, buf);
    }

    //!!! overflow may happen
#ifdef _WIN32
    strcat(buf, "\\vmm.dat");
#else
    strcat(buf, "/vmm.dat");
#endif


    if (uIsFileExist(buf, __sys_call_error))
    {
        UFile fd = uOpenFile(buf, U_SHARE_READ, U_READ, 0, __sys_call_error);
        if (fd == U_INVALID_FD)
            throw USER_EXCEPTION2(SE4042, "vmm.dat");

        int bytes_read = 0;
        int res = uReadFile(fd, &v, sizeof(vmm_region_values), &bytes_read, __sys_call_error);
        if (res == 0 || bytes_read != sizeof(vmm_region_values))
            throw USER_EXCEPTION2(SE4044, "vmm.dat");

        if (uCloseFile(fd, __sys_call_error) == 0)
            throw USER_EXCEPTION2(SE4043, "vmm.dat");

        LAYER_ADDRESS_SPACE_START_ADDR_INT = v.LAYER_ADDRESS_SPACE_START_ADDR_INT;
        LAYER_ADDRESS_SPACE_BOUNDARY_INT = v.LAYER_ADDRESS_SPACE_BOUNDARY_INT;
        PH_ADDRESS_SPACE_START_ADDR_INT = v.PH_ADDRESS_SPACE_START_ADDR_INT;
        LAYER_ADDRESS_SPACE_SIZE = v.LAYER_ADDRESS_SPACE_SIZE;

//d_printf2("load LAYER_ADDRESS_SPACE_SIZE = %d\n", LAYER_ADDRESS_SPACE_SIZE);

        LAYER_ADDRESS_SPACE_START_ADDR = (void*)LAYER_ADDRESS_SPACE_START_ADDR_INT;
        LAYER_ADDRESS_SPACE_BOUNDARY = (void*)LAYER_ADDRESS_SPACE_BOUNDARY_INT;
        PH_ADDRESS_SPACE_START_ADDR = (void*)PH_ADDRESS_SPACE_START_ADDR_INT;

        *(vmm_region_values*)((char*)global_memory + PAGE_SIZE - sizeof(vmm_region_values)) = v;
    }
    else
    {
        uSetEnvironmentVariable(SEDNA_DETERMINE_VMM_REGION, "1", __sys_call_error);

        char path_buf[U_MAX_PATH + 10];
        std::string path_str = uGetImageProcPath(path_buf, __sys_call_error) + std::string("/") + SESSION_EXE;
        strcpy(path_buf, path_str.c_str());

               
        UPID pid;
        UPHANDLE process_handle;
        if (uCreateProcess(path_buf,
                           false, // inherit handles
                           NULL,
                           U_DETACHED_PROCESS,
                           &process_handle,
                           NULL,
                           &pid,
                           NULL,
                           NULL,
                           __sys_call_error) != 0)
            throw SYSTEM_ENV_EXCEPTION("Can't create process");

        int status;
        uWaitForChildProcess(pid, process_handle, &status, __sys_call_error);
        if (status) SYSTEM_ENV_EXCEPTION("Can't determine VMM region");
        uCloseProcess(process_handle, __sys_call_error);

        uSetEnvironmentVariable(SEDNA_DETERMINE_VMM_REGION, "0", __sys_call_error);

        v = *(vmm_region_values*)((char*)global_memory + PAGE_SIZE - sizeof(vmm_region_values));

        LAYER_ADDRESS_SPACE_START_ADDR_INT = v.LAYER_ADDRESS_SPACE_START_ADDR_INT;
        LAYER_ADDRESS_SPACE_BOUNDARY_INT = v.LAYER_ADDRESS_SPACE_BOUNDARY_INT;
        PH_ADDRESS_SPACE_START_ADDR_INT = v.PH_ADDRESS_SPACE_START_ADDR_INT;
        LAYER_ADDRESS_SPACE_SIZE = v.LAYER_ADDRESS_SPACE_SIZE;

//fprintf(res_os, "create LAYER_ADDRESS_SPACE_START_ADDR_INT = 0x%x\n", v.LAYER_ADDRESS_SPACE_START_ADDR_INT);
//fprintf(res_os, "create LAYER_ADDRESS_SPACE_BOUNDARY_INT = 0x%x\n", v.LAYER_ADDRESS_SPACE_BOUNDARY_INT);
//fprintf(res_os"create PH_ADDRESS_SPACE_START_ADDR_INT = 0x%x\n", v.PH_ADDRESS_SPACE_START_ADDR_INT);
//fprintf(res_os, "create LAYER_ADDRESS_SPACE_SIZE = 0x%x\n", v.LAYER_ADDRESS_SPACE_SIZE);

        if (LAYER_ADDRESS_SPACE_SIZE < VMM_REGION_MIN_SIZE) throw USER_EXCEPTION(SE1031);

        LAYER_ADDRESS_SPACE_START_ADDR = (void*)LAYER_ADDRESS_SPACE_START_ADDR_INT;
        LAYER_ADDRESS_SPACE_BOUNDARY = (void*)LAYER_ADDRESS_SPACE_BOUNDARY_INT;
        PH_ADDRESS_SPACE_START_ADDR = (void*)PH_ADDRESS_SPACE_START_ADDR_INT;

        UFile fd = uCreateFile(buf, 0, U_WRITE, 0, NULL, __sys_call_error);
        if (fd == U_INVALID_FD)
            throw USER_EXCEPTION2(SE4040, "vmm.dat");

        int bytes_written = 0;
        int res = uWriteFile(fd, &v, sizeof(vmm_region_values), &bytes_written, __sys_call_error);
        if (res == 0 || bytes_written != sizeof(vmm_region_values))
            throw USER_EXCEPTION2(SE4045, "vmm.dat");

        if (uCloseFile(fd, __sys_call_error) == 0)
            throw USER_EXCEPTION2(SE4043, "vmm.dat");
    }
}

void release_global_memory_mapping()
{
    if (uUnmapViewOfFile(global_memory_mapping, global_memory, PAGE_SIZE, __sys_call_error) == -1)
        throw USER_EXCEPTION(SE4079);

    if (uReleaseFileMapping(global_memory_mapping, SEDNA_GLOBAL_MEMORY_MAPPING, __sys_call_error) == -1)
        throw USER_EXCEPTION(SE4076);
}

void open_global_memory_mapping(int err_code)
{
    global_memory_mapping = uOpenFileMapping(U_INVALID_FD, PAGE_SIZE, SEDNA_GLOBAL_MEMORY_MAPPING, __sys_call_error);
    if (U_INVALID_FILEMAPPING(global_memory_mapping))
        throw USER_EXCEPTION(err_code);
}

void close_global_memory_mapping()
{
    if (uCloseFileMapping(global_memory_mapping, __sys_call_error) == -1)
        throw USER_EXCEPTION(SE4077);
}

UMMap get_global_memory_mapping()
{
    return global_memory_mapping;
}

void get_vmm_region_values()
{
    global_memory = uMapViewOfFile(global_memory_mapping, NULL, PAGE_SIZE, 0, __sys_call_error);
    if (global_memory == NULL)
        throw USER_EXCEPTION(SE4078);

    vmm_region_values *v;
    v = (vmm_region_values*)((char*)global_memory + PAGE_SIZE - sizeof(vmm_region_values));

    LAYER_ADDRESS_SPACE_START_ADDR_INT = v->LAYER_ADDRESS_SPACE_START_ADDR_INT;
    LAYER_ADDRESS_SPACE_BOUNDARY_INT = v->LAYER_ADDRESS_SPACE_BOUNDARY_INT;
    PH_ADDRESS_SPACE_START_ADDR_INT = v->PH_ADDRESS_SPACE_START_ADDR_INT;
    LAYER_ADDRESS_SPACE_SIZE = v->LAYER_ADDRESS_SPACE_SIZE;

    LAYER_ADDRESS_SPACE_START_ADDR = (void*)LAYER_ADDRESS_SPACE_START_ADDR_INT;
    LAYER_ADDRESS_SPACE_BOUNDARY = (void*)LAYER_ADDRESS_SPACE_BOUNDARY_INT;
    PH_ADDRESS_SPACE_START_ADDR = (void*)PH_ADDRESS_SPACE_START_ADDR_INT;

    if (uUnmapViewOfFile(global_memory_mapping, global_memory, PAGE_SIZE, __sys_call_error) == -1)
        throw USER_EXCEPTION(SE4079);
}

void set_vmm_region_values()
{
    global_memory = uMapViewOfFile(global_memory_mapping, NULL, PAGE_SIZE, 0, __sys_call_error);
    if (global_memory == NULL)
        throw USER_EXCEPTION(SE4078);

    vmm_region_values *v;
    v = (vmm_region_values*)((char*)global_memory + PAGE_SIZE - sizeof(vmm_region_values));

    v->LAYER_ADDRESS_SPACE_START_ADDR_INT = LAYER_ADDRESS_SPACE_START_ADDR_INT;
    v->LAYER_ADDRESS_SPACE_BOUNDARY_INT = LAYER_ADDRESS_SPACE_BOUNDARY_INT;
    v->PH_ADDRESS_SPACE_START_ADDR_INT = PH_ADDRESS_SPACE_START_ADDR_INT;
    v->LAYER_ADDRESS_SPACE_SIZE = LAYER_ADDRESS_SPACE_SIZE;

    if (uUnmapViewOfFile(global_memory_mapping, global_memory, PAGE_SIZE, __sys_call_error) == -1)
        throw USER_EXCEPTION(SE4079);
}

