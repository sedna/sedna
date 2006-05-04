/*
 * File:  event_log.c
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "event_log.h"
#include "uutils.h"
#include "uthread.h"
#include "ushm.h"
#include "usem.h"



#define SE_EVENT_LOG_FILENAME          "event.log"
#define SE_EVENT_LOG_FILENAME_BACKUP   "event-old.log"
#define SE_EVENT_LOG_FILENAME_BU_BASE  "event-"
#define SE_EVENT_LOG_FILENAME_BU_SUFX  ".log"


/* Until the configuration file is read and the value is obtained, 
 * the default value for event_log_level is EL_ERROR */
int event_log_elevel = EL_ERROR;
int event_log_initialized = 0;
int event_log_location = 1;
int event_log_detailed_location = 1;
int event_log_recommended_size = 1024 * 1024;
int event_log_truncate = 0;


#define SE_EVENT_LOG_SHORT_MSG         1
#define SE_EVENT_LOG_LONG_MSG_START    2
#define SE_EVENT_LOG_LONG_MSG_NEXT     3
#define SE_EVENT_LOG_LONG_MSG_END      4


#define SE_EVENT_LOG_SEMS_NUM          4


#ifdef _WIN32
#define SE_EVENT_LOG_THREAD_STACK_SIZE (1024 * 10)
#else
#define SE_EVENT_LOG_THREAD_STACK_SIZE (1024 * 100)
#endif


#define EVENT_LOG_START_MSG_PROCESSING \
    va_list ap; \
    int res = 0; \
    USemaphoreArrDown(el_sems, 0); \
    va_start(ap, s); \
    res = _vsnprintf(el_msg->content, SE_EVENT_LOG_CONTENT_LEN, s, ap); \
    va_end(ap);



static event_log_msg *el_msg = NULL;
static bool el_shutdown_daemon = false;
static UShMem el_shmem;
static USemaphoreArr el_sems;
static UTHANDLE el_thread_handle;
static FILE *el_ostr = NULL;
static int el_cur_file_size = 0;
static int el_component = EL_UNK;
static const char *el_component_detail = NULL;
static int el_sid = -1;
static int el_trid = -1;



static void __event_log_set_msg_attrs(int elevel, const char *filename, int lineno, const char *funcname)
{
    el_msg->processed = 0;
    el_msg->elevel = elevel;
    el_msg->component = el_component;
    el_msg->sid = el_sid;
    el_msg->trid = el_trid;

    if (el_component_detail && *el_component_detail)
        strcpy(el_msg->component_detail, el_component_detail);
    else
        el_msg->component_detail[0] = '\0';

    el_msg->lineno = lineno;

    if (strlen(filename) < SE_EVENT_LOG_FILENAME_LEN)
        strcpy(el_msg->filename, filename);
    else
    {
        memcpy(el_msg->filename, filename, SE_EVENT_LOG_FILENAME_LEN - 4);
        el_msg->filename[SE_EVENT_LOG_FILENAME_LEN - 4] = '.';
        el_msg->filename[SE_EVENT_LOG_FILENAME_LEN - 3] = '.';
        el_msg->filename[SE_EVENT_LOG_FILENAME_LEN - 2] = '.';
        el_msg->filename[SE_EVENT_LOG_FILENAME_LEN - 1] = '\0';
    }

    if (strlen(funcname) < SE_EVENT_LOG_FUNCNAME_LEN)
        strcpy(el_msg->funcname, funcname);
    else
    {
        memcpy(el_msg->funcname, funcname, SE_EVENT_LOG_FUNCNAME_LEN - 4);
        el_msg->funcname[SE_EVENT_LOG_FILENAME_LEN - 4] = '.';
        el_msg->funcname[SE_EVENT_LOG_FILENAME_LEN - 3] = '.';
        el_msg->funcname[SE_EVENT_LOG_FILENAME_LEN - 2] = '.';
        el_msg->funcname[SE_EVENT_LOG_FILENAME_LEN - 1] = '\0';
    }
}

static int __event_log_short_write_to_stderr(const char *s, va_list ap)
{
/* !!!
    fprintf(stderr, "EVENT LOG: ");
    vfprintf(stderr, s, ap);
    fprintf(stderr, "\n");
*/

    return 0;
}


/* ============================================================================
 * Event log server input/output functions
 * ============================================================================
 */

/*
 * Format for event log record is the following:
 *
 * elevel time ["(" component ")"] ["[" location-details "]" content
 *
 * Example:
 * INFO  18/04/2006 13:16:02 (SM web) The process is running w/o root privelegies, so memory lock has not been done.
 *
*/
static int __event_log_write_hdr(int elevel, 
                                 int component, 
                                 const char *component_detail, 
                                 int sid,
                                 int trid,
                                 int lineno, 
                                 const char *filename, 
                                 const char *funcname)
{
    char dt_buf[32];
    struct tm *newtime;
    time_t aclock;
    const char* elevel_c_str = NULL;
    int res = 0;

    time(&aclock);                   /* Get time in seconds */
    newtime = localtime(&aclock);    /* Convert time to struct tm form */

    switch (elevel)
    {
        case EL_DBG:
            elevel_c_str = "DBG";
            break;
        case EL_LOG:
            elevel_c_str = "LOG";
            break;
        case EL_COMM:
            elevel_c_str = "COMM";
            break;
        case EL_INFO:
            elevel_c_str = "INFO";
            break;
        case EL_WARN:
            elevel_c_str = "WARN";
            break;
        case EL_ERROR:
            elevel_c_str = "ERROR";
            break;
        case EL_SYS:
            elevel_c_str = "SYS";
            break;
        case EL_FATAL:
            elevel_c_str = "FATAL";
            break;
        default:
            elevel_c_str = "UNK";
    }

    res = fprintf(el_ostr,"%-5s %02d/%02d/%04d %02d:%02d:%02d",
                  elevel_c_str,
                  newtime->tm_mday, newtime->tm_mon + 1, newtime->tm_year + 1900,
                  newtime->tm_hour, newtime->tm_min, newtime->tm_sec);
    if (res == -1) return res;
    else el_cur_file_size += res;


    if (event_log_location)
    {
        const char* component_c_str = NULL;
    
        switch (component)
        {
            case EL_CDB:
                component_c_str = "CDB";
                break;
            case EL_DDB:
                component_c_str = "DDB";
                break;
            case EL_GOV:
                component_c_str = "GOV";
                break;
            case EL_RC:
                component_c_str = "RC";
                break;
            case EL_SM:
                component_c_str = "SM";
                break;
            case EL_SMSD:
                component_c_str = "SMSD";
                break;
            case EL_STOP:
                component_c_str = "STOP";
                break;
            case EL_TRN:
                component_c_str = "TRN";
                break;
            default:
                component_c_str = "UNK";
        }
    
        if (component_detail && *component_detail)
        {
            if (component == EL_TRN)
                res = fprintf(el_ostr, " (%s %s sid=%d trid=%d)", component_c_str, component_detail, sid, trid);
            else
                res = fprintf(el_ostr, " (%s %s)", component_c_str, component_detail);
        }
        else
            res = fprintf(el_ostr, " (%s)", component_c_str);

        if (res == -1) return res;
        else el_cur_file_size += res;
    }

    if (event_log_detailed_location)
    {
        if (filename && funcname)
        {
            char buf[U_MAX_PATH];

            uGetFileNameFromFilePath(filename, buf, U_MAX_PATH);
            res = fprintf(el_ostr, " [%s:%s:%d]", buf, funcname, lineno);

            if (res == -1) return res;
            else el_cur_file_size += res;
        }
    }

    return 0;
}

static void __event_log_check_output_stream()
{
event_log_init_file:
    if (!el_ostr)
    {
        /* initialize output stream */
        char buf[SEDNA_DATA_VAR_SIZE + 128];
        struct stat st;

        strcpy(buf, SEDNA_DATA);
#ifdef _WIN32
        strcat(buf, "\\data\\");
#else
        strcat(buf, "/data/");
#endif
        strcat(buf, SE_EVENT_LOG_FILENAME);

        if (stat(buf, &st) == 0) 
            el_cur_file_size = st.st_size;
        else 
            el_cur_file_size = 0;
        
        el_ostr = fopen(buf, "at");
        if (!el_ostr) return;
    }

    if (el_cur_file_size >= event_log_recommended_size)
    {
        /* rotate log */
        char buf1[SEDNA_DATA_VAR_SIZE + 128];
        char buf2[SEDNA_DATA_VAR_SIZE + 128];

        /* close file */
        fclose(el_ostr);
        el_ostr = NULL;

        /* backup file if needed */
        strcpy(buf1, SEDNA_DATA);
#ifdef _WIN32
        strcat(buf1, "\\data\\");
#else
        strcat(buf1, "/data/");
#endif
        strcat(buf1, SE_EVENT_LOG_FILENAME);


        if (event_log_truncate)
        {
            strcpy(buf2, SEDNA_DATA);
#ifdef _WIN32
            strcat(buf2, "\\data\\");
#else
            strcat(buf2, "/data/");
#endif
            strcat(buf2, SE_EVENT_LOG_FILENAME_BACKUP);

            if (remove(buf2))
            {
                if (errno != ENOENT) return;
            }
        }
        else
        {
            char dt_buf[32];
            struct tm *newtime;
            time_t aclock;

            time(&aclock);                   /* Get time in seconds */
            newtime = localtime(&aclock);    /* Convert time to struct tm form */

            sprintf(dt_buf,"%04d-%02d-%02d-%02d-%02d-%02d",
                    newtime->tm_year + 1900, newtime->tm_mon + 1, newtime->tm_mday,
                    newtime->tm_hour, newtime->tm_min, newtime->tm_sec);

            strcpy(buf2, SEDNA_DATA);
#ifdef _WIN32
            strcat(buf2, "\\data\\");
#else
            strcat(buf2, "/data/");
#endif
            strcat(buf2, SE_EVENT_LOG_FILENAME_BU_BASE);
            strcat(buf2, dt_buf);
            strcat(buf2, SE_EVENT_LOG_FILENAME_BU_SUFX);
        }

        if (rename(buf1, buf2))
            return;

        /* create new file and initialize output stream */
        goto event_log_init_file;
    }
}

static void __event_log_write_short_msg()
{
    int res = 0;
    __event_log_check_output_stream();
    if (!el_ostr) return;

    res = __event_log_write_hdr(el_msg->elevel, 
                                el_msg->component, 
                                el_msg->component_detail,
                                el_msg->sid,
                                el_msg->trid,
                                el_msg->lineno, 
                                el_msg->filename, 
                                el_msg->funcname);
    if (res == -1) return;

    res = fprintf(el_ostr, ": %s\n", el_msg->content);
    if (res == -1) return;
    else el_cur_file_size += res;

    fflush(el_ostr);
}

static void __event_log_write_long_msg_start()
{
    int res = 0;
    __event_log_check_output_stream();
    if (!el_ostr) return;

    res = __event_log_write_hdr(el_msg->elevel, 
                                el_msg->component, 
                                el_msg->component_detail,
                                el_msg->lineno, 
                                el_msg->sid,
                                el_msg->trid,
                                el_msg->filename, 
                                el_msg->funcname);
    if (res == -1) return;

    res = fprintf(el_ostr, ": %s", el_msg->content);
    if (res == -1) return;
    else el_cur_file_size += res;
}

static bool __event_log_write_long_msg_next_end()
{
    int res = 0;
    if (el_ostr)
    {
        if (el_msg->type == SE_EVENT_LOG_LONG_MSG_END)
        {
            res = fprintf(el_ostr, "%s\n", el_msg->content);
            fflush(el_ostr);
        }
        else
            res = fprintf(el_ostr, "%s", el_msg->content);
    
        if (res != -1) el_cur_file_size += res;
    }

    return (el_msg->type == SE_EVENT_LOG_LONG_MSG_NEXT);
}


/* ============================================================================
 * Event log daemon function
 * ============================================================================
 */
static U_THREAD_PROC(__event_log_daemon, arg)
{
    bool long_msg_next = true;

    while (1)
    {
        USemaphoreArrDown(el_sems, 1);

        if (el_shutdown_daemon && el_msg->processed)
            return 0;

        switch (el_msg->type)
        {
            case SE_EVENT_LOG_SHORT_MSG:
                __event_log_write_short_msg();
                break;

            case SE_EVENT_LOG_LONG_MSG_START:
                __event_log_write_long_msg_start();
                while (long_msg_next)
                {
                    USemaphoreArrUp(el_sems, 2);
                    USemaphoreArrDown(el_sems, 3);
                    long_msg_next = __event_log_write_long_msg_next_end();
                }
                long_msg_next = true;
                break;
        }

        el_msg->processed = 1;

        USemaphoreArrUp(el_sems, 0);
    }

    return 0;
}


/* ============================================================================
 * Client functions for logging short messages
 * ============================================================================
 */
int event_log_short_msg_macro(int elevel,
                              const char *filename,
                              int lineno,
                              const char *funcname,
                              int content_len)
{
    el_msg->type = SE_EVENT_LOG_SHORT_MSG;
    __event_log_set_msg_attrs(elevel, filename, lineno, funcname);

    if (content_len < 0)
    {
        el_msg->content[SE_EVENT_LOG_CONTENT_LEN - 4] = '.';
        el_msg->content[SE_EVENT_LOG_CONTENT_LEN - 3] = '.';
        el_msg->content[SE_EVENT_LOG_CONTENT_LEN - 2] = '.';
        el_msg->content[SE_EVENT_LOG_CONTENT_LEN - 1] = '\0';
    }

    USemaphoreArrUp(el_sems, 1);

    return 0;
}

int event_log_short_msg_param(const char *s, ...)
{
    EVENT_LOG_START_MSG_PROCESSING;
    return res;
}

int event_log_short_msg(int elevel, 
                        const char *filename, 
                        int lineno, 
                        const char *funcname, 
                        const char *s, 
                        ...)
{
    if (elevel <= event_log_elevel)
    {
        if (event_log_initialized)
        {
            EVENT_LOG_START_MSG_PROCESSING;
            return event_log_short_msg_macro(elevel, filename, lineno, funcname, res);
        }
        else
        {
            va_list ap;
            va_start(ap, s);
            __event_log_short_write_to_stderr(s, ap);
            va_end(ap);
        }
    }

    return 0;
}

int event_log_short_write_to_stderr(const char *s, ...)
{
    va_list ap;
    va_start(ap, s);
    __event_log_short_write_to_stderr(s, ap);
    va_end(ap);

    return 0;
}


/* ============================================================================
 * Client functions for logging long messages
 * ============================================================================
 */
int event_log_long_msg(int elevel,
                       const char *filename,
                       int lineno,
                       const char *funcname,
                       const char *short_str,
                       const char *long_str)
{
    int pos = 0, portion_size = 0;
    int short_str_len = 0, long_str_len = 0;
    bool copy = true;

    USemaphoreArrDown(el_sems, 0);

    __event_log_set_msg_attrs(elevel, filename, lineno, funcname);

    short_str_len = strlen(short_str);
    long_str_len = strlen(long_str);

    if (short_str_len + long_str_len < SE_EVENT_LOG_CONTENT_LEN)
    {
        el_msg->type = SE_EVENT_LOG_SHORT_MSG;
        copy = false;

        strcpy(el_msg->content, short_str);
        strcat(el_msg->content, long_str);
    }
    else
    {
        el_msg->type = SE_EVENT_LOG_LONG_MSG_START;
        copy = true;

        if (short_str_len < SE_EVENT_LOG_CONTENT_LEN)
        {
            /* write the whole short string only (note that we can copy a part of the long string,
               but we do not do for code simplification) */
            strcpy(el_msg->content, short_str);
        }
        else
        {
            /* write only the part of short string that fits the buffer */
            memcpy(el_msg->content, short_str, SE_EVENT_LOG_CONTENT_LEN - 5);
            el_msg->content[SE_EVENT_LOG_CONTENT_LEN - 5] = '.';
            el_msg->content[SE_EVENT_LOG_CONTENT_LEN - 4] = '.';
            el_msg->content[SE_EVENT_LOG_CONTENT_LEN - 3] = '.';
            el_msg->content[SE_EVENT_LOG_CONTENT_LEN - 2] = ' ';
            el_msg->content[SE_EVENT_LOG_CONTENT_LEN - 1] = '\0';
        }
    }

    USemaphoreArrUp(el_sems, 1);

    while (copy)
    {
        USemaphoreArrDown(el_sems, 2);

        portion_size = s_min(SE_EVENT_LOG_CONTENT_LEN - 1, long_str_len - pos);
        memcpy(el_msg->content, long_str + pos, portion_size);
        el_msg->content[portion_size] = '\0';
        pos += portion_size;

        if (pos < long_str_len) 
            el_msg->type = SE_EVENT_LOG_LONG_MSG_NEXT;
        else
        {
            el_msg->type = SE_EVENT_LOG_LONG_MSG_END;
            copy = false;
        }

        USemaphoreArrUp(el_sems, 3);
    }

    return 0;
}


int event_log_long_write_to_stderr(const char *short_str, const char *long_str)
{
/* !!!
    fprintf(stderr, "EVENT LOG: %s%s\n", short_str, long_str);
*/

    return 0;
}


/* ============================================================================
 * Init/release functions
 * ============================================================================
 */
int event_logger_start_daemon(int elevel, global_name shm_name, global_name sems_name)
{
    int sems_init_values[SE_EVENT_LOG_SEMS_NUM] = {1, 0, 0, 0};

    /* create shared memory */
    if (uCreateShMem(&el_shmem, shm_name, sizeof(event_log_msg), NULL) != 0)
        return 1;

    el_msg = (event_log_msg*)uAttachShMem(el_shmem, NULL, sizeof(event_log_msg));
    if (el_msg == NULL)
        return 2;

    /* create semaphores */
    if (USemaphoreArrCreate(&el_sems, SE_EVENT_LOG_SEMS_NUM, sems_init_values, sems_name, NULL) != 0)
        return 3;

    /* start daemon thread */
    if (uCreateThread(__event_log_daemon, NULL, &el_thread_handle, SE_EVENT_LOG_THREAD_STACK_SIZE, NULL) != 0)
        return 4;

    /* set actual event_log_elevel */
    event_log_elevel = elevel;
    /* set global actual event_log_elevel */
    el_msg->global_elevel = elevel;
    /* set component */
    el_component = EL_GOV;

    event_log_initialized = 1;

    return 0;
}

int event_logger_shutdown_daemon()
{
    if (event_log_initialized)
    {
        event_log_initialized = 0;
    
        /* stop daemon thread */
        el_shutdown_daemon = true;
        USemaphoreArrUp(el_sems, 1);
    
        if (uThreadJoin(el_thread_handle) != 0)
            return 1;
    
        if (uCloseThreadHandle(el_thread_handle) != 0)
           return 2;
    
        /* release semaphores */
        if (USemaphoreArrRelease(el_sems, SE_EVENT_LOG_SEMS_NUM) != 0)
            return 3;
    
        /* release shared memory */
        if (uDettachShMem(el_shmem, el_msg) != 0)
            return 4;
    
        el_msg = NULL;
    
        if (uReleaseShMem(el_shmem) != 0)
            return 5;
    }

    return 0;
}

int event_logger_init(int component, const char* component_detail, global_name shm_name, global_name sems_name)
{
    /* open shared memory */
    if (uOpenShMem(&el_shmem, shm_name, sizeof(event_log_msg)) != 0)
        return 1;

    el_msg = (event_log_msg*)uAttachShMem(el_shmem, NULL, sizeof(event_log_msg));
    if (el_msg == NULL) 
        return 2;

    /* open semaphores */    
    if (USemaphoreArrOpen(&el_sems, SE_EVENT_LOG_SEMS_NUM, sems_name) != 0)
        return 3;

    /* read actual event_log_elevel */
    event_log_elevel = el_msg->global_elevel;
    el_component = component;
    el_component_detail = component_detail;

    event_log_initialized = 1;

    return 0;
}

int event_logger_release()
{
    if (event_log_initialized)
    {
        event_log_initialized = 0;
    
        /* close semaphores */
        if (USemaphoreArrClose(el_sems, SE_EVENT_LOG_SEMS_NUM) != 0)
            return 1;
    
        /* close shared memory */
        if (uDettachShMem(el_shmem, el_msg) != 0)
            return 2;
    
        el_msg = NULL;
    
        if (uCloseShMem(el_shmem) != 0)
            return 3;
    }

    return 0;
}

int event_logger_set_sid(int sid)
{
    el_sid = sid;
    return 0;
}

int event_logger_set_trid(int trid)
{
    el_trid = trid;
    return 0;
}
