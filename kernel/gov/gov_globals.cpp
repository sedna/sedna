/*
 * File:  gov_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "aux/counted_ptr.h"
#include "gov/gov_globals.h"
#include "common/sedna.h"
#include "common/errdbg/d_printf.h"
#include "aux/argtable2/argtable2.h"
#include "u/uprocess.h"

#include <sstream>

/* !TODO: tune linux_install.sh (or make some more clear procedure to install sedna */

void setDefaultSednaOptions(SednaOptions * sednaOptions)
{
    sednaOptions->bindAddress = "0.0.0.0";
    sednaOptions->dataDirectory = "/var/sedna";
    sednaOptions->listenPort = 5050;
    sednaOptions->logLevel = 2;
    sednaOptions->osObjectsOffset = 1024;
    sednaOptions->stackDepth = 4000;
    sednaOptions->keepAlive = 0;
};

int background_mode = 0;

/* TODO: make gov able to listen multiple addresses and define a max ifaces constant */
struct arg_lit * help                = arg_lit0  ("h","help",                    "print this help and exit");
struct arg_lit * govVersion          = arg_lit0  ("v","version",                 "print Sedna version");
struct arg_str * cmdDataDirectory    = arg_file0 ("i","data directory", "<path>", "Path to the directory containing databases and cfg directories");
struct arg_str * cmdBindAddress      = arg_strn  ("l","listen-addresses","\"address1\"<,\"address2\",..",0,10, 
                                                  "local address (or addresses) Sedna listens for client connections (default localhost)" ); // Governor listen address (or addresses)
struct arg_int * cmdListenPort       = arg_int0  ("p","port-to-listen", "<int>", "socket listening port");
struct arg_int * cmdLogLevel    = arg_int0  ("d","event-logging-verbosity-level", NULL, "event log verbosity level: 0 - event logging is off, 1 - log only fatal errors, 2 - log all errors, 3 - system operational messages");
struct arg_int * cmdKeepAliveTimeout = arg_int0  ("a","keep-alive-timeout", "<int>", "session keep alive timeout (default 0 - infinite timeout)");
struct arg_int * cmdStackDepth       = arg_int0  ("d","stack-depth", "<int>", "executor stack depth");
struct arg_int * cmdOsObjectsOffset  = arg_int0  ("m","os-min-bound", "<int>", "os min bound \(default 1500\)" );


void * govArgtable[] =  {help, 
                         govVersion, 
                         cmdDataDirectory, 
                         cmdBindAddress, 
                         cmdListenPort, 
                         cmdLogLevel, 
                         cmdStackDepth, 
                         cmdOsObjectsOffset};


void getSednaconfValues()
{
    char sednaCfgFile[U_MAX_PATH + 30];
    char procBuf[U_MAX_PATH + 1];

    FILE* fs;
    char buf[1024];
    size_t size;
    std::string cfgText;
    cfgText.reserve(10240);

    uGetImageProcPath(procBuf, __sys_call_error);
    if (procBuf[0] == '\0')
        throw USER_EXCEPTION(SE4081);

    /* TODO: find out how to set sedna_cfg_file location correctly */
    strcpy(sednaCfgFile,  procBuf);

#ifdef _WIN32
    strcat(sednaCfgFile, "\\..\\etc\\sednaconf.xml");
#else
    strcat(sednaCfgFile, "/../etc/sednaconf.xml");
#endif

    fs = fopen(sednaCfgFile, "r");

#ifndef _WIN32
    if(NULL == fs)
    {
        strcpy(sednaCfgFile, "/etc/sednaconf.xml");
        fs = fopen(sednaCfgFile, "r");
    }
#endif /* _WIN32 */

    if (fs != NULL)
    {
        d_printf2("sednaCfgFile=%s\n", sednaCfgFile);

        while (true)
        {
            size = fread(buf, sizeof(char), 1024, fs);
            if (ferror(fs)) throw USER_EXCEPTION2(SE4044, sednaCfgFile);
            cfgText.append(buf, size);
            if (feof(fs)) break;
        }

        fclose(fs);
        
        setDefaultSednaOptions(sednaGlobalOptions->global);

        sednaGlobalOptions->loadFromStream(scoped_ptr<std::stringstream>(new std::stringstream(cfgText)).get());
    }
}