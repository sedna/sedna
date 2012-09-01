/*
 * File:  gov_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "auxiliary/counted_ptr.h"
#include "gov/gov_globals.h"
#include "common/sedna.h"
#include "common/errdbg/d_printf.h"
#include "auxiliary/argtable2/argtable2.h"
#include "u/uprocess.h"

#include <sstream>

#ifdef GOV_DEBUG
#include <iostream>
#endif /* GOV_DEBUG */

int background_mode = 0;

/* TODO: make gov able to listen multiple addresses and define a max ifaces constant */
struct arg_lit * help                = arg_lit0  ("h","help",                    "print this help and exit");
struct arg_lit * govVersion          = arg_lit0  ("v","version",                 "print Sedna version");
struct arg_file * cmdDataDirectory    = arg_file0 ("i","data directory", "<path>", "Path to the directory containing databases and cfg directories");
struct arg_str * cmdBindAddress      = arg_strn  ("l","listen-addresses","\"address1\"<,\"address2\",..",0,10, 
                                                  "local address (or addresses) Sedna listens for client connections (default localhost)" ); // Governor listen address (or addresses)
struct arg_int * cmdListenPort       = arg_int0  ("p","port-to-listen", "<int>", "socket listening port");
struct arg_int * cmdLogLevel         = arg_int0  ("d","event-logging-verbosity-level", NULL, "event log verbosity level: 0 - event logging is off, 1 - log only fatal errors, 2 - log all errors, 3 - system operational messages");
struct arg_int * cmdKeepAliveTimeout = arg_int0  ("a","keep-alive-timeout", "<int>", "session keep alive timeout (default 0 - infinite timeout)");
struct arg_int * cmdStackDepth       = arg_int0  ("d","stack-depth", "<int>", "executor stack depth");
struct arg_end * end                 = arg_end(80);

void * govArgtable[] =  {help, govVersion, 
                         cmdDataDirectory, 
                         cmdBindAddress, 
                         cmdListenPort, 
                         cmdLogLevel, 
                         cmdKeepAliveTimeout,
                         cmdStackDepth, 
                         end};


void getSednaConfValues(GlobalParameters * sednaGlobalOptions)
{
    char sednaCfgFile[U_MAX_PATH + 30];
    char procBuf[U_MAX_PATH + 1];

    FILE* fs;
    char buf[10240];
    size_t size;
    std::string cfgText;
    cfgText.reserve(10240);

    uGetImageProcPath(procBuf, __sys_call_error);
    if (procBuf[0] == '\0')
        throw USER_EXCEPTION(SE4081);

    /* TODO: find out how to set sedna_cfg_file location correctly */
    
    if (strlen(procBuf) > strlen(GOV_EXE)) {
        memset(procBuf + strlen(procBuf) - strlen(GOV_EXE), 0, 1);
    }
    
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
            size = fread(buf, sizeof(char), 10240, fs);
            if (ferror(fs)) throw USER_EXCEPTION2(SE4044, sednaCfgFile);
            cfgText.append(buf, size);
            if (feof(fs)) break;
        }

        fclose(fs);
        
        sednaGlobalOptions->setDefaultOptions();

#ifdef GOV_DEBUG
        std::cout << "\n\nDefault hardcoded defaults are: \n";
        sednaGlobalOptions->saveToStream(&std::cout);     
#endif /* GOV_DEBUG */

        sednaGlobalOptions->loadFromStream(scoped_ptr<std::stringstream>(new std::stringstream(cfgText)).get());

#ifdef GOV_DEBUG
        std::cout << "\n\nParameters from file are: \n";
        sednaGlobalOptions->saveToStream(&std::cout);     
#endif /* GOV_DEBUG */
        
    } else {
        sednaGlobalOptions->setDefaultOptions();
    }
}



void mergeCommandLineAndConfig(SednaOptions * sednaOptions)
{
    if (cmdBindAddress->count > 0) {
        sednaOptions->bindAddress = std::string(cmdBindAddress->sval[0]);
    }
    
    if (cmdDataDirectory->count > 0) {
        sednaOptions->dataDirectory = std::string(cmdBindAddress->sval[0]);
    }
    
    if (cmdListenPort->count > 0) {
        sednaOptions->listenPort = cmdListenPort->ival[0];
    }
    
    if (cmdLogLevel->count > 0) {
        sednaOptions->logLevel = cmdLogLevel->ival[0];
    }
    
    if (cmdStackDepth->count > 0) {
        sednaOptions->stackDepth = cmdStackDepth->ival[0];
    }
    
    if (cmdKeepAliveTimeout->count > 0) {
        sednaOptions->keepAlive = cmdKeepAliveTimeout->ival[0];
    }
}


/* returns true if we should exit se_gov */
bool parseSednaOptions(int argc, char ** argv, GlobalParameters * sednaGlobalOptions, const char * progname)
{
    int nerrors;

    /* verify the argtable[] entries were allocated sucessfully */
    if (arg_nullcheck(govArgtable) != 0)
    {
        /* NULL entries were detected, some allocations must have failed */
        throw USER_EXCEPTION2(SE4601, "Cannot allocate argtable");
    }
    nerrors = arg_parse(argc,argv,govArgtable);
    if (help->count > 0) {
        printf("Usage: %s [options] \n\n", progname);
        arg_print_syntax(stdout,govArgtable,"\n");
        arg_print_glossary(stdout, govArgtable, "  %-25s $s\n");
        
        arg_freetable(govArgtable,sizeof(govArgtable)/sizeof(govArgtable[0]));
        return true;
    }
    
    if (govVersion->count > 0)
    {
        printf("Sedna governor; version 4.0\n");
        printf("Copyright (C) 2003-2012 Institute for System Programming RAS ISP RAS and others. All rights reserved.\n");
        printf("See file COPYRIGHT provided with the distribution.\n");
        
        arg_freetable(govArgtable,sizeof(govArgtable)/sizeof(govArgtable[0]));
        return true;
    }
    
    if (nerrors > 0)
    {
        arg_print_errors(stdout,end,progname);
        printf("Try '%s --help' for more information.\n", progname);
        
        arg_freetable(govArgtable,sizeof(govArgtable)/sizeof(govArgtable[0]));
        return true;
    }
    
    getSednaConfValues(sednaGlobalOptions);
    mergeCommandLineAndConfig( &(sednaGlobalOptions->global) );
    
    arg_freetable(govArgtable,sizeof(govArgtable)/sizeof(govArgtable[0]));
    return false;
}
