/*
 * File:  version.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <iostream>
#include "version.h"

using namespace std;

void print_version_and_copyright(const char *prog_name)
{
    fprintf(stdout, "%s Version %g.%d\n", prog_name, SEDNA_VERSION, SEDNA_BUILD);
    fprintf(stdout, "Copyright (C) 2004-2006 ISP RAS and others. All rights reserved.\n"); 
    fprintf(stdout, "See file COPYRIGHT provided with the distribution.\n\n");   
}
