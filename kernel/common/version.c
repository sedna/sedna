/*
 * File: version.c
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */


#include "common/sedna.h"

void print_version_and_copyright(const char *prog_name)
{
    const char* ver = SEDNA_VERSION;
    const char* build = SEDNA_BUILD;
    fprintf(stdout, "%s Version %s.%s\n", prog_name, ver, build);
    fprintf(stdout, "Copyright (C) 2004-2013 ISP RAS and others. All rights reserved.\n");
    fprintf(stdout, "See file COPYRIGHT provided with the distribution.\n");
}
