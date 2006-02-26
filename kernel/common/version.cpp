/*
 * File:  version.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <iostream>
#include "version.h"

using namespace std;

void print_version_and_copyright(const char *prog_name)
{
    cout << prog_name << " ";
    cout << "Version " << SEDNA_VERSION;
    cout << "." << SEDNA_BUILD << endl;
    cout << "Copyright (C) 2004 ISP RAS and others. All rights reserved." << endl;
    cout << "See file COPYRIGHT provided with the distribution." << endl << endl;
}
