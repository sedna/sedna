/*********************************************************************
This file is part of the argtable library. It contains the definitions
of the argtable library functions as well as specially formatted
comments that constitute the source text for the manual pages
pertaining to those functions.

Copyright (C) 2003,2004 ISP RAS (modis@ispras.ru)
Copyright (C) 1998,1999,2001 Stewart Heitmann (sheitmann@users.sourceforge.net)

The argtable library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
*********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "common/argtable.h"

#define ARGSTRLEN 1024
#define NAMESTRING(A) ((A.argname)?(A.argname):(arg_typestr[A.argtype]))
#define NULLSAFE(S) ((S)?(S):(""))

/* Need a pointer distinct from NULL and any valid string address.
I admit this particular hack is THE UGLY ONE.
ZN*/ 
const char *arg_no_default_value = (const char *)&malloc;

static
const char whitespace[] = " \f\n\r\v\t";

static
int arg_convertbool(const char *str)
  {
  /************************************************************
  Returns 0 if *str satisfies a case-insensitive match on a
  leading substring of "FALSE", "NO", or "OFF". Leading whitespace
  is ignored.
  Returns 1 if *str matches "TRUE", "YES", or "ON".
  Returns -1 otherwise.
  Handles str==NULL safely.
  **************************************************************/
  char buff[ARGSTRLEN];
  size_t i,n;

  /*-- handle NULL str */
  if (str==NULL)
     return -1;

  /*-- skip leading whitespace --*/
  while (isspace((int)*str))
     str++;

  /*-- handle empty string --*/
  if (strcmp(str,"")==0)
     return -1;

  /*-- put an upper case copy of *str in buff[] --*/
  n = strlen(str);
  strncpy(buff,str,ARGSTRLEN);
  for (i=0; i<n; i++)
     buff[i]=toupper(str[i]);

  /*-- match on "FALSE", "NO", or "OFF" --*/
  if (strncmp(buff,"FALSE",n)==0)
     return 0;
  if (strncmp(buff,"NO",n)==0)
     return 0;
  if (strncmp(buff,"OFF",n)==0 && n>1)
     return 0;

  /*-- match on "TRUE", "YES", or "ON" --*/
  if (strncmp(buff,"TRUE",n)==0)
     return 1;
  if (strncmp(buff,"YES",n)==0)
     return 1;
  if (strncmp(buff,"ON",n)==0 && n>1)
     return 1;

  /*-- no match --*/
  return -1;
  }


static
int arg_set_defaults(arg_rec *argtable, int n, char* ErrMsg)
  {
  /***************************************************************************
  Converts the default argument values from strings into the appropriate data
  type for that argument.
  Argument table entries with NULL default strings are ignored.
  Default values  of arguments with NULL valueptrs are converted but then
  discarded. This is for consistent behaviour, rather than absolute necessity.

  Should a default value be invalid, then an error message is written into
  *ErrMsg and the function returns 0. Should ErrMsg==NULL then the error
  message is omitted.
  The funtion returns 1 upon success.
  ***************************************************************************/
  int i;

  /*-- clear *ErrMsg --*/
  if (ErrMsg)
     ErrMsg[0]='\0';

  for (i=0; i<n; i++)
     {
     /*-- skip over NULL defaults --*/
     if (argtable[i].defaultstr==NULL || argtable[i].defaultstr == ARGTABLE_NO_DEFAULT_VALUE)
        continue;

     /*-- convert default value to appropriate data type --*/
     switch (argtable[i].argtype)
       {
       case arg_bool:
          {
          int x = arg_convertbool(argtable[i].defaultstr);
          if (!(x==0 || x==1))
             {
             if (ErrMsg)
                 sprintf(ErrMsg,"arg[%d].defaultstr == \"%s\", invalid bool",
                         i,argtable[i].defaultstr);
             return 0;
             }
          if (argtable[i].valueptr!=NULL)
             *((int*)argtable[i].valueptr) = x;
          break;
          }

       case arg_int:
       case arg_lit:
          {
          char *p=NULL;
          int x;
          //printf("errno %d\n",errno);
          x = (int)strtol(argtable[i].defaultstr,&p,10);
          //printf("arg x=%d\n\n", x);
          //printf("argtable[i].defaultstr=%s\n\n", argtable[i].defaultstr);
          //printf("errno %d, p \"%s\"\n",errno,p);
          if (errno!=0 || strlen(p)>0 || p==argtable[i].defaultstr)
             {
             if (ErrMsg)
                 sprintf(ErrMsg,"arg[%d].defaultstr == \"%s\", invalid int",
                         i,argtable[i].defaultstr);
             return 0;
             }
          if (argtable[i].valueptr!=NULL)
             *((int*)argtable[i].valueptr) = x;
          break;
          }

       case arg_dbl:
          {
          char *p;
          double x;
          x = strtod(argtable[i].defaultstr,&p);
          if (errno!=0 || strlen(p)>0 || p==argtable[i].defaultstr)
             {
             if (ErrMsg)
                 sprintf(ErrMsg,"arg[%d].defaultstr == \"%s\", invalid double",
                         i,argtable[i].defaultstr);
             return 0;
             }
          if (argtable[i].valueptr!=NULL)
             *((double*)argtable[i].valueptr) = x;
          break;
          }

       case arg_str:
          if (argtable[i].valueptr!=NULL)
             strcpy((char*)(argtable[i].valueptr), argtable[i].defaultstr);
          break;

       };

     }

  return 1;
  }


static
int arg_scanf(const char* str, arg_rec* argrec)
  {
  /*************************************************************
  Scans *src for the argument specified in *argrec and writes
  the result into *(argrec->valueptr). The function returns the
  number of characters that were read.
  If (argrec->valueptr)==NULL then the scanned value is discarded.
  Should an error occur, the function returns -1 and leaves
  *(argrec->valueptr) unaltered.
  Note: Strings which are enclosed by single quotes are automatically
  stripped of the enclosing quotes.
  **************************************************************/
  int length = 0;

  switch (argrec->argtype)
    {
    case arg_bool:
      {
      char boolstr[ARGSTRLEN];
      int  boolval;

      /*-- scan a boolean string value --*/
      if ( sscanf(str,"%s%n",boolstr,&length) != 1 )
         return -1;

      /*-- convert string to boolean int value --*/
      boolval = arg_convertbool(boolstr);
      if (boolval==-1)
         return -1;

      /*-- write scanned value into *argrec --*/
      if (argrec->valueptr)
         *((int*)argrec->valueptr) = boolval;
      break;
      }

    case arg_lit:
      {
      /*-- if arg has a tag then presume it was located just prior to   --*/
      /*-- this function. Thus we have found a literal tag. Set the arg --*/
      /*-- value (presuming it also is non-NULL) to 1.                  --*/
      if (argrec->tagstr && argrec->valueptr)
         *((int*)argrec->valueptr) = 1;

      /*-- if argname!=NULL then scan a literal from the command line.   --*/
      if (argrec->argname)
         {
         /*char fmtstr[ARGSTRLEN];*/
         char litstr[ARGSTRLEN];

         /*-- scan a literal string value --*/
         if ( sscanf(str,"%s%n",litstr,&length) != 1 )
            return -1;

         /*-- compare literal value to expected value --*/
         /*-- (ignores possibility of leading whitespace in argname) --*/
         if (strcmp(argrec->argname,litstr)!=0)
            return -1;

         /*-- scan succeeded --*/
         if (argrec->valueptr)
            *((int*)argrec->valueptr) = 1;
         /*printf("...ok (%d)\n",length);*/
         }
      break;
      }

    case arg_int:
      {
      int intval;

      /*-- scan an int value --*/
      if (sscanf(str,"%i %n",&intval,&length) !=1)
         return -1;

      /*-- write scanned value into *argrec --*/
      if (argrec->valueptr)
         *((int*)argrec->valueptr)  = intval;
      break;
      }

    case arg_dbl:
      {
      double dblval;

      /*-- scan for double value --*/
      if ( sscanf(str,"%lf%n",&dblval,&length) != 1)
         return -1;

      /*-- write scanned value into *argrec --*/
      if (argrec->valueptr)
         *((double*)argrec->valueptr)  = dblval;
      break;
      }

    case arg_str:
      {
      char strval[ARGSTRLEN];

      /*-- scan for quoted string fist, if no good then try unquoted --*/
      if (sscanf(str," \"%[^\"]\"%n",strval,&length) != 1 && 
          sscanf(str," '%[^']'%n",strval,&length) != 1)
         if (sscanf(str,"%s%n",strval,&length) != 1 || 
             (length != 0 && strval[0] == '-'))
            return -1;

      /*-- write the string into *argrec --*/
      if (argrec->valueptr)
         strcpy((char*)argrec->valueptr,strval);
      break;
      }
    }

  return length;
  }


static
char* arg_extract_tag(char* str, const char* tag)
  {
  /**********************************************************************
  Searches 'str' for the first occurrence of 'tag'.
  If found, the tag is erased from str by overwriting it with
  spaces. The function then returns a pointer to the next char in 'str'
  immediately after the tag.
  If the tag cannot be found in cmdline then NULL is returned.
  **********************************************************************/
  char* p;
restart:
  p = strstr(str,tag);
  if (p)
    {
    size_t n = strlen(tag);
    if (p!=str && !isspace(p[-1]))
    {
        str = p+1;
        goto restart;
    }
    if (p[n] == '=') n+=1;
    memset(p,' ',n);
    p+=n;
    }
  return p;
  }



static
char* arg_extract_value(char* str, arg_rec* argrec)
  {
  /************************************************************
  Scans the leading characters of str for an argument value as
  specified by 'argrec'. If successful, the scanned value is
  written into *(argrec->valueptr). The scanned characters are then
  erased from str by overwriting them with spaces. The function
  then returns a pointer to the next char in 'str' following the
  scanned chars.
  If argrec->valueptr==NULL then the scanned value is discarded.
  If the scan was unsuccessful, both 'str' and *(argrec-valueptr)
  remain unaltered and the function returns NULL.
  **************************************************************/
  int nscan = arg_scanf(str, argrec);
  if (nscan==-1)
     return NULL;
  memset(str,' ',nscan);
  return (str+nscan);
  }


static
void arg_sprint_marker(size_t i, size_t n, char *ErrMark)
  {
  /************************************************************
  Writes i leading spaces, followed by n carets '^' into *ErrMark.
  **************************************************************/
  while (i-->0)
    *ErrMark++ = ' ';
  while (n-->0)
    *ErrMark++ = '^';
  *ErrMark='\0';
  }


static
int arg_extract_tagged_args(char* cmdline, arg_rec* argtable, int n,
                            char* ErrMsg, char* ErrMark)
  {
  /****************************************************************
  Steps thru each entry in the argument table. For each entry that
  has a non-NULL tag string, the string in *cmdline is scanned for
  a sequence of chars that match the tag. If found, the argument
  value that is associated with the tag is scanned from *cmdline
  and written into the memory location pointed to by the argument
  table entry.
  Having done so, the tag + value is then erased from *cmdline by
  overwriting them with spaces.
  If the valueptr field of the argument table entry is NULL then
  the scanned value is discarded.
  The function returns 1 upon success, 0 upon failure.
  If the tag is not found in *cmdline and there is no default
  argument value for that argument table entry then an error is
  returned.
  If the tag is found, but the argument value could not be scanned
  then an error is returned.
  If multiple occurrences of the tag are found then an error is
  returned.
  Error messages are written into a user supplied string buffer at
  *ErrMsg.
  A marker string that indicates the position of the error in *cmdline
  is written into a user supplied string buffer at *ErrMark.
  Both ErrMsg and ErrMark are optional. If they are given as NULL
  they are ignored.
  ****************************************************************/
  int i;

  /*-- init ErrMsg and ErrMark if given non-NULL ptrs --*/
  if (ErrMsg)
     *ErrMsg = '\0';
  if (ErrMark)
     *ErrMark = '\0';

  /*-- iterate thru the tagged entries in argtable[]--*/
  for (i=0; i<n; i++)
     {
     const char* argtag  = NULLSAFE(argtable[i].tagstr);
     const char* argname = NAMESTRING(argtable[i]);
     char *p;
     /*printf("scanning arg %d in \"%s\"\n",i,cmdline);*/

     /*-- skip entries with NULL tags --*/
     if (argtable[i].tagstr==NULL)
        continue;

     /*-- extract 1st occurrence of argument tag from cmdline string.    --*/
     /*-- if successful, the return char* points to the char immediately --*/
     /*-- following the tag that just extracted from *cmdline.           --*/
     p = arg_extract_tag(cmdline,argtable[i].tagstr);

     /*-- if tag was not found then... --*/
     if (!p)
        {
        /*-- if a default argument value is supplied then skip, else error --*/
        if (argtable[i].defaultstr!=NULL)
           continue;
        else
           {
           if (ErrMsg)
               sprintf(ErrMsg,"missing argument: %s%s", argtag,argname);
           if (ErrMark)
              arg_sprint_marker(strlen(cmdline),1,ErrMark);
           return 0;
           }
        }

     /*-- tag was found, now scan the arg value --*/
     if (!arg_extract_value(p,&argtable[i]))
        {
        if (ErrMsg)
            sprintf(ErrMsg,"invalid argument: %s%s", argtag,argname);
        if (ErrMark)
           arg_sprint_marker((size_t)(p-cmdline),1,ErrMark);
        return 0;
        }

     /*-- check for other occurrences of same tag --*/
     p = arg_extract_tag(cmdline,argtable[i].tagstr);
     if (p)
        {
        if (ErrMsg)
           sprintf(ErrMsg,"repeat tag");
        if (ErrMark)
           {
           size_t taglen = strlen(argtable[i].tagstr);
           arg_sprint_marker((size_t)(p-cmdline-taglen),taglen,ErrMark);
           }
        return 0;
        }
     }

  return 1;
  }


static
int arg_extract_untagged_args(char* cmdline, arg_rec* argtable, int n, char* ErrMsg, char * ErrMark)
  {
  /****************************************************************
  Steps thru each entry in the argument table. For each entry that
  has a NULL tag string, cmdline[] is scanned from left to right for
  the first occuring argument. The value of that argument is written
  into the location specified by the valueptr field of the appropriate
  argtable entry. The argument is then erased from cmdline[] by
  overwriting it with spaces.
  If the valueptr field is NULL then the scanned value is discarded.
  Upon successful completion of the function, all scanned arguments
  will have been erased from cmdline[]. Any extraneous arguments
  remain in cmdline[] intact.
  If an argument value does not match the expected type and that
  argument is optional (ie: it has a non-NULL default value) then
  we give the user the benefit of the doubt and assume that the
  value given was not intended for this argument. On the other
  hand, if the argument is not optional, then we know immediately
  that the value is invalid and an error should be generated.
  Error messages are written into a user supplied string buffer at
  *ErrMsg.
  A marker string that indicates the position of the error in *cmdline
  is written into a user supplied string buffer at *ErrMark.
  Both ErrMsg and ErrMark are optional. If they are given as NULL
  they are ignored.
  The function returns 1 upon success, 0 upon failure.
  ****************************************************************/
  char* p = cmdline;
  int i;

  /*-- init ErrMsg and ErrMark if given non-NULL ptrs --*/
  if (ErrMsg)
     *ErrMsg = '\0';
  if (ErrMark)
     *ErrMark = '\0';

  /*-- iterate thru each untagged entry in argtable[] --*/
  for (i=0; i<n; i++)
     {
     const char* argname = NAMESTRING(argtable[i]);

     /*printf("scanning arg %d in \"%s\"\n",i,cmdline);*/

     /*-- skip entries with non-NULL tags --*/
     if (argtable[i].tagstr!=NULL)
        continue;

     /*-- skip leading whitespace --*/
     while (isspace((int)*p))
        p++;

     /*-- if there are no more arguments on the command line then... --*/
     if (*p=='\0')
        {
        /*-- if a default argument is given then skip arg, else error --*/
        if (argtable[i].defaultstr!=NULL)
           continue;
        else
           {
           if (ErrMsg)
              sprintf(ErrMsg,"%s argument expected", argname);
           if (ErrMark)
              {
              size_t m = strspn(cmdline,whitespace);
              arg_sprint_marker(m,1,ErrMark);
              }
           return 0;
           }
        }

     /*-- extract leading argument value from cmdline string --*/
     /*-- if successfull then skip to next arg --*/
     if (arg_extract_value(cmdline,&argtable[i]))
        continue;

     /*-- we get to here only if argument value extraction failed. --*/
     /*-- if the arg is optional (ie: has a default) then we presume that --*/
     /*-- the value was not intended for this arg, so skip to next arg.   --*/
     if (argtable[i].defaultstr!=NULL)
         continue;

     /*-- we get here only if the command line value is invalid and   --*/
     /*-- the expected argument is not optional. the value given must --*/
     /*-- therefore be erroneous so generate an error and return zero --*/
     if (ErrMsg)
        sprintf(ErrMsg,"invalid %s argument", argname);
     if (ErrMark)
        {
        size_t m = strspn(cmdline,whitespace);
        size_t n = strcspn(cmdline+m,whitespace);
        arg_sprint_marker(m,n,ErrMark);
        }
     return 0;
     }

  return 1;
  }



static
int arg_checktable(arg_rec *argtable, /* ptr to argument table */
                   int n,             /* number of entries in argtable[] */
                   char *ErrMsg)      /* ptr to target error string (may be NULL)*/
  {
  /************************************************************
  Checks the validity of an argument table.

  DESCRIPTION:
  Each of the 'n' entries in 'argtable[]' are tested for validity;
  any errors will cause a one-line description of the failure to be
  written into *ErrMsg, after which the function will return zero.
  *ErrMsg is assumed to be big enough to hold the result, unless it
  is given as NULL in which case the error messages are suppressed.

  There is little, or no, reason to call this function directly as
  both arg_scanargv() and arg_scanstr() call it automatically.

  The function also converts the default value of each argument from
  a string into its appropriate data type and stores the result in
  the location designated  for that argument. Any invalid default
  values are reported in the same manner as invalid arguments.

  RETURN VALUE:
  Returns 1 if the argument table is valid, otherwise returns 0.

  VALIDITY CONSTRAINTS:
  An argument tag can be NULL, but it cannot be an empty string or
  comprise entirely of whitespace. Whitespace is allowed provided
  there is at least one non-whitespace character in the tag.

  **************************************************************/
  int i;

  for (i=0; i<n; i++)
     {
     /*-- check validity of non-NULL tag strings --*/
     if (argtable[i].tagstr!=NULL)
        {
        if (strcmp(argtable[i].tagstr,"")==0 ||
            strcspn(argtable[i].tagstr,whitespace)==0)
           {
           if (ErrMsg)
              sprintf(ErrMsg,"arg[%d], invalid tag \"%s\"",i,argtable[i].tagstr);
           return 0;
           }
        }
     }

  /*-- check and set defaults --*/
  if (!arg_set_defaults(argtable,n,ErrMsg))
     return 0;

  return 1;
  }






/*=============== publicly accessable functions follow ===================*/
/*================ these are documented in argtable.h ====================*/


void arg_catargs(int argc, char **argv, char *str)
  {
  int i;
  size_t j;
  str[0]='\0';
  for (i=0; i<argc; i++)
     {
     /*-- if argv[i] has any whitespace then... --*/
     if (strpbrk(argv[i],whitespace))
        {
		  strcat(str,"\"");
          for (j=0; j<strlen(argv[i]); j++)
             if (argv[i][j] == '\"')
                strcat(str,"'");
             else
                strncat(str,argv[i]+j,1);
        strcat(str,"\"");            
        }
     else
        strcat(str,argv[i]);

     strcat(str," ");
     }
  }


void arg_dump(FILE* fp, const arg_rec* argtable, int n)
  {
  int i;

  for (i=0; i<n; i++)
    {
    char tag[ARGSTRLEN], name[ARGSTRLEN], type[ARGSTRLEN], value[ARGSTRLEN],
         descr[ARGSTRLEN];

    /*-- tag --*/
    if (argtable[i].tagstr)
       sprintf(tag, "\"%s\"", argtable[i].tagstr);
    else
       sprintf(tag,"NULL");

    /*-- name --*/
    if (argtable[i].argname)
       sprintf(name, "\"%s\"", argtable[i].argname);
    else
       sprintf(name,"NULL");

    /*-- type and value--*/
    sprintf(value, "NULL");
    switch (argtable[i].argtype)
      {
      case arg_int:
        sprintf(type,"arg_int");
        if (argtable[i].valueptr)
           sprintf(value, "%d", *((int*)argtable[i].valueptr));
        break;
      case arg_dbl:
        sprintf(type,"arg_dbl");
        if (argtable[i].valueptr)
           sprintf(value, "%f", *((double*)argtable[i].valueptr));
        break;
      case arg_str:
        sprintf(type,"arg_str");
        if (argtable[i].valueptr)
           sprintf(value, "\"%s\"", (char*)argtable[i].valueptr);
        break;
      case arg_bool:
        sprintf(type,"arg_bool");
        if (argtable[i].valueptr)
           sprintf(value, "%d", *((int*)argtable[i].valueptr));
        break;
      case arg_lit:
        sprintf(type,"arg_lit");
        if (argtable[i].valueptr)
           sprintf(value, "%d", *((int*)argtable[i].valueptr));
        break;
      };

    /*-- description --*/
    if (argtable[i].argdescrip)
       sprintf(descr,"\"%s\"",argtable[i].argdescrip);
    else
       sprintf(descr,"NULL");

    fprintf(fp,"%3d: %-15s %-15s %10s = %-10s %s\n", i, tag, name, type, value, descr);
    }
  }



const char* arg_glossary(const arg_rec* argtable,  /**< pointer to the argument table */
                         int n,                    /**< number of array elements in argtable[] */
                         const char* prefix)       /**< a string to be prefixed to each line of the output */
  {
  static char str[3000];
  static char NULLprefix[]="";
  const char *fmt = "%s%s\n";
  int i;

  /*-- handle case of NULL prefix string --*/
  if (prefix==NULL) prefix=NULLprefix;
  else if (prefix[0]=='\001')
  {
      fmt = prefix+1;
      prefix = NULLprefix;
  }

  /*-- initialise str to "" --*/
  str[0]='\0';

  for (i=0; i<n; i++)
    if (argtable[i].argdescrip!=NULL)
      {
      char tempstr[ARGSTRLEN]="";
      char tagname[ARGSTRLEN]="";

      strcpy(tagname, NULLSAFE(argtable[i].tagstr));
      strcat(tagname, NAMESTRING(argtable[i]));

      strcat(str, prefix);
      sprintf(tempstr, fmt, tagname, argtable[i].argdescrip);
      strcat(str,tempstr);
      }
  return (const char*)str;
  }


/**
 * \brief Builds and returns an argument table record.
 *
 * Returns an arg_rec structure containing the values passed to
 * the function. It is useful for building argument tables dynamically.
 */
arg_rec arg_record(char *tagstr,      /**< argument tag string */
                   char *argname,     /**< argument name string */
                   arg_type argtype,  /**< argument data type */
                   void *valueptr,    /**< pointer to user-supplied storage location */
                   char *defaultstr,  /**< default argument value, as a string */
                   char *argdescrip)  /**< argument description string */
  {
  arg_rec result;
  result.tagstr     = tagstr;
  result.argname    = argname;
  result.argtype    = argtype;
  result.valueptr   = valueptr;
  result.defaultstr = defaultstr;
  result.argdescrip = argdescrip;
  return result;
  }


int (arg_scanargv)(int argc,          /**< number of entries in 'argv'. */
                 char **argv,       /**< pointer to command line arguments. */
                 arg_rec *argtable, /**< pointer to the argument table. */
                 int n,             /**< number of entries in argtable[]. */
                 char* CmdLine,     /**< pointer to storage for command line (may be NULL). */
                 char* ErrMsg,      /**< pointer to storage for error message (may be NULL). */
                 char* ErrMark)     /**< pointer to storage for error marker (may be NULL). */
  {
  char cmdline[10000], *p;

  /*-- init CmdLine, ErrMsg and ErrMark if given --*/
  if (CmdLine) *CmdLine='\0';
  if (ErrMsg)  *ErrMsg='\0';
  if (ErrMark) *ErrMark='\0';

  /*-- concat arguments in argv[] into one line in cmdline[] --*/
  arg_catargs(argc,argv,cmdline);

  /*-- give user a copy of cmdline if requested --*/
  if (CmdLine)
      strcpy(CmdLine,cmdline);

  errno = 0;

  /*-- check argtable validity and convert default values --*/
  if (!arg_checktable(argtable,n,ErrMsg))
     return 0;

  /*-- erase the first word, it is the program name and we dont want it. --*/
  /*-- dont forget enclosing quotes on prog names containing spaces!     --*/
  if (strpbrk(argv[0],whitespace))
     memset(cmdline,' ',strlen(argv[0])+2);
  else
     memset(cmdline,' ',strlen(argv[0]));

  /*-- extract arguments from cmdline[] --*/
  if (!arg_scanstr(cmdline,argtable,n,ErrMsg,ErrMark))
     return 0;

  /*-- check cmdline[] for extraneous arguments --*/
  p = cmdline;
  while (isspace((int)*p))
     p++;
  if (*p!='\0')
     {
     if (ErrMsg)
         sprintf(ErrMsg,"unexpected argument: %.*s", strcspn(p, " \n\t"), p);
     if (ErrMark)
        {
        size_t n = strcspn(p,whitespace);
        arg_sprint_marker((size_t)(p-cmdline),n,ErrMark);
        }
     return 0;
     }

  return 1;
  }



int (arg_scanargv_10)(int argc, char **argv, arg_rec *argtable, int n,
                     char* CmdLine, char* ErrMsg, char* ErrMark)
  {
  char cmdline[10000], *p;

  /*-- init CmdLine, ErrMsg and ErrMark if given --*/
  if (CmdLine) *CmdLine='\0';
  if (ErrMsg)  *ErrMsg='\0';
  if (ErrMark) *ErrMark='\0';

  /*-- concat arguments in argv[] into one line in cmdline[] --*/
  arg_catargs(argc,argv,cmdline);

  /*-- give user a copy of cmdline if requested --*/
  if (CmdLine)
      strcpy(CmdLine,cmdline);

  /*-- check argtable validity and convert default values --*/
  if (!arg_checktable(argtable,n,ErrMsg))
     return 0;

  /*-- extract arguments from cmdline[] --*/
  if (!arg_scanstr(cmdline,argtable,n,ErrMsg,ErrMark))
     return 0;

  /*-- check cmdline[] for extraneous arguments --*/
  p = cmdline;
  while (isspace((int)*p))
     p++;
  if (*p!='\0')
     {
     if (ErrMsg)
        sprintf(ErrMsg,"unexpected argument");
     if (ErrMark)
        {
        size_t n = strcspn(p,whitespace);
        arg_sprint_marker((size_t)(p-cmdline),n,ErrMark);
        }
     return 0;
     }

  return 1;
  }


int arg_scanstr(char* str,         /**< pointer to command line string. */
                arg_rec *argtable, /**< pointer to the argument table. */
                int n,             /**< number of array elements in argtable[]. */
                char* ErrMsg,      /**< pointer to storage for error message (may be NULL). */
                char* ErrMark)     /**< pointer to storage for error marker (may be NULL). */
  {
  /*-- init ErrMsg and ErrMark if given --*/
  if (ErrMsg)  *ErrMsg='\0';
  if (ErrMark) *ErrMark='\0';

  /*-- check argtable validity and convert default values --*/
  if (!arg_checktable(argtable,n,ErrMsg))
     return 0;

  /*-- extract tagged arguments from str --*/
  if (!arg_extract_tagged_args(str,argtable,n,ErrMsg,ErrMark))
     return 0;

  /*-- extract untagged arguments from str --*/
  if (!arg_extract_untagged_args(str,argtable,n,ErrMsg,ErrMark))
     return 0;

  return 1;
  }


const char* arg_syntax(const arg_rec* argtable,  /**< pointer to the argument table */
                       int n)                    /**< number of array elements in argtable[] */
  {
  static char str[ARGSTRLEN];
  int addspace=0;
  int i;

  /*-- initialise str to "" --*/
  str[0]='\0';

  /*-- concatenate consecutive argument descriptions to str.          --*/
  /*-- ignore those arguments whose tag and name strings are both "". --*/
  for (i=0; i<n; i++)
    if (strcmp(NULLSAFE(argtable[i].argdescrip),"")!=0)
      {
      const int has_default = 
          (argtable[i].defaultstr==NULL || 
          argtable[i].defaultstr==ARGTABLE_NO_DEFAULT_VALUE);

      /*-- prefix all but first argument with space separator --*/
      if (addspace)
        strcat(str," ");
      else
        addspace=1;

      /*-- write opening [ for optional arguments--*/
      if (has_default)
        strcat(str,"[");

      /*-- write tag string --*/
      strcat(str,NULLSAFE(argtable[i].tagstr));

      /*-- write arg name --*/
      strcat(str,NAMESTRING(argtable[i]));

      /*-- write closing ] for optional args --*/
      if (has_default)
        strcat(str,"]");
      }

  return (const char*)str;
  }


const char* arg_typestr[] = { "<int>", "<double>", "<string>", "<bool>", "" };


