/*********************************************************************
This file is part of the argtable library. It contains the declarations
of the argtable library functions.

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
#ifndef ARGTABLE
#define ARGTABLE

#include <stdio.h>
#define ARGTABLE_VERSION 1.3

#ifdef ARGTABLE_COMPATIBILITY_10
#define arg_scanargv(a,b,c,d,e,f,g) arg_scanargv_10(a,b,c,d,e,f,g)
#endif

#define ARGTABLE_NO_DEFAULT_VALUE   arg_no_default_value

#ifdef __cplusplus
extern "C" {
#endif

/**
 * arg_type enums are used in the argument table to define
 * the data type of a command line argument.
 */
typedef enum
  {
  arg_int=0,    /**< Integer value. */
  arg_dbl,      /**< Double value. */
  arg_str,      /**< Ascii string; may be quoted or un-quoted.*/
  arg_bool,     /**< Boolean; accepts the keywords \em yes, \em no, \em true, \em false, \em on , or \em off and converts them into an integer value of 0 (negative) or 1 (affirmative) accordingly. */
  arg_lit       /**< Literal; returns 1 if a given literal string was present on the command line otherwise returns the default value. */
  } arg_type;

/**
 * A fixed array of strings that are used when arguments are
 * given NULL names.
 * The array is indexed by arg_type, with each name describing
 * the corresponding data type.
 * \code
 * arg_str[arg_int]  = "<int>";
 * arg_str[arg_dbl]  = "<double>";
 * arg_str[arg_str]  = "<string>";
 * arg_str[arg_bool] = "<bool>";
 * arg_str[arg_lit]  = "";
 * \endcode
 */
extern const char* arg_typestr[];

/**
 * An argument table is defined an array of arg_rec structs
 * having one entry for each command line argument that is expected.
 * It is most conveniently defined and initialised as a
 * static array (but need not be so).
 *
 * \code
 * //command line arguments will be written into these variables
 * int x,y,z,verbose,debug;
 * double radius;
 * char infname[100];
 * char outfname[100];i
 *
 * //The argument table.
 * arg_rec argtable[] =
 *   // TAG        NAME         TYPE     STORAGE   DEFAULT DESCRIPTION
 *   {
 *   { NULL,       "x",         arg_int,  &x,       NULL,  "x coord" },
 *   { NULL,       "y",         arg_int,  &y,       NULL,  "y coord" },
 *   { NULL,       "z",         arg_int,  &z,       "0",   "z coord" },
 *   { "-r ",      NULL,        arg_dbl,  &radius,  "1.0", "radius" },
 *   { "-o ",      "<outfile>", arg_str,  outfname, "-",   "output file" },
 *   { "-verbose", NULL,        arg_lit,  &verbose, "0",   "verbose output" },
 *   { NULL,       "<infile>",  arg_str,  infname,  NULL,  "input file" },
 *   { "debug=",   "<on/off>",  arg_bool, &debug,   "off", NULL },
 *   };
 * const size_t narg = sizeof(argtable)/sizeof(arg_rec);
 * \endcode
 */
typedef struct
  {
  /**
  * The argument tag string defines the tag that identifies the argument on the command line.
  * Tags may be any string you choose provided they have no whitespace.
  * Examples of common tag string styles are "mytag:", "-mytag", "mytag=".
  * An argument can be specified without a tag by setting the tag string to NULL.
  * Untagged argument are taken from the command line from left to right after
  * all the tagged arguments (if any) have first been extracted.
  */
  const char *tagstr;
  /**
  * The argument name has no effect on the argument processing.
  * It is simply a descriptive name used to represent the argument in the
  * output of the arg_syntax() and arg_glossary() functions.
  * The argument name can be whatever you want, and is a convenient place
  * to communicate the default value to the user if you so wish,
  * for example "\<size\>=1024".
  * If a NULL name string is given, it is automatically replaced by the
  * argument's data type enclosed in angled brackets, as in "\<integer\>".
  * If you dislike such behaviour, you can effectively suppress the name by
  * defining it as an empty string.
  */
  const char *argname;
  /**
  * This defines the data type associated with a command line argument.
  * It supports integer, double, string and boolean data types as well
  * as literal argument strings.
  *
  * Strings may appear on the command line either quoted or unquoted.
  *
  * Booleans expect one of the keywords "true", "false", "yes", "no",
  * "on", or "off" to appear on the command line. These are converted to 0
  * (false,no,off) or 1 (true,yes,on) and stored as an integer.
  *
  * Literals are command line arguments with no associated data value,
  * they are used to define keyword strings that can be used as command line switches.
  * The string literal can be defined in either the tag string or the name string
  * fields. If you use the tag string then the literal, like other tagged
  * arguments, may appear anywhere on the command line. On the other hand,
  * if you use the name string, then the literal must appear in that
  * argument's position just as for normal untagged arguments.
  * When a string literal is succesfully scanned, an integer value of 1 is
  * written into its user-suplied program variable, otherwise it is assigned
  * the default value (if it has one). If there is no default value, then the
  * literal is regarded as a mandatory argument the same as for any other
  * argument.
  *
  */
  arg_type    argtype;
  /**
  * Points to a user-defined variable into which the command line value will
  * be written.
  * It is imperative that the data type of the user-defined variable matches
  * the argtype field, otherwise you can expect very spurious behaviour.
  * \li \em arg_int arguments require \em int storage.
  * \li \em arg_double arguments require \em double storage.
  * \li \em arg_str arguments require a \em char buffer big enough to store
  * the expected result (you'll have to guess what big enough is!).
  * \li \em arg_bool arguments require \em int storage.
  * \li \em arg_lit arguments require \em int storage.
  *
  * Lastly, a NULL placed in this field will cause the argument value to be scanned
  * in the normal way, but the resulting value will be discarded.
  */
  void       *valueptr;
  /**
  * The default string contains an optional default value to be used
  * should the argument be missing from the command line. All defaults are
  * defined as strings (as they would appear on the command line) and converted
  * to the appropriate data type during processing.
  *
  * If a default is specified as NULL then that argument is regarded as
  * mandatory, meaning that a parse error will result if the argument
  * was not given on the command line.
  *
  * Alternatively one may specify ARGTABLE_NO_DEFAULT_VALUE. When this
  * special value is encountered, defaults assignment is suppressed.
  * If the argument was not given on the command line storage pointed
  * by valueptr is not changed.
  */
  const char *defaultstr;
  /**
  * The argument description string, like the name string, does not
  * affect argument processing. The arg_glossary() function
  * uses it to display additional descriptions of command line arguments.
  * Arguments with NULL description strings are omitted from the glossary
  * altogether.
  */
  const char *argdescrip;
  } arg_rec;

/**
 * \brief Parse the command line as per a given argument table.
 *
 *  Attempts to resolve the argv[] command line arguments
 *  (ignoring argv[0]) with the specifications given in the argument table.
 *  The values scanned from the command line are written directly into
 *  the program variables nominated by each argument table entry.
 *
 *  During the process, a copy of the command line is written
 *  (as a single line of space separated arguments) into the
 *  user-supplied string at *CmdLine in case it is needed in future
 *  for error reporting.
 *
 *  Should there be any conflict between the command line arguments
 *  and the argument table specifications, an error message and
 *  corresponding error marker are written into the user-supplied
 *  strings at *ErrMsg and *ErrMark respectively, after which the
 *  function returns zero. The error marker string is used to store
 *  a string of tilde characters formated in such a way that the tildes
 *  underscore the exact location of the error in *CmdLine when the
 *  strings are aligned one above the other. This can be useful for
 *  including in on-line error messages to help the user quickly
 *  pinpoint the cause of the error.
 *
 *  If, on the other hand, all arguments were resolved successfully
 *  then *ErrMsg and *ErrMark are set to empty strings and the
 *  function returns 1.
 *  Either way, CmdLine, ErrMsg, or ErrMark can be safely ignored
 *  by passing them as NULL.
 *
 *  \returns 1 upon success, 0 upon failure.
 */
int arg_scanargv(int argc,          /**< number of entries in argv[]. */
                 char **argv,       /**< command line arguments. */
                 arg_rec *argtable, /**< pointer to the argument table. */
                 int n,             /**< number of entries in argtable[]. */
                 char* CmdLine,     /**< pointer to storage for command line (may be NULL). */
                 char* ErrMsg,      /**< pointer to storage for error message (may be NULL). */
                 char* ErrMark      /**< pointer to storage for error marker (may be NULL). */
                 );

/**
 * \brief Parse a string as per a given argument table.
 *
 * This function is much like arg_scanargv() except that is scans
 * the arguments from the string at *str rather than from argv[].
 * The string is expected to contain a single line, space separated
 * list of arguments, like that generated by arg_catargs().
 *
 * In a departure from \b arg_scanargv, this function erases the scanned
 * arguments from \em *str by overwriting them with spaces once they have been
 * successfully scanned.
 * Furthermore, this function does not throw an error if there are still
 * arguments remaining in *str after the argtable has been fully processed.
 * Thus, complicated argument usages can be achieved by invoking this
 * function multiple times on the same command line string,
 * each time applying a different argument table until the arguments
 * have been exhausted, or an error has been detected.
 *
 * \returns 1 upon success, 0 upon failure.
 */
int arg_scanstr(char* str,         /**< pointer to command line string. */
                arg_rec *argtable, /**< pointer to the argument table. */
                int n,             /**< number of array elements in argtable[]. */
                char* ErrMsg,      /**< pointer to storage for error message (may be NULL). */
                char* ErrMark      /**< pointer to storage for error marker (may be NULL). */
                );


/**
 * \brief Generates a 'usage' syntax string from an argument table
 *
 * Builds a syntactical description of the allowable command line arguments
 * specified by the 'argtable' array.
 * The resulting string is stored in static data within the local scope of
 * this function. Its contents are overwritten by subsequent calls.
 *
 * The syntactical description is generated as a single line of space
 * separated argument descriptors, each comprising of the argument's tag
 * string and name string concatenated together. For example,
 *
 * "myprog x y [z] [-r <double>] [-o <outfile>] [-verbose] <infile> [debug=<on/off>]"
 *
 * If an argument is optional (has a non-NULL default value) then its
 * descriptor is enclosed in square brackets.
 * NULL name strings are substituted with the argument's data type enclosed
 * in angled brackets, as in <int>, <double>, or <string>.
 * If both the tag and the name are empty strings ("") then the argument is
 * omitted from the description altogether. This allows the suppression of
 * individual arguments that you do not want to appear.
 *
 * \returns a pointer to the internal string.
 */
const char* arg_syntax(const arg_rec* argtable,  /**< pointer to the argument table */
                       int n                     /**< number of array elements in argtable[] */
                       );

/**
 * \brief Generate a glossary string from an argument table.
 *
 * Returns a pointer to an internal 'glossary' string which
 * contains a multi-line description of each of the argument table
 * entres that have a non-NULL \<description\> field.
 * The contents of the glossary string remain unaltered up until the
 * next invocation of this function.
 * Each line of the glossary string is formatted as
 *
 * "\<prefix\>\<tag\>\<name\>\<description\>"
 *
 * The 'prefix' string is useful for adding indenting spaces
 * before each line in the description to improve the look of
 * the glossary string, or it can be given as NULL in which
 * case it is ignored.
 *
 * If 'prefix' string starts with '\001', the leading character
 * is stripped and remaining part is interpreted as format string
 * in the form "%s%s\n". First %s is substituted with concatenated
 * tag and name, and second %s is substituted with description.
 * If you like description lines to be aligned, pass something like
 * this: "\001    %-30s %s\n".
 *
 * Any NULL \<tag\> fields in the argument table will appear in the
 * glosssary as empty strings.
 *
 * Any NULL \<name\> fields will be substituted by a description of
 * that argument's data type, enclosed in angled brackets, as in
 * \<int\> and \<double\>.
 * A name can effectively be suppressed from the glossary by defining
 * it as an empty string in the argument table.
 *
 * \returns a pointer to the internal string.
 */
const char* arg_glossary(const arg_rec* argtable,  /**< pointer to the argument table */
                         int n,                    /**< number of array elements in argtable[] */
                         const char* prefix        /**< a string to be prefixed to each line of the output */
                         );

/**
 * \brief Concatenate all argv[] arguments into a single string.
 *
 * Concatenates all of the arguments in the argv[] array and writes the
 * result into *str as a single line, space separated string.
 *
 * Any argv[] entries that contain whitespace are automatically
 * encapsulated by single quotes prior to the concatenation to preserve
 * their word grouping.
 * A trailing space is always appended to the resulting string as a
 * safety precaution in lieu of scanning for string literals that expect
 * trailing space.
 * It is assumed that *str is big enough to store the result.
 */
extern void arg_catargs(int argc,    /**< number of arguments in argv[] */
                        char **argv, /**< command line arguments */
                        char *str    /**< pointer to destination string */
                        );


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
                   char *argdescrip   /**< argument description string */
                   );

/**
 * \brief Print the contents of an argument table.
 *
 * The contents of the argument table, and the user-supplied
 * variables it references,  are printed to the stream 'fp'.
 * This can be useful for debugging argument tables.
 */
void arg_dump(FILE* fp,                /**< output stream  */
              const arg_rec* argtable, /**< pointer to the argument table */
              int n                    /**< number of array elements in argtable[] */
              );

extern const char *arg_no_default_value;

#ifdef __cplusplus
}
#endif



/**
\mainpage Introduction to Argtable

\section LegalNotice Legal notice.
The argtable library and accompanying documentation is copyright
1998, 1999, 2001 Stewart Heitmann (sheitmann@users.sourceforge.net).
Argtable is free
software; you can redistribute it and/or modify it under the terms of
the <b>GNU Library General Public License</b> as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

\section Overview Overview.
Argtable (http://argtable.sourceforge.net) is a freely available programmer's library for parsing
the command line arguments of any C/C++ program. It allows the
programmer to specify the desired format of the command line
arguments in one or more statically defined arrays known as argument
tables. Each row of an argument table specifies the data type of
an expected argument and nominates a user-defined program variable as
storage for the incoming argument value. If arguments require default
values, then these too are specified in the argument table.

Once an argument table has been established, parsing the command line is
simply a matter of calling the library's arg_scanargv() function which
attempts to resolve the contents of \b argv[] with the entries
of the argument table. If successful, the command line arguments are
now known to be valid and their values are ready and available for
use in their nominated program variables.

If the arguments could not be successfully resolved then
\b arg_scanargv returns an error message string describing the
reason for the failure and showing the location of the error in
the command line. The program can simply print the error message
to stdout or stderr and exit.
\code
ERROR: myprog grad:13 99 uh oh
                         ^^ unexpected argument
\endcode

Alternatively, if the program has multiple command line usages then
it may choose to call \b arg_scanargv several times each with a different
argument table until a successful match is found or all argument tables
are exhausted.

Auxilliary functions arg_glossary() and arg_syntax() generate plain text
descriptions of the arguments defined in an argument table and their
command line syntax.
These make it easy to generate on-line help facilities that are always current.

\section ArgStyles Styles of command line arguments.
Argtable supports both \em tagged and \em untagged command line arguments.
Tagged arguments are identified by a prefix tag, as in
<b>-o file</b> or <b>title:mystuff</b>. The tag enables these arguments to
appear anywhere on the command line, and in any order. The programmer
may implement any style of argument tag desired, including such common
styles as <b>-title mystuff</b>, <b>title:mystuff</b>, <b>--title mystuff</b>,
or <b>title=mystuff</b>.
Untagged arguments on the other hand have no prefix; they are identified
strictly by their ordering on the command line.
The two styles of arguments may be freely mixed,
whereupon the tagged arguments are always processed first, leaving
any remaining (untagged) arguments to be scanned from left to
right.

A command line argument may be of type \b integer,
\b double, \b string, or \b boolean. Doubles are accepted in
in either floating point or scientific notation, and strings may be
either quoted or unquoted. Booleans will accept any of the keywords
\b on, \b off, \b yes, \b no, \b true, or \b false
and yield an integer value of 0 (negative) or 1 (affirmative)
accordingly.

A special argument type called \b literal is also provided;
it yields an integer value according to the presence or
absence of a given string literal on the command line. It is useful
for specifying unparameterised command line switches such as \b -verbose
and \b -help.

\section OptionalArgs Optional arguments and default values.
Arguments may be assigned default values that take effect when
no matching command line argument could be found. When a
default value is specified for an argument you are, in effect,
declaring that argument as being optional.
Arguments without defaults are, by definition, regarded as mandatory arguments.

\section Platforms Supported platforms.
Argtable conforms to ansi C requirements and should compile on
any standard ansi C compiler. To date, it has been successfully
compiled on:
\li MIPSpro C/C++ on IRIX 6.2, 6.3 and IRIX64 6.2
\li DEC C/C++ on Digital Unix V4.0 (OSF/1)
\li GNU gcc/g++ on DEC Digital Unix V4.0 (OSF/1); IRIX 6.2, 6.3;
IRIX64 6.2; Linux 2.0.30; and SunOS 5.5.1.

Please let me know if you have successfully used argtable on other platforms.

\section InstallSection Installing Argtable
The fastest and easiest way to use argtable is simply to copy
the \b argtable.h and \b argtable.c files into your project and compile
them with the rest of your code.

If you are a system administrator, you may wish to install argtable
on your system as a programmer's library, complete with man pages and
html documentation.
This is easy to do as the makefiles follow the usual \b autoconf procedure.
    -# \b cd to the argtable package directory and type
       \b ./configure to configure it for your system.
    -# Type \b make to compile the package.
    -# Optionally, type \b make \b check to run the self-tests.
    -# \b su as root user.
    -# Type \b make \b install to install the programs and documentation.
     By default the package installs into \b /usr/local/ but a different location
     may be specified when configuring the package. Type \b ./configure \b --help
     for the full list of configuration options.
    -# logout from root user.
    -# Type \b make \b clean to remove the temporary binaries created during compilation.

\section GetStarted Getting started.
\ref Tutorial is the best place to get started.
Then look at the example code supplied with the distribution.

\section Others Similar packages.
Here are some other command line parsing tools that I am aware of.
Apologies for any I may have omitted.
\li \b clig: The Command Line Interpreter Generator:
    http://wsd.iitb.fhg.de/~kir/clighome/

\li \b opt: The Options and Parameter parsing Toolkit:
    ftp://ftp.lanl.gov/pub/users/jt/Software/opt/opt_toc.html

\li \b getopt: The GNU-style options parser.
    This comes as standard on most unix systems.
*/



/**
\page Tutorial The Argtable Tutorial
Imagine we have written a program called \b myprog and we wish
to implement the following command line usage syntax for it:
\verbatim
myprog [-tit <title>] grad:gradient [y-int]
   -tit <title>    your title
   grad:gradient   line gradient
   y-int           y-intercept
\endverbatim

We will create a single argument table in our program
that defines the argument properties, and pass that
table along with \b argc and \b argv[] to the
arg_scanargv() function to do the parsing.

\section Argtable Defining the argument table.
An argument table is just an array of \b arg_rec structs, with each
array element pertaining to a single command line argument. The
arg_rec struct is defined in argtable.h as:
\code
typedef enum {arg_int=0,arg_dbl,arg_str,arg_bool,arg_lit} arg_type;
typedef struct
   {
   const char *tagstr;       // argument tag string
   const char *argname;      // argument name string
   arg_type    argtype;      // argument data type
   void       *valueptr;     // ptr to user storage location
   const char *defaultstr;   // default value, as a string
   const char *argdescrip;   // argument description string
   } arg_rec;
\endcode

Thus we may define our argument table statically in the code as follows:
\code
int main(int argc, char **argv)
   {
   static char str[50];
   static double grad;
   static int c;
   arg_rec argtable[] =
      {
      {"-tit ", "<title>",  arg_str, str,   "noname", "\t\t your title"},
      {"grad:", "gradient", arg_dbl, &grad, NULL,     "\t line gradient"},
      {NULL,    "y-int",    arg_int, &c,    "0",      "\t\t y-intercept"}
      };
   const size_t narg = sizeof(argtable)/sizeof(arg_rec);
   ...
  }
\endcode

Defining the tables statically is a programming convenience but not a
requirement; the table could equally well have been dynamically allocated
and initialized at runtime.
Notice that I also chose to define the argument table within
the main() block because that's the only place where it is
used so there is no need to promote it to a higher namespace. However
you may define it in the global namespace if you prefer.

Our argument table has three rows, one for each command line argument
<b>-tit \<title\></b>, <b>grad:gradient</b>, and <b>y-int</b>.
The behaviour of the argument parsing is governed entirely by the
contents of the various fields (columns) of the argument table. Lets
step through each field one by one.

\par The tag string:
The first field is the argument's tag string.
It defines the prefix literal that identifies a tagged argument and
should contain at least one non-whitespace character unless
the argument is untagged whereupon the field should be NULL.
In our example, <b>-tit \<title\></b> and <b>grad:gradient</b> are
tagged arguments but <b>y-int</b> is untagged so it has a NULL
tag string.

\par The name string:
The second field is the argument's name string.
It is not actually used to process the command line
arguments, rather it defines the name of the argument as it appears
in the description strings generated by the \b arg_syntax and
\b arg_glossary functions. Those functions
automatically substitute any NULL names with the argument's data
type enclosed in angled brackets, as in "\<int\>" or "\<string\>".

\par The data type:
The third field is an enumerated type that
defines the data type of the command line argument. Possible values
are \b arg_int, \b arg_dbl, \b arg_str, \b arg_bool, and \b arg_lit.
They represent integer, double, string, boolean, and literal arguments
respectively. In our example <b>-tit \<title\></b> expects
\em \<title\> to be substituted by a string value,
<b>grad:gradient</b> expects \em gradient to be a double, and
<b>y-int</b> is expected to be an integer.

\par The data pointer:
The fourth field is a pointer-to-void
that gives the address of the user-defined program variable used to
store the argument's value. A NULL pointer here causes the value to
be discarded once is has been scanned. Take care that the data
type of of the target memory location matches that specified in the
previous column. Arguments of type \b arg_int, \b arg_bool,
and \b arg_lit must each point to an \em integer variable.
Those of type \b arg_dbl must point to a \em double and those
of \b arg_str must point to a \em char \em array.
In our example, the string value associated with <b>-tit \<title\></b>
is written into the \em char \em str[50] buffer, the double value
associated with \b grad:gradient is written into \em double \em grad,
and the integer value associated with <b>y-int</b> is written into
\em int \em c.

\par The default value:
The fifth field is a string which contains an optional default value
for the argument. It is defined as a string and automatically cast to
the appropriate data type at run time. A NULL value indicates no default.
In our example, <b>-tit \<title\></b> and <b>y-int</b> have default values of "noname"
and zero respectively, whereas <b>grad:gradient</b> has no default
and is thus regarded as a mandatory argument.

\par The description string:
The sixth and final field allows
the programmer to enter a brief description of the argument. It is
these descriptions that are output by the arg_glossary() function.
A NULL value causes that entry to be omitted from the glossary output.

\section Parsing Parsing the command line.
Having defined the argument table, we can now use it to parse the command line
arguments in argv[]. There are several ways to do this, but the
simplest is to use the \b arg_scanargv function.
\code
int arg_scanargv(int argc,
                 char** argv,
                 arg_rec *argtable,
                 int n,
                 char* CmdLine,
                 char* ErrMsg,
                 char* ErrMark);
\endcode

It takes as input the command line arguments in \b argv[]
(there are \b argc of them) and a pointer to the argument table in
\b argtable (which has \b n rows). It proceeds to scan the argv[]
arguments (skipping argv[0]) and tries to resolve them with
the entries given in the argument table. If this can be done
successfully then all argument values are written into the program
variables as designated by the argument table and the function
returns 1. If not, the function returns 0 to indicate failure.

The three string pointers \b CmdLine, \b ErrMsg, and \b ErrMark
refer to user defined string buffers in which \b arg_scanargv
returns information about the parsing. They are optional parameters
in the sense that they may be given as NULL if you do not wish to use them.

\b CmdLine is always assigned a copy of the orginal command line,
concatenated into a single space-separated string.

\b ErrMsg and \b ErrMark are only used when \b arg_scanargv detects a
parse error in the command line.
In those cases, \b ErrMsg is assigned an explantory error message string,
and \b ErrMark is assigned a string of tilde characters which have been
formatted in such a way as to highlight the exact location of the error
in \b CmdLine when printed one atop the other.

The code fragment below demonstrates the use of \b arg_scanargv.
It presumes that \em argc, \em argv, \em argtable, and \em narg are
as defined in the example above.
\code
{
char cmdline[200], errmsg[200], errmark[200];

if (!arg_scanargv(argc,argv,argtable,narg,cmdline,errmsg,errmark))
   {
   // arg error occurred, print error message and exit
   printf("ERROR: %s\n", cmdline);
   printf("       %s %s\n", errmark, errmsg);
   return 1;
   }

// only get here if the arguments were scanned successfully

}
\endcode

And here are some examples of the console output that this code produces when
\b arg_scanargv detects a parse error.
\code
$ myprog grad:oops
ERROR: myprog grad:oops
                   ^ invalid grad:gradient argument
$ myprog grad:13 nope
ERROR: myprog grad:13 nope
                      ^^^^ unexpected argument
$ myprog grad:13 99 uh oh
ERROR: myprog grad:13 99 uh oh
                         ^^ unexpected argument
\endcode

\section OnlineHelp Generating online help.
The arg_syntax() and arg_glossary() functions take an argument table
and generate plain text descriptions of its command line syntax as
well as descriptions of the individual arguments.
These are useful for displaying help text to the user.

\code
const char* arg_syntax(const arg_rec* argtable, int n);
const char* arg_glossary(const arg_rec* argtable, int n, const char* prefix);
\endcode

The \b arg_syntax function returns a pointer to an internal
string buffer that contains a plain text description of the usage
syntax of the argument table it was passed. The string comprises a
space separated list of the tag and name strings of each argument
table entries concatenated into a single line string. Optional
command line arguments are automatically enclosed in square brackets.
Calling \b arg_syntax on our example argument table would
return the string:
\verbatim
[-tit <title>] grad:gradient [y-int]
\endverbatim

The calling program would ordinarily prepend the program
name from argv[0] to this to get the full command line usage syntax.

The \b arg_glossary function is similar, except it generates a
multi-line text string with one argument per line. Each line includes
the argument's tag, its name string, and its description string as
given in the argument table. Arguments that have a NULL description
string are omitted. Each line of the glossary string is prefixed with
the string given in the prefix parameter; it useful for
indenting each line of the string. Calling \b arg_glossary with
our example argument table results in the following multi-line
string:
\verbatim
-tit \<title\> your title
grad:gradient line gradient
y-int y-intercept
\endverbatim

\section Altogether Putting it all together.
Lets return to our example program and put it all together in its
entirety.
Our program, when executed without any arguments
(argc==1), will print the argument usage syntax and a glossary
on stdout, then exit.
When given a valid set of arguments, it will
display the resulting argument values as they are stored in the local
program variables. Otherwise, it echoes the erroneous command line
together with an appropriate error message and terminates
with error code 1.

\code
#include "common/argtable.h"

int main(int argc, char **argv)
   {
   static char str[50];
   static double grad;
   static int c;
   arg_rec argtable[] =
      {
      {"-tit ", "<title>",  arg_str, str,   "noname", "\t\t your title"},
      {"grad:", "gradient", arg_dbl, &grad, NULL,     "\t line gradient"},
      {NULL,    "y-int",    arg_int, &c,    "0",      "\t\t y-intercept"}
      };
   const size_t narg = sizeof(argtable)/sizeof(arg_rec);

   // process the command line args
   if (argc==1)
      {
      // display program usage and exit.
      printf("Usage: %s %s\n", argv[0], arg_syntax(argtable,narg));
      printf("%s\n",arg_glossary(argtable,narg,"  "));
      return 0;
      }
   else
      {
      // scan command line arguments from argv[]
      char cmdline[200], errmsg[200], errmark[200];
      if (!arg_scanargv(argc, argv, argtable, narg, cmdline, errmsg, errmark))
         {
         // arg error occurred, print error message and exit
         printf("ERROR: %s\n", cmdline);
         printf("       %s %s\n", errmark, errmsg);
         return 1;
         }
      }

   // get here only if command line args ok
   printf("title: \"%s\"\n",str);
   printf("gradient: %f\n",grad);
   printf("y-intercept: %d\n",c);

   return 0;
   }
\endcode

Here are some results of running myprog with various command
line arguments.
\verbatim
$ myprog
Usage: myprog [-tit <title>] grad:gradient [y-int]
  -tit <title>     your title
  grad:gradient    line gradient
  y-int            y-intercept

$ myprog grad:10
title: "noname"
gradient: 10.000000
y-intercept: 0

$ myprog 7 grad:1.234
title: "noname"
gradient: 1.234000
y-intercept: 7

$ myprog -tit "hello world" grad:3.33 11
title: "hello world"
gradient: 3.330000
y-intercept: 11
\endverbatim

*/


/**
\page Release Release Notes.

\par Argtable-2.0 coming eventually.
Argtable-2.0 will be a major overhaul of the code. The
changes are required to address the most common complaint about
argtable; the potential for buffer overruns as argtable writes into
fixed size string buffers. The redesign will bring some inevitable
changes to the library interface, but the basic look and feel of the
argument tables will stay the same.
I been promising to finish Argtable-2.0 for a very long time.
Well, one of the days....

\par Argtable-1.3 released December 20, 2001.
Moved argtable to a new home on the \b sourceforge site and revamped
the documentation.
Documentation is now created with \b Doxygen instead of \b c2man.
Also fixed some minor bugs in the Makefiles.
The source code itself is unaltered.

\par Argtable-1.2 released August 5, 1999.
The original makefiles have been replaced by autoconf makefiles.
The char pointers in the argument table have been redefined as
pointers to const char.
Some of argtable's internal string buffers have been made larger to
accommodate long command lines, and a bug that occurred when program
names contained whitespace has been fixed. The documentation has
also been revised.

\par Argtable-1.1 released January 20, 1999.
This version fixes some cross-platform compilation errors, and saw
the introduction of the multi-platform configuration.
It also saw the addition of the arg_record() function and a change
to the arg_scanargv() function so that it no longer requires argv[0]
to be the first entry of the argument table.
To maintain backwards compatibility, programs
written for version 1.0 should now define the macro #define
ARGTABLE_COMPATIBILITY_10 prior to including the argtable.h header file.

\par Argtable-1.0 released November 13, 1998.
Argtable's debut!
*/

#endif





