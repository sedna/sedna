/*
 * simple_prompt
 *
 * Generalized function especially intended for reading in usernames and
 * password interactively. Reads from /dev/tty or stdin/stderr.
 *
 * prompt:		The prompt to print
 * maxlen:		How many characters to accept
 * echo:		Set to false if you want to hide what is entered (for passwords)
 *
 * Returns a malloc()'ed string with the input (w/o trailing newline).
 */

//#ifdef HAVE_TERMIOS_H
//#include <termios.h>
//#else
//#ifdef WIN32
#ifdef _WIN32
#include <windows.h>
#else
#include <termios.h>
//#undef ERROR
#endif
//#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


char *
simple_prompt(const char *prompt, int maxlen, int echo);
