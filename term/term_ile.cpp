/*
 * File:  term_ile.cpp
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#ifndef ENABLE_LIBEDIT
#include <cctype>

int ile_init()
{
    return 0;
}

void ile_deinit()
{
}

char *ile_gets(size_t * sz)
{
    return NULL;
}

const char *get_query_from_term_buffer()
{
    return NULL;
}

#else /* ifndef ENABLE_LIBEDIT */

#include <cstdio>
#include <assert.h>
#include <cctype>
#include <cstring>
#include <cassert>
#include <pwd.h>
#include "term_globals.h"
#include "complet.h"
#include <readline.h>

// readline defines history as a separate library
#ifdef USE_HISTORY_H
    #include <history.h>
#endif

#include "common/u/u.h"
#include "common/u/uprocess.h"

using namespace term_globals;

static int se_analyze_line(char *, int lineno); // check if we need another line
static int multiline = 0; // number of lines in a query

static char *history_file = NULL;

// get rid of '\n' in history
// needed since multiline queries get messed up
static void
make_safe_history()
{
    HIST_ENTRY *hist_ent;
    char       *hist_line_char;

    history_set_pos(0);
    for (hist_ent = current_history(); hist_ent; hist_ent = next_history())
        for (hist_line_char = (char *)hist_ent->line; *hist_line_char; hist_line_char++)
            if (*hist_line_char == '\n') *hist_line_char = 0x01;
}

// reverse make_safe_history
static void
restore_history()
{
    HIST_ENTRY *hist_ent;
    char       *hist_line_char;

    for (hist_ent = current_history(); hist_ent; hist_ent = next_history())
        for (hist_line_char = (char *)hist_ent->line; *hist_line_char; hist_line_char++)
            if (*hist_line_char == 0x01) *hist_line_char = '\n';
}


void ile_deinit()
{
    // save history to file (~/.sehistory)
    if (history_file)
    {
        make_safe_history();
        write_history(history_file);
        free(history_file);
    }
}

// returns user home directory
static
char *get_home_dir()
{
#ifndef _WIN32
    struct passwd *pw = NULL;
    char *res = NULL;

    res = (char *)malloc(U_MAX_PATH);
    assert(res);

    // first try to search in environment
    if (!uGetEnvironmentVariable("HOME", res, U_MAX_PATH, __sys_call_error))
        return res;

    // else try passwd file
    pw = getpwuid(getuid());

    if (pw && pw->pw_dir && strlen(pw->pw_dir) <= U_MAX_PATH)
    {
         strcpy(res, pw->pw_dir);
         return res;
    }

    free(res);
    return NULL;
#else /* #ifndef _WIN32 */
    return NULL;
#endif /* #ifndef _WIN32 */
}

int ile_init()
{
    char *home = NULL;
    rl_readline_name = (char *)"se_term";
    rl_attempted_completion_function = term_complet;
    rl_basic_word_break_characters = (char *)"\t\n ";

    if (rl_initialize())
        return 0;

    using_history();
    // load history from file (~/.sehistory)
    home = get_home_dir();
    if (home)
    {
        history_file = (char *)malloc(U_MAX_PATH);
        assert(history_file);

        snprintf(history_file, U_MAX_PATH, "%s/%s", home, ".sehistory");

        free(home);
        read_history(history_file);
        restore_history();
    }

	return 1;
}

char *query_buffer = NULL; // we make it global because we must return it to completion function

const char *get_query_from_term_buffer()
{
    return query_buffer;
}

char *ile_gets(size_t * sz)
{
	char *line = NULL;

	int count = 0, max_len = 2048;
	char dummy[4]={0,0,0,0};

    query_buffer = NULL;

    if (!(query_buffer = (char *)malloc(max_len)))
        return NULL;

    query_buffer[0] = '\0';
    multiline = 0;

    do
    {
        free(line); // its ok on NULL according to standard
        line = readline(multiline ? micro_prompt : prompt);
        if (line)
        {
            count += strlen(line);
            if (count + 1 > max_len)
            {
                max_len += count + 1;
                if (!(query_buffer = (char *)realloc(query_buffer, max_len)))
                    return NULL;
            }
            strcat(query_buffer, line);
            strcat(query_buffer, "\n");
            count++; // for '\n'
        }
        else
        {
            // caught EOF
            free(query_buffer);
            query_buffer = NULL;
            return NULL;
        }

    } while (line && !se_analyze_line(line, multiline++));

    free(line);

    if (count > 0 && 1==sscanf(query_buffer,"%1s",dummy)) /* true if the line consists any non-WS character */
	{
        // strip lasn '\n' character
        query_buffer[--count] = '\0';
        add_history(query_buffer);
	}
	if (sz) *sz = count;
    return query_buffer;
}

int se_analyze_line(char *buffer, int lineno)
{
    const char *lastchar = buffer + strlen(buffer), *i;
    int res;

    assert(buffer);

    /* should we accept input? */
    // find first non-space character
    for (i = buffer; i < lastchar && isspace(*i); ++i)
        ;

    if (!lineno && (i == lastchar || *i == '\\'))
    {
        // user hits Enter and entire input contains nothing but whitespace
        // or it is a special command -- accept it
        res = 1;
    }
    else if (*(lastchar - 1) == '&')
        //  input is & terminated -- accept it
        res = 1;
    else
        // some usual line -- continue to append
        res = 0;

    return res;
}

#endif /* ifndef ENABLE_LIBEDIT */
