/*
 * File:  complet.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * This file implements some kind of tab-completion using readline facilities. It can be used either with libedit
 * or readline libraries or, in fact, any other library that respects the same completion mechanism.
 *
 */

#ifdef ENABLE_LIBEDIT

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <readline.h>
#include "term_ile.h"

// table of chains, which contains all possible alternatives to match
#define MAX_CHAINS 28
const char *chain_table[][13][11] =
{
    {{"UPDATE", NULL}, {"insert", NULL}, {"@any", NULL}, {"into", "preceding", "following", NULL}, {NULL}},
    {{"UPDATE", NULL}, {"delete", "delete_undeep", NULL}, {NULL}},
    {{"UPDATE", NULL}, {"replace", NULL}, {"@any", NULL}, {"in", NULL}, {"@any", NULL}, {"with", NULL}, {"@any", NULL}, {NULL},},
    {{"UPDATE", NULL}, {"replace", NULL}, {"@any", NULL}, {"as", NULL}, {"@any", NULL}, {"in", NULL}, {"@any", NULL},
        {"with", NULL}, {NULL},},
    {{"UPDATE", NULL}, {"rename", NULL}, {"@any", NULL}, {"on", NULL}, {NULL}},
    {{"LOAD", NULL}, {"@file", NULL}, {NULL}},
    {{"LOAD", NULL}, {"STDIN", NULL}, {NULL}},
    {{"LOAD", "DROP", NULL}, {"MODULE", NULL}, {NULL}},
    {{"LOAD", NULL}, {"OR", NULL}, {"REPLACE", NULL}, {"MODULE", NULL}, {NULL}},
    {{"CREATE", "DROP", NULL}, {"DOCUMENT", NULL}, {"@any", NULL}, {"IN", NULL}, {"COLLECTION", NULL}, {NULL}},
    {{"CREATE", "DROP", NULL}, {"DOCUMENT", NULL}, {NULL}},
    {{"CREATE", "DROP", NULL}, {"COLLECTION", NULL}, {NULL}},
    {{"CREATE", NULL}, {"INDEX", NULL}, {"@any", NULL}, {"ON", NULL}, {"@any", NULL}, {"BY", NULL}, {"@any", NULL},
        {"AS", NULL}, {NULL}},
    {{"DROP", NULL}, {"USER", NULL}, {NULL}},
    {{"CREATE", "ALTER", NULL}, {"USER", NULL}, {"@any", NULL}, {"WITH", NULL}, {"PASSWORD", NULL}, {NULL},},
    {{"CREATE", NULL}, {"FULL-TEXT", NULL}, {"INDEX", NULL}, {"@any", NULL}, {"ON", NULL}, {"@any", NULL}, {"TYPE", NULL}, {NULL}},
    {{"CREATE", NULL}, {"TRIGGER", NULL}, {"@any", NULL}, {"BEFORE", "AFTER", NULL},
        {"INSERT", "DELETE", "REPLACE", NULL}, {"ON", NULL}, {"@any", NULL},
        {"FOR", NULL}, {"EACH", NULL}, {"NODE", "STATEMENT", NULL}, {"DO", NULL}, {NULL}},
    {{"CREATE", NULL}, {"ROLE", NULL}, {NULL}},
    {{"RENAME", NULL}, {"COLLECTION", NULL}, {"@any", NULL}, {"INTO", NULL}, {NULL}},
    {{"GRANT", NULL}, {"@any", NULL}, {"ON", NULL}, {"DOCUMENT", "COLLECTION", NULL}, {"@any", NULL}, {"TO", NULL}, {NULL}},
    {{"GRANT", NULL}, {"@any", NULL}, {"ON", NULL}, {"DATABASE", NULL}, {"TO", NULL}, {NULL}},
    {{"GRANT", NULL}, {"@any", NULL}, {"TO", NULL}, {NULL}},
    {{"REVOKE", NULL}, {"@any", NULL}, {"ON", NULL}, {"DOCUMENT", "COLLECTION", NULL}, {"@any", NULL}, {"FROM", NULL}, {NULL}},
    {{"REVOKE", NULL}, {"@any", NULL}, {"ON", NULL}, {"DATABASE", NULL}, {"FROM", NULL}, {NULL}},
    {{"REVOKE", NULL}, {"@any", NULL}, {"FROM", NULL}, {NULL}},
    {{"\\set", "\\unset", NULL}, {"AUTOCOMMIT", "ON_ERROR_STOP", "DEBUG", "TRANSACTION_READ_ONLY", "QUERY_TIMEOUT=", "LOG_LESS_MODE", NULL}, {NULL}},
    {{"\\set?", "\\unset?", "\\?", "\\commit", "\\rollback", "\\showtime", "\\quit", "\\commit", "\\rollback",
        "\\showtime", NULL}, {NULL}},
    {{"\\ac", "\\nac", "\\ro", "\\upd", "\\ll", "\\fl", "\\q", NULL}, {NULL}}
};

// libedit strange behaviour: it's got completion_matches instead of rl_completion_matches
#define rl_completion_matches completion_matches

// we use this global param to tell generator how to complete matches
// Values are:
//      const_comp_generator -- const string
//      fromchain_comp_generator -- compatible chain array
static void *complete_param;

// determines global position of our lookup in chains
static int global_pos = 0;

// creates duplicate of string
// conv == 0 leave string as it is
// conv == 1 convert to upper-case
// conv == 2 convert to lower-case
static
char *str_dup(const char *str, int conv)
{
    int i, text_len;
    char *res;

    if (str == NULL)
        return NULL;

    text_len = strlen(str);
    res = (char *)malloc(text_len + 1);
    if (res == NULL)
        return NULL;

    if (conv)
    {
        for (i = 0; i < text_len; i++)
            res[i] = (char)((conv == 1) ? toupper(str[i]) : tolower(str[i]));
        res[text_len] = '\0';
    }
    else
        strcpy(res, str);

    return res;
}

// this function gives us the next word
// semantics similar to strtok: first call with buf as string
// all other calls on the same string must be with NULL
// returns NULL if cannot find the next word
static
char *get_word(const char *buf)
{
    static const char *inbuf = NULL;
    static int pos = 0;
    unsigned int i, start, end, inquotes = 0;
    char *res = NULL;


    // consider it first call
    if (buf != NULL)
    {
        pos = 0;
        inbuf = buf;
    }

    // no string? then return nothing
    if (inbuf == NULL)
        return NULL;

    // restore position
    i = pos;

    // first skip any spaces
    for (; i < strlen(inbuf) && isspace(inbuf[i]); i++)
        ;

    // nothing found
    if (i == strlen(inbuf))
        return NULL;

    // then try to find  end of word; start would be i
    start = i;
    for (; i < strlen(inbuf); i++)
    {
        if (inbuf[i] == '"')
            inquotes = !inquotes;
        if (isspace(inbuf[i]) && inquotes == 0)
            break;
    }

    end = i - 1;

    res = (char *)malloc(end - start + 2);
    if (res == NULL)
        return NULL;

    strncpy(res, inbuf + start, end - start + 1);
    res[end - start + 1] = '\0';

    // save current position
    pos = i;

    return res;
}

// generates single match with const specified in complete_param
// to be called from libedit
static
char *const_comp_generator(const char *text, int state)
{
    // first time return constant, then return NULL
    if (state == 0)
        return str_dup((char *)complete_param, 0);
    else
        return NULL;
}

// resets chains found flag
static
void reset_chains(int *chains, int count)
{
    int i;

    for (i = 0; i < count; i++)
        chains[i] = 1;
}

// find next chain that is set in found chains flag
// start searching from specified start
static
int find_next_set_chain(int *chains, int count, int start)
{
    int i, res = -1;

    for (i = start + 1; i < count; i++)
        if (chains[i])
    {
        res = i;
        break;
    }

    return res;
}

// generates matches from possible chains detemined at some previous steps
// chains must be passed via complete_param parameter
// to be called from libedit
static
char *fromchain_comp_generator(const char *text, int state)
{
    static int curr_chain = 0, curr_pos_chain = 0, curr_pos_chain_chain = 0;
    static int returned_smth = 0, can_file_comp = 0, text_len, state_to_rlfnc = 0;
    const char **chain;
    static char *uptext, *lotext; // copy of text with toupper, tolower
    char *file_res = NULL, *q_file_res = NULL; // result from filename generator and quoted result (to se_term)

    // for the first time set some params
    if (state == 0)
    {
        curr_chain = find_next_set_chain((int *)complete_param, MAX_CHAINS, -1);
        curr_pos_chain = global_pos;
        curr_pos_chain_chain = 0;
        can_file_comp = 0;
        returned_smth = 0;
        state_to_rlfnc = 0;
        file_res = NULL;

        // get upped text string
        uptext = str_dup(text, 1);
        lotext = str_dup(text, 2);
        text_len = strlen(uptext);
    }

    // if we have something to find then try give another match
    while (curr_chain != -1)
    {
        // get subchain in current_chain
        chain = chain_table[curr_chain][curr_pos_chain];
        if (chain[curr_pos_chain_chain] == NULL)
        {
            curr_chain = find_next_set_chain((int *)complete_param, MAX_CHAINS, curr_chain);
            curr_pos_chain = global_pos;
            curr_pos_chain_chain = 0;
        }
        else if (strcmp(chain[curr_pos_chain_chain], "@any") &&
                 strcmp(chain[curr_pos_chain_chain], "@file") &&
                 (strncmp(uptext, chain[curr_pos_chain_chain], text_len) == 0 ||
                  strncmp(lotext, chain[curr_pos_chain_chain], text_len) == 0))
        {
            returned_smth = 1;
            return str_dup(chain[curr_pos_chain_chain++], 0);
        }
        else
        {
            if (strcmp(chain[curr_pos_chain_chain], "@file") == 0) can_file_comp = 1;
            curr_pos_chain_chain++;
        }
    }

    // nothing to match
    // if we can file-complete here then do it
    if (can_file_comp)
    {
        if (text[0] == '"')
            text++;

        file_res = rl_filename_completion_function(text, state_to_rlfnc);
        state_to_rlfnc = 1;
        if (file_res)
        {
            if ((q_file_res = (char *)malloc(strlen(file_res) + 3)))
            {
                sprintf(q_file_res, "\"%s\"", file_res);
                returned_smth = 1;
                free(file_res);
                return q_file_res;
            }
        }
    }

    // if haven't found any matches return "" to turn off filename completion
    if (!returned_smth)
    {
        returned_smth = 1;
        rl_completion_append_character = '\0'; // to disable matching
        return str_dup("", 0);
    }

    free(uptext);
    free(lotext);

    return NULL;
}

// determine what chains match current word
static
int determine_chains(const char *word, int *chains, int count)
{
    int i, j, found;
    const char **chain;
    char *upword, *loword;

    upword = str_dup(word, 1);
    loword = str_dup(word, 2);

    for (i = 0; i < count; i++)
    {
        if (chains[i] == 0)
            continue;

        chain = chain_table[i][global_pos];
        if (chain[0] == NULL) // end of chain; get rid of it
            chains[i] = 0;
        else if (strcmp(chain[0], "@any") == 0 ||
                 strcmp(chain[0], "@file") == 0) // chain matches any word; let it stay
            continue;
        else
        {
            j = 0;
            while (chain[j] != NULL)
            {
                if (strcmp(upword, chain[j]) == 0||
                    strcmp(loword, chain[j]) == 0)
                    break;

                j++;
            }

            if (chain[j] == NULL) // chain doesn't match
                chains[i] = 0;
        }
        if (chains[i] == 1) // chains stays
            found = 1;
    }

    free(upword);

    return found;
}

char **term_complet(const char *text, int start, int end)
{
    const char *saved_query_buf;
    char *word, *total_query;
    int possible_chains[MAX_CHAINS];
    char **matches = NULL;
    int found = 1;

    // starting from beginning of chains
    global_pos = 0;
    reset_chains(possible_chains, MAX_CHAINS);

    // first we must take query saved in term buffer so we won't be
    // confused with multiline queries
    saved_query_buf = get_query_from_term_buffer();
    total_query = (char *)malloc(strlen(saved_query_buf) + start + 1);
    if (total_query == NULL)
    {
        complete_param = (void *)"";
        return rl_completion_matches(text, const_comp_generator);
    }

    strcpy(total_query, saved_query_buf);
    strncat(total_query, rl_line_buffer, start);

    // try to analyze from first word to determine the possible query chain
    word = get_word(total_query);
    while (word != NULL)
    {
        found = determine_chains(word, possible_chains, MAX_CHAINS);

        // shift position
        global_pos++;

        // nothing found
        if (found == 0)
            break;

        word = get_word(NULL);
    }

    rl_completion_append_character = ' '; // to add space after match
    if (!found)
    {
        complete_param = (void *)"";
        matches = rl_completion_matches(text, const_comp_generator);
    }
    else
    {
        complete_param = possible_chains;
        matches = rl_completion_matches(text, fromchain_comp_generator);
    }

    return matches;
}

#endif /* #ifdef ENABLE_LIBEDIT */
