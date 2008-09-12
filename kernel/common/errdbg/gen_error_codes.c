#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>


#define IS_WHITESPACE(byte)              ((byte)==0 || (byte)=='\n' || (byte)==' ' || (byte)=='\t' || (byte)=='\r')
#define COMMENT_BEGIN                    '#'
#define ERROR_CODE_DELIMITER             ":"
#define ERROR_CODE_MAX_SIZE              10
#define ERROR_DESCRIPTION_MAX_SIZE       1000

/// File names
#define ERRORS_FILE_NAME                 "error.codes"
#define C_ERROR_CODES                    "error_codes.c"
#define H_ERROR_CODES                    "error_codes.h"
#define JAVA_ERROR_CODES                 "ErrorCodes.java"
#define SCM_ERROR_CODES                  "error_codes_scm.scm"

struct error_code
{
    char* code;
    char* description;
};


/// Cuts off everyting after the first whitespace
/// Length of code is returned
int prepare_error_code(char* dest, const char* src)
{
    int i = 0;
    while(!IS_WHITESPACE(src[i])) { dest[i] = src[i]; i++; }
    dest[i] = '\0';
    return i;
}

/// Cuts off any leading whitespaces and takes string before newline or it's end.
/// Length of description is returned
int prepare_error_description(char* dest, const char* src)
{
    int i = 0;
    int j = 0;
    while(src[i] == ' ' || src[i] == '\t') { i++; }
    while(src[i] != '\r' && src[i] != '\n' && src[i] != 0) { dest[j] = src[i]; i++; j++; }
    dest[j] = '\0';
    return j;
}


void write_with_handler(FILE* file, const char *file_name, const char *s, ...)
{
    va_list ap;
    int res = 0;

    va_start(ap, s);
    res = vfprintf(file, s, ap);
    va_end(ap);

    if(res < 0)
    {
        fprintf(stderr, "Can't write error codes: '%s'!\n", file_name);
        exit(1);
    }
}


void write_quoted_string(FILE* file, const char *file_name, const char *s)
{
    while(*s)
    {
        switch(*s)
        {
            case '\\':
            case '\"':
                if(fputc('\\', file) == EOF)
                {
                    fprintf(stderr, "Can't write error codes: '%s'!\n", file_name);
                    exit(1);
                }
            default:
                if(fputc(*s, file) == EOF)
                {
                    fprintf(stderr, "Can't write error codes: '%s'!\n", file_name);
                    exit(1);
                }
        }
        s++;
    }
}



void write_c_error_codes(const struct error_code* codes, const int codes_len)
{
    FILE* cerrors = NULL;
    FILE* herrors = NULL;

    int counter = 0;

    char* c_header = "\n//This file was generated. Do not edit it!!!\n\n\
#include \"common/errdbg/error_codes.h\"\n\n\
struct user_error_code_entry user_error_code_entries[] = {\n";
    char* h_header = "#ifndef _USER_ERROR_CODES_H\n\
#define _USER_ERROR_CODES_H\n\n\
//This file was generated. Do not edit it!!!\n\n\
#ifdef __cplusplus\n\
extern \"C\" {\n\
#endif\n\n\
enum user_error_code_act {ueca_NOTHING, ueca_ROLLBACK_TRN};\n\n\
struct user_error_code_entry\n\
{\n\
    const char* code;               /* error code */\n\
    enum user_error_code_act act;   /* reaction on error */\n\
    const char* descr;              /* error decrtiption */\n\
};\n\n\
extern struct user_error_code_entry user_error_code_entries[];\n\
extern const int user_error_code_entries_size;\n\n\
#ifdef __cplusplus\n\
}\n\
#endif\n\n";

    char* c_tail = "};\n\nconst int user_error_code_entries_size = sizeof user_error_code_entries;\n";
    char* h_tail = "\n#endif\n";    


    if((cerrors = fopen(C_ERROR_CODES, "wb")) == NULL)
    {
        fprintf(stderr, "Can't open file to write C error codes: '%s'!\n", C_ERROR_CODES);
        exit(1);
    }
    if((herrors = fopen(H_ERROR_CODES, "wb")) == NULL)
    {
        fprintf(stderr, "Can't open file to write C error codes: '%s'!\n", H_ERROR_CODES);
        exit(1);
    }

    write_with_handler(cerrors, C_ERROR_CODES, c_header);    
    write_with_handler(herrors, H_ERROR_CODES, h_header);

    for(counter = 0; counter < codes_len; counter++)
    {
        write_with_handler(herrors, H_ERROR_CODES, "#define %s           %d  // %s\n", codes[counter].code, counter, codes[counter].description);
 
        write_with_handler  (cerrors, C_ERROR_CODES, "%c{\"%s\", ueca_ROLLBACK_TRN, \"", counter == 0 ? ' ' : ',', codes[counter].code);
        write_quoted_string (cerrors, C_ERROR_CODES, codes[counter].description);
        write_with_handler  (cerrors, C_ERROR_CODES, "\"}\n");
    }

    write_with_handler(cerrors, C_ERROR_CODES, c_tail);    
    write_with_handler(herrors, H_ERROR_CODES, h_tail);


    fclose(cerrors);
    fclose(herrors);
}



void write_scm_error_codes(const struct error_code* codes, const int codes_len)
{
    FILE* errors = NULL;

    int counter = 0;

    char* header = "\n;; This file was generated. Do not edit it!!!\n\n\
(declare (unit scm-error-codes))\n\n";


    if((errors = fopen(SCM_ERROR_CODES, "wb")) == NULL)
    {
        fprintf(stderr, "Can't open file to write Scheme error codes: '%s'!\n", SCM_ERROR_CODES);
        exit(1);
    }

    write_with_handler(errors, SCM_ERROR_CODES, header);    

    for(counter = 0; counter < codes_len; counter++)
    {
        write_with_handler(errors, SCM_ERROR_CODES, "(define %s %d)  ;; %s\n", codes[counter].code, counter, codes[counter].description);
    }

    fclose(errors);
}

void write_java_error_codes(const struct error_code* codes, const int codes_len)
{
    FILE* errors = NULL;

    int counter = 0;


    char* header = "\n// This file was generated. Do not edit it!!!\n\n\
package ru.ispras.sedna.driver;\n\
import java.io.*;\n\
import java.lang.*;\n\n\
class ErrorCodes {\n";
    char* middle = "\n    static String [][] user_error_code_entry = {\n";
    char* tail = "    };\n}\n";


    if((errors = fopen(JAVA_ERROR_CODES, "wb")) == NULL)
    {
        fprintf(stderr, "Can't open file to write Java error codes: '%s'!\n", JAVA_ERROR_CODES);
        exit(1);
    }

    write_with_handler(errors, JAVA_ERROR_CODES, header);

    for(counter = 0; counter < codes_len; counter++)
    {
        write_with_handler(errors, JAVA_ERROR_CODES, "    final static int %s = %d;  // %s\n", codes[counter].code, counter, codes[counter].description);
    }

    write_with_handler(errors, JAVA_ERROR_CODES, middle);

    for(counter = 0; counter < codes_len; counter++)
    {
        write_with_handler  (errors, JAVA_ERROR_CODES, "        %c{ \"%s\", \"", counter == 0 ? ' ' : ',', codes[counter].code); 
        write_quoted_string (errors, JAVA_ERROR_CODES, codes[counter].description);
        write_with_handler  (errors, JAVA_ERROR_CODES, "\"}\n");
    }

    write_with_handler(errors, JAVA_ERROR_CODES, tail);

    fclose(errors);
}




int main(int argc, char** argv)
{
    FILE* ferrors = NULL;
    char buf[1024];
    int line = 0;
    struct error_code* codes = NULL;
    int codes_size = 0, counter = 0, code_len = 0, description_len = 0; 


    if((ferrors = fopen(ERRORS_FILE_NAME, "r")) == NULL)
    {
        fprintf(stderr, "Can't open file with error code: '%s'!\n", ERRORS_FILE_NAME);
        return 1;
    }

    while(1)
    {
        char *code = NULL, *new_code = NULL, *new_description = NULL;

        if(fgets(buf, sizeof(buf), ferrors) == NULL)
        { 
            if(feof(ferrors)) break;
            else return 1;
        }
        line++;
        if(IS_WHITESPACE(buf[0]) || buf[0] == COMMENT_BEGIN) continue;

        /// I. Prepare error code
        if((code = strstr(buf, ERROR_CODE_DELIMITER)) == NULL)
        {
            fprintf(stderr, "Error code must be delimited with '%s', line %d\n", ERROR_CODE_DELIMITER, line);
            return 1;
        }

        if(IS_WHITESPACE(code[1]))
        {
            fprintf(stderr, "Error code can't have leading whitespace or be empty, line %d\n", line);
            return 1;
        }
        
        code_len = strlen(code + 1);
        if(code_len > ERROR_CODE_MAX_SIZE) 
        { 
            fprintf(stderr, "Too long error code, line %d\n", line);
            return 1;
        }

        new_code = (char*)malloc(code_len + 1);
        prepare_error_code(new_code, code + 1);

        /// II. Prepare error description
        if(fgets(buf, sizeof(buf), ferrors) == NULL || fgets(buf, sizeof(buf), ferrors) == NULL)
        { 
            fprintf(stderr, "Two lines are expected after error code, line %d\n", line);
            return 1;
        }
        line+=2;
        
        description_len = strlen(buf);
        if(description_len > ERROR_DESCRIPTION_MAX_SIZE) 
        { 
            fprintf(stderr, "Too long error code description, line %d\n", line);
            return 1;
        }
        
        new_description = (char*)malloc(description_len + 1);
        if(prepare_error_description(new_description, buf) == 0)
        { 
            fprintf(stderr, "Empty error description, line %d\n", line);
            return 1;
        }


        counter++;

        /// III. Allocate new error code wrapper
        if(codes_size < counter)
        {
            if(!codes_size) 
            {
                codes = (struct error_code*)malloc(sizeof(struct error_code) * 100);
                codes_size = 100;
            }
            else 
            {
                codes = (struct error_code*)realloc(codes, sizeof(struct error_code) * codes_size * 2);
                codes_size *= 2;
            }
        }

        codes[counter-1].code        = new_code;
        codes[counter-1].description = new_description;
    }

    write_c_error_codes(codes, counter); 
    write_scm_error_codes(codes, counter); 
    write_java_error_codes(codes, counter);    

    while(counter > 0)
    {
        //printf("[%s:%s]\n", codes[counter-1].code, codes[counter-1].description);
        free(codes[counter-1].code);
        codes[counter-1].code = NULL;
        free(codes[counter-1].description);
        codes[counter-1].description = NULL;
        counter--;
    }

    free(codes);
    codes = NULL;

    fclose(ferrors);
    return 0;
}

