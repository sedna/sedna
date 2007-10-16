/*
 * File:  exceptions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



/**
The following primary exceptions are defined for Sedna (later referred as 
"the system").

                         SednaException
                         //          \\______________________
                        //            \\_____________________\\
                       //                                     \\
       SednaSystemException                               SednaUserException  ______________________________
               ||                                ________//      ||       \\____________    _______________\\
               ||                              / ________/       ||        \____________\\                  \\
               ||                             //                 ||                      \\                  \\
     SednaSystemEnvException   SednaUserExceptionFnError SednaUserEnvException SednaUserSoftException SednaXQueryException


SednaException -- abstract base exception class. You cannot use it for raising
exceptions

SednaSystemException -- use it when some system error happens. This error means
that the system is completely malfunction. The reaction on this error is hard 
stopping of all components currently running.

SednaSystemEnvException -- the same as SednaSystemException except one point.
The output to a user produced as the reaction to this error says that it was 
environment (operating system) fault and the system can not be longer running.
For the previous exception responsibility is on the developers.

SednaUserException -- use it when some kind of error happens but this error
is not fatal for the system but rather a recoverable error. Most errors of 
this type are errors caused by users (wrong queries they sends). But they can
be other errors such as "can not connect to sm because sm is not running"
caused by trn. The reaction on this error is to produce correct message to the
user with an explanation of the problem. Error codes and descriptions of errors
are defined in error.codes file, which is in the same directory as the file you
are reading now.

SednaXQueryException -- use it within physical plan operations (PP*). It has 
semantic as SednaUserException with better diagnostic. Line of the XQuery query
must be provided with which error is connected. 0 means 'I can't say exact line'.

SednaUserExceptionFnError -- use it for errors raised by users (fn:error function).

SednaUserEnvException -- the same as SednaUserException with a bit different
semantics.  Use this exception to notify the user that the system cannot do
something because of the environment (operating system). You need not define 
error code for this kind of error (it is already defined), but you
rather need to provide a correct description. This exception is needed to
simplify the life of the developers, because it allow them not to define
enormous number of error codes for routine operations such as semaphore
creation.
The example of using this exception is the following:

if (<cannot create semaphore during startup>)
   throw USER_ENV_EXCEPTION("Cannot create semaphore <the name of the semaphore>",
                            false);

SednaUserSoftException -- this exception class rather differs from other 
exceptions. It is used for correct program termination instead of signaling
some error condition. Its purpose can be clarified by the following example.
Suppose you have to parse command line arguments for already started (and 
initialized) process. If you find some error in command line parameters its your
obligatory to finish the process and bring the error message to the user. If you
just simply write error message to console and exit from the process, it may
crash the system because in this case you avoid process deinitialization, which
is performed by the program code somewhere else in the program. From the other
hand, if you raise SednaUserException then you have to supply error code and,
what is more important, the error message that is identified by this error
code will be enlarged with additional information such as error code and a 
comment. The solution to this problem is to use SednaUserSoftException for raising
the error. In this case process deinitialization will be completed as a part
of the standard error handling mechanism and the user will see exactly the same
error message that you have supplied.




For raising exception it is better to use these macroses:

#define SYSTEM_EXCEPTION(msg)						SednaSystemException(__FILE__, __FUNCTION__, __LINE__, msg)
#define SYSTEM_ENV_EXCEPTION(msg)					SednaSystemEnvException(__FILE__, __FUNCTION__, __LINE__, msg)
#define USER_EXCEPTION(code)						SednaUserException(__FILE__, __FUNCTION__, __LINE__, code)
#define USER_EXCEPTION2(code, details)				SednaUserException(__FILE__, __FUNCTION__, __LINE__, details, code)
#define XQUERY_EXCEPTION(code)						SednaUserException(__FILE__, __FUNCTION__, __LINE__, code, __current_physop)
#define XQUERY_EXCEPTION2(code, details)			SednaUserException(__FILE__, __FUNCTION__, __LINE__, details, code, __current_physop)
#define USER_EXCEPTION_FNERROR(err_name, err_descr) SednaUserException(__FILE__, __SE_FUNCTION__, __LINE__, err_name, err_descr))
#define USER_ENV_EXCEPTION(msg, rollback)			SednaUserEnvException(__FILE__, __FUNCTION__, __LINE__, msg, rollback)
#define USER_ENV_EXCEPTION2(msg, expl, rollback)	SednaUserEnvException(__FILE__, __FUNCTION__, __LINE__, msg, expl, rollback)
#define USER_SOFT_EXCEPTION(msg)					SednaUserSoftException(__FILE__, __FUNCTION__, __LINE__, msg)

Their names are straightfoward. Parameters are:
msg                     -- a textual message (some kind of error description)
code                    -- the code for user defined error (use constants defined in 
                           error_codes.h; example is SE1001)
details                 -- details for user error
expl                    -- explanation of error
rollback                -- does the error leads to rollback?
__current_physop        -- defines current physical operation



Errors could be outputted to the user in the format of <sedna-message>:

<sedna-message. ::=
"SEDNA Message: " <problem> <line-break>
<description> <line-break>
[ "Details: " <details> <line-break> ]
[ "Position: [" <file> ":" <function> ":" <line> "]" <line-break> ]

<problem> ::= "ERROR " <code>
            | "FATAL ERROR"

<description> ::= <string-without-without-line-break>

<file> ::= <string-without-without-line-break>

<function> ::= <string-without-without-line-break>

<line> ::= <integer>

<code> ::= <code-from-(error.codes)>

*/



#ifndef __EXCEPTIONS_H
#define __EXCEPTIONS_H

#include <string>
#include "common/utils.h"
#include "common/errdbg/error_codes.h"


#define SYSTEM_EXCEPTION(msg) \
    ((U_ASSERT_MACRO(false)), \
     elog(EL_FATAL, (msg)), \
     SednaSystemException(__FILE__, __SE_FUNCTION__, __LINE__, msg))

#define SYSTEM_ENV_EXCEPTION(msg) \
    (elog(EL_FATAL, (msg)), \
     SednaSystemEnvException(__FILE__, __SE_FUNCTION__, __LINE__, msg))

#define USER_EXCEPTION(internal_code) \
    (elog(EL_ERROR, ("(%s) %s", \
                     user_error_code_entries[internal_code].code, \
                     user_error_code_entries[internal_code].descr)), \
     SednaUserException(__FILE__, __SE_FUNCTION__, __LINE__, internal_code))

#define USER_EXCEPTION2(internal_code, details) \
    (elog(EL_ERROR, ("(%s) %s Details: %s", \
                     user_error_code_entries[internal_code].code, \
                     user_error_code_entries[internal_code].descr, \
                     details)), \
     SednaUserException(__FILE__, __SE_FUNCTION__, __LINE__, details, internal_code))

#define XQUERY_EXCEPTION(internal_code) \
    (elog(EL_ERROR, ("(%s) %s", \
                     user_error_code_entries[internal_code].code, \
                     user_error_code_entries[internal_code].descr)), \
     SednaXQueryException(__FILE__, __SE_FUNCTION__, __LINE__, internal_code, __current_physop))

#define XQUERY_EXCEPTION2(internal_code, details) \
    (elog(EL_ERROR, ("(%s) %s Details: %s", \
                     user_error_code_entries[internal_code].code, \
                     user_error_code_entries[internal_code].descr, \
                     details)), \
     SednaXQueryException(__FILE__, __SE_FUNCTION__, __LINE__, details, internal_code, __current_physop))

#define USER_EXCEPTION_FNERROR(err_name, err_descr) \
    (elog(EL_ERROR, ("(%s) %s", \
                     err_name, \
                     err_descr)), \
     SednaUserExceptionFnError(__FILE__, __SE_FUNCTION__, __LINE__, err_name, err_descr))

#define USER_ENV_EXCEPTION(msg, rollback) \
    (elog(EL_ERROR, ("(%s) %s Details: %s", \
                     user_error_code_entries[0].code, \
                     user_error_code_entries[0].descr, \
                     msg)), \
     SednaUserEnvException(__FILE__, __SE_FUNCTION__, __LINE__, msg, rollback))


#define USER_ENV_EXCEPTION2(msg, expl, rollback) \
    (elog(EL_ERROR, ("(%s) %s Details: %s (%s)", \
                     user_error_code_entries[0].code, \
                     user_error_code_entries[0].descr, \
                     msg, \
                     expl)), \
     SednaUserEnvException(__FILE__, __SE_FUNCTION__, __LINE__, msg, expl, rollback))

#define USER_SOFT_EXCEPTION(msg) \
     SednaUserSoftException(__FILE__, __SE_FUNCTION__, __LINE__, msg)


//////////////////////////////////////////////////////////////////////////////////////////////////////////
/// SednaException
//////////////////////////////////////////////////////////////////////////////////////////////////////////

class SednaException
{
protected:
    std::string file; 
    std::string function;
    int line;
    std::string err_msg;

public:
    SednaException(const char* _file_, 
                   const char* _function_,
                   int _line_,
                   const char* _err_msg_) : file(_file_),
                                            function(_function_),
                                            line(_line_),
                                            err_msg(_err_msg_) {}
    virtual ~SednaException() {}

    virtual std::string getMsg()         const = 0;
    virtual std::string getDescription() const { return err_msg; }
    virtual std::string getFile()        const { return file; }
    virtual std::string getFunction()    const { return function; }
    virtual int         getLine()        const { return line; }

};



//////////////////////////////////////////////////////////////////////////////////////////////////////////
/// SednaSystemException
//////////////////////////////////////////////////////////////////////////////////////////////////////////

class SednaSystemException : public SednaException
{
public:
    SednaSystemException(const char* _file_, 
                         const char* _function_,
                         int _line_,
                         const char* _err_msg_) : SednaException(_file_,
                                                                        _function_,
                                                                        _line_,
                                                                        _err_msg_) {}
    virtual std::string getMsg() const
    {
        std::string res;
        res += "SEDNA Message: FATAL ERROR\n";
        res += "System error. This error means system malfunction.\n";
        res += "Details: " + err_msg + "\n";
#if (EL_DEBUG == 1)
        res += "Position: [" + file + ":" + function + ":" + int2string(line) + "]\n";
#endif
        return res;
    }
};



//////////////////////////////////////////////////////////////////////////////////////////////////////////
/// SednaSystemEnvException
//////////////////////////////////////////////////////////////////////////////////////////////////////////

class SednaSystemEnvException : public SednaSystemException
{
public:
    SednaSystemEnvException(const char* _file_, 
                            const char* _function_,
                            int _line_,
                            const char* _err_msg_) : SednaSystemException(_file_, 
                                                                                 _function_, 
                                                                                 _line_, 
                                                                                 _err_msg_) {}
    virtual std::string getMsg() const
    {
        std::string res;
        res += "SEDNA Message: FATAL ERROR\n";
        res += "Environment error. This error is caused by environment (operating system) and ";
        res += "it means that the system cannot continue execution anymore.\n";
        res += "Details: " + err_msg + "\n";
#if (EL_DEBUG == 1)
        res += "Position: [" + file + ":" + function + ":" + int2string(line) + "]\n";
#endif
        return res;
    }
};


//////////////////////////////////////////////////////////////////////////////////////////////////////////
/// SednaUserException
//////////////////////////////////////////////////////////////////////////////////////////////////////////

class SednaUserException : public SednaException
{
protected:
    int internal_code;

public:
    SednaUserException(const char* _file_, 
                       const char* _function_,
                       int _line_,
                       int _internal_code_) : SednaException(_file_,
                                                             _function_,
                                                             _line_,
                                                             ""), 
                                              internal_code(_internal_code_) {}
    SednaUserException(const char* _file_, 
                       const char* _function_,
                       int _line_,
                       const char* _err_msg_,
                       int _internal_code_) : SednaException(_file_,
                                                             _function_,
                                                             _line_,
                                                             _err_msg_), 
                                              internal_code(_internal_code_) {}
    virtual std::string getMsg() const
    {
        std::string res;
        res += "SEDNA Message: ERROR ";
        res += std::string(user_error_code_entries[internal_code].code) + "\n";
        res += std::string(user_error_code_entries[internal_code].descr) + "\n";
        if (err_msg.length() != 0)
        {
            res += "Details: " + err_msg + "\n";
        }
#if (EL_DEBUG == 1)
        res += "Position: [" + file + ":" + function + ":" + int2string(line) + "]\n";
#endif
        return res;
    }

    virtual int  get_code() const { return internal_code; }
    virtual bool need_rollback() { return user_error_code_entries[internal_code].act == ueca_ROLLBACK_TRN; }
};


//////////////////////////////////////////////////////////////////////////////////////////////////////////
/// SednaUserExceptionFnError
//////////////////////////////////////////////////////////////////////////////////////////////////////////

class SednaUserExceptionFnError : public SednaUserException
{
protected:
    std::string error_name;
    std::string error_descr;

public:
    SednaUserExceptionFnError(const char* _file_, 
                              const char* _function_,
                              int _line_,
                              const char* _error_name_,
                              const char* _error_descr_) : 
                                    SednaUserException(_file_,
                                                       _function_,
                                                       _line_,
                                                       SE9000), 
                                    error_name(_error_name_),
                                    error_descr(_error_descr_ ? _error_descr_ : "") {}

    virtual std::string getMsg() const
    {
        U_ASSERT(error_name.size() != 0);

        std::string res;
        res += "SEDNA Message: ERROR ";
        res += error_name + "\n";
        res += "    " + (error_descr.size() == 0 ? std::string("User defined error") : error_descr) + "\n";
#if (EL_DEBUG == 1)
        res += "Position: [" + file + ":" + function + ":" + int2string(line) + "]\n";
#endif
        return res;
    }

    virtual int  get_code() const { return internal_code; }
    virtual bool need_rollback() { return true; }
};



//////////////////////////////////////////////////////////////////////////////////////////////////////////
/// SednaUserEnvException
//////////////////////////////////////////////////////////////////////////////////////////////////////////

class SednaUserEnvException : public SednaUserException
{
protected:
    bool rollback;
    std::string explanation;
public:
    SednaUserEnvException(const char* _file_, 
                          const char* _function_,
                          int _line_,
                          const char* _err_msg_,
                          bool _rollback_) : SednaUserException(_file_,
                                                                _function_,
                                                                _line_,
                                                                _err_msg_,
                                                                0),
                                             rollback(_rollback_) {}
    SednaUserEnvException(const char* _file_, 
                          const char* _function_,
                          int _line_,
                          const char* _err_msg_,
                          const char* _explanation_,
                          bool _rollback_) : SednaUserException(_file_,
                                                                _function_,
                                                                _line_,
                                                                _err_msg_,
                                                                0),
                                             rollback(_rollback_),
					     explanation(_explanation_){}
    virtual std::string getDescription() const 
    { 
        return err_msg + 
               (explanation.length() != 0 ? " (" + explanation + ")"
                                          : ""); 
    }

    virtual std::string getMsg() const
    {
        std::string res;
        res += "SEDNA Message: ERROR ";
        res += std::string(user_error_code_entries[internal_code].code) + "\n";
        res += std::string(user_error_code_entries[internal_code].descr) + "\n";
        res += "Details: " + err_msg;
        if (explanation.length() != 0)
        {
            res += " (" + explanation + ")";
        }
        res += "\n";
#if (EL_DEBUG == 1)
        res += "Position: [" + file + ":" + function + ":" + int2string(line) + "]\n";
#endif
        return res;
    }

    virtual bool need_rollback() { return rollback; }
};



//////////////////////////////////////////////////////////////////////////////////////////////////////////
/// SednaUserSoftException
//////////////////////////////////////////////////////////////////////////////////////////////////////////

class SednaUserSoftException : public SednaUserException
{
public:
    SednaUserSoftException(const char* _file_, 
                           const char* _function_,
                           int _line_,
                           const char* _err_msg_) : SednaUserException(_file_,
                                                                       _function_,
                                                                       _line_,
                                                                       _err_msg_,
                                                                       -1) {}
    virtual std::string getMsg() const { return err_msg; }

    virtual bool need_rollback() { return false; }
};



//////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Sedna soft fault fuctions.
/// 'sedna_soft_fault' fuction with no message parameter is defined in event_loc.h
//////////////////////////////////////////////////////////////////////////////////////////////////////////

void sedna_soft_fault(const SednaException &e, int component);
void sedna_soft_fault(const char* s, int  component);



#endif
