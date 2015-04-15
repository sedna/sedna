/*
 * File:  exceptions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */
#ifndef _SEDNA_EXCEPTIONS_H
#define _SEDNA_EXCEPTIONS_H

#include <exception>
#include <string>

/*
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


#include "common/errdbg/error_codes.h"
#include "common/errdbg/event_log.h"

#define EXCEPTION_PARAMETERS __FILE__, __SE_FUNCTION__, __LINE__

/*	Never use catch(...), use catch(ANY_SE_EXCEPTION) instead. Catch(...) 
	statement is missinterpreted by cl version 13 and earlier ones. It catches
	not just any C++ object coming from 'throw' statement (as per C++ standard)
	but also access violations and other exceptional conditions reported by OS.
	For catch(ANY_SE_EXCEPTION) to work properly ensure that all exceptions
	in the project are descendants of std::exception class. */ 
#define ANY_SE_EXCEPTION	std::exception &

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
     SednaUserException(__FILE__, __SE_FUNCTION__, __LINE__, "", internal_code))

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
     SednaXQueryException(__FILE__, __SE_FUNCTION__, __LINE__, "", internal_code, executor_globals::__current_physop))

#define XQUERY_EXCEPTION2(internal_code, details) \
    (elog(EL_ERROR, ("(%s) %s Details: %s", \
                     user_error_code_entries[internal_code].code, \
                     user_error_code_entries[internal_code].descr, \
                     details)), \
     SednaXQueryException(__FILE__, __SE_FUNCTION__, __LINE__, details, internal_code, executor_globals::__current_physop))

#define USER_EXCEPTION_FNERROR(err_name, err_descr) \
    (elog(EL_ERROR, ("(%s) %s", \
                     err_name, \
                     err_descr)), \
     SednaUserExceptionFnError(__FILE__, __SE_FUNCTION__, __LINE__, "Sedna user error", err_name, err_descr))

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

//////////////////////////////////////////////////////////////////////////////
/// SednaException
//////////////////////////////////////////////////////////////////////////////

#define EXCEPTION_PARAMETERS_DECL const char* _file_, const char* _function_, int _line_, const char* _err_msg_
#define SEDNA_EXCEPTION_INHERIT _file_, _function_, _line_, _err_msg_
#define EXCEPTION_BUFFER_SIZE SE_SOFT_FAULT_LOG_CONTENT_LEN

class SednaException : public std::exception
{
protected:
    const char * file;
    const char * function;
    int line;

    const char * err_msg;
    const char * global_description;

     static char error_buffer[EXCEPTION_BUFFER_SIZE];

    virtual const char * createMessage(char * buffer) const;
public:
    SednaException(EXCEPTION_PARAMETERS_DECL, const char * _global_description)
      : file(_file_), function(_function_), line(_line_), err_msg(_err_msg_), global_description(_global_description) {}

    const char * getMsg (char * buffer) const { return createMessage(buffer); };

    const char * getDescription() const { return err_msg; }
    const char * getFile()        const { return file; }
    const char * getFunction()    const { return function; }
    int          getLine()        const { return line; }

    /*  The standard method to obtain exception info defined by
          all std::exception descendants. */
    virtual const char * what() const throw() { return getMsg(error_buffer); }
};

//////////////////////////////////////////////////////////////////////////////
/// SednaSystemException
//////////////////////////////////////////////////////////////////////////////

class SednaSystemException : public SednaException
{
    static const char * description;
protected:
    SednaSystemException(EXCEPTION_PARAMETERS_DECL, const char * _global_description)
      : SednaException(SEDNA_EXCEPTION_INHERIT, _global_description) {}
public:
    SednaSystemException(EXCEPTION_PARAMETERS_DECL)
      : SednaException(SEDNA_EXCEPTION_INHERIT, description) {}
};

//////////////////////////////////////////////////////////////////////////////
/// SednaSystemEnvException
//////////////////////////////////////////////////////////////////////////////

class SednaSystemEnvException : public SednaSystemException
{
    static const char * description;
public:
    SednaSystemEnvException(EXCEPTION_PARAMETERS_DECL)
        : SednaSystemException(SEDNA_EXCEPTION_INHERIT, description) {}
};

//////////////////////////////////////////////////////////////////////////////
/// SednaUserException
//////////////////////////////////////////////////////////////////////////////

class SednaUserException : public SednaException
{
protected:
    int internal_code;

    const char * error_name;
    const char * error_descr;

    virtual const char * createMessage(char* buffer) const;

    SednaUserException(EXCEPTION_PARAMETERS_DECL, const char* _error_name_, const char* _error_descr_)
      : SednaException(SEDNA_EXCEPTION_INHERIT, "ERROR"), internal_code(-1),
          error_name(_error_name_), error_descr(_error_descr_) {}
public:
    SednaUserException(EXCEPTION_PARAMETERS_DECL, int _internal_code_)
      : SednaException(SEDNA_EXCEPTION_INHERIT, "ERROR"), internal_code(_internal_code_),
        error_name(""), error_descr("")
    {
        error_name = user_error_code_entries[internal_code].code;
        error_descr = user_error_code_entries[internal_code].descr;
    }

    int getCode() const { return internal_code; }

    virtual bool needRollback() const { return user_error_code_entries[internal_code].act == ueca_ROLLBACK_TRN; }
};

//////////////////////////////////////////////////////////////////////////////
/// SednaUserExceptionFnError
//////////////////////////////////////////////////////////////////////////////

class SednaUserExceptionFnError : public SednaUserException
{
public:
    SednaUserExceptionFnError(EXCEPTION_PARAMETERS_DECL, const char* _error_name_, const char* _error_descr_)
      : SednaUserException(SEDNA_EXCEPTION_INHERIT, _error_name_, _error_descr_ ? _error_descr_ : "User defined error") {}
};

//////////////////////////////////////////////////////////////////////////////
/// SednaUserEnvException
//////////////////////////////////////////////////////////////////////////////

class SednaUserEnvException : public SednaUserException
{
protected:
    bool rollback;
    const char * explanation;

    virtual const char * createMessage(char* buffer) const;
public:
    SednaUserEnvException(EXCEPTION_PARAMETERS_DECL, bool _rollback_)
        : SednaUserException(SEDNA_EXCEPTION_INHERIT, 0), rollback(_rollback_), explanation("") {}

    SednaUserEnvException(EXCEPTION_PARAMETERS_DECL, const char* _explanation_, bool _rollback_)
        : SednaUserException(SEDNA_EXCEPTION_INHERIT, 0), rollback(_rollback_), explanation(_explanation_) {}

    const char * getDescription(char * buffer) const;
    virtual bool needRollback() { return rollback; }
};

//////////////////////////////////////////////////////////////////////////////
/// SednaUserSoftException
//////////////////////////////////////////////////////////////////////////////

class SednaUserSoftException : public SednaUserException
{
public:
    SednaUserSoftException(EXCEPTION_PARAMETERS_DECL)
        : SednaUserException(SEDNA_EXCEPTION_INHERIT, "Sedna soft fault", "") {}

    virtual bool needRollback() { return false; }
};

//////////////////////////////////////////////////////////////////////////////
/// SednaXQueryException - in PPBase.h
//////////////////////////////////////////////////////////////////////////////

class SednaXQueryException;

//////////////////////////////////////////////////////////////////////////////
/// Sedna soft fault fuctions.
/// 'sedna_soft_fault' fuction with no message parameter is defined in event_loc.h
//////////////////////////////////////////////////////////////////////////////

void sedna_soft_fault(const SednaException &e, int component);
void sedna_soft_fault(const char* s, int  component);


//////////////////////////////////////////////////////////////////////////////
/// Under Darwin we need this hack to compile Sedna with gcc 4.0.1
//////////////////////////////////////////////////////////////////////////////

#if defined(DARWIN)

#ifdef    SYSTEM_EXCEPTION
#undef    SYSTEM_EXCEPTION
#define   SYSTEM_EXCEPTION(msg) __system_exception(__FILE__, __SE_FUNCTION__, __LINE__, msg)
#endif /* SYSTEM_EXCEPTION */

#ifdef    SYSTEM_ENV_EXCEPTION
#undef    SYSTEM_ENV_EXCEPTION
#define   SYSTEM_ENV_EXCEPTION(msg) __system_env_exception(__FILE__, __SE_FUNCTION__, __LINE__, msg)
#endif /* SYSTEM_ENV_EXCEPTION */

#ifdef    USER_EXCEPTION2
#undef    USER_EXCEPTION2
#define   USER_EXCEPTION2(internal_code, details) __user_exception2(__FILE__, __SE_FUNCTION__, __LINE__, internal_code, details)
#endif /* USER_EXCEPTION2 */

#ifdef    XQUERY_EXCEPTION2
#undef    XQUERY_EXCEPTION2
#define   XQUERY_EXCEPTION2(internal_code, details) __xquery_exception2(__FILE__, __SE_FUNCTION__, __LINE__, internal_code, details)
#endif /* XQUERY_EXCEPTION2 */

#ifdef    USER_EXCEPTION_FNERROR
#undef    USER_EXCEPTION_FNERROR
#define   USER_EXCEPTION_FNERROR(err_name, err_descr) __user_exception_fnerror(__FILE__, __SE_FUNCTION__, __LINE__, err_msg, err_name, err_descr)
#endif /* USER_EXCEPTION_FNERROR */

#ifdef    USER_ENV_EXCEPTION
#undef    USER_ENV_EXCEPTION
#define   USER_ENV_EXCEPTION(msg, rollback) __user_env_exception(__FILE__, __SE_FUNCTION__, __LINE__, msg, rollback)
#endif /* USER_ENV_EXCEPTION */

#ifdef    USER_ENV_EXCEPTION2
#undef    USER_ENV_EXCEPTION2
#define   USER_ENV_EXCEPTION2(msg, expl, rollback) __user_env_exception2(__FILE__, __SE_FUNCTION__, __LINE__, msg, expl, rollback)
#endif /* USER_ENV_EXCEPTION2 */

#ifdef    USER_SOFT_EXCEPTION
#undef    USER_SOFT_EXCEPTION
#define   USER_SOFT_EXCEPTION(msg) __user_soft_exception(__FILE__, __SE_FUNCTION__, __LINE__, msg)
#endif /* USER_SOFT_EXCEPTION */


inline SednaSystemException __system_exception(const char *file, const char *func, int line, const char *msg) {
    return ((U_ASSERT_MACRO(false)), 
             elog(EL_FATAL, (msg)), 
             SednaSystemException(file, func, line, msg)); }

inline SednaSystemEnvException __system_env_exception(const char *file, const char *func, int line, const char *msg) {
    return (elog(EL_FATAL, (msg)), 
            SednaSystemEnvException(file, func, line, msg)); }

inline SednaUserException __user_exception2(const char *file, const char *func, int line, int code, const char * details) {
    return (elog(EL_ERROR, ("(%s) %s Details: %s", 
                            user_error_code_entries[code].code, 
                            user_error_code_entries[code].descr,
                            details)), 
            SednaUserException(file, func, line, details, code)); }

inline SednaUserExceptionFnError __user_exception_fnerror(const char *file, const char *func, int line, const char *err_msg, const char *err_name, const char* err_descr) {
    return (elog(EL_ERROR, ("(%s) %s", 
                            err_name, 
                            err_descr)), 
            SednaUserExceptionFnError(file, func, line, err_msg, err_name, err_descr)); }

inline SednaUserEnvException __user_env_exception(const char *file, const char *func, int line, const char *msg, bool rollback) {
    return (elog(EL_ERROR, ("(%s) %s Details: %s", 
                            user_error_code_entries[0].code, 
                            user_error_code_entries[0].descr,
                            msg)), 
            SednaUserEnvException(file, func, line, msg, rollback)); }

inline SednaUserEnvException __user_env_exception2(const char *file, const char *func, int line, const char *msg, const char* expl, bool rollback) {
    return (elog(EL_ERROR, ("(%s) %s Details: %s (%s)", 
                            user_error_code_entries[0].code, 
                            user_error_code_entries[0].descr,
                            msg,
                            expl)), 
            SednaUserEnvException(file, func, line, msg, expl, rollback)); }

inline SednaUserSoftException __user_soft_exception(const char *file, const char *func, int line, const char *msg) {
    return SednaUserSoftException(file, func, line, msg); }


#endif /* DARWIN */

//////////////////////////////////////////////////////////////////////////////


#endif /* _SEDNA_EXCEPTIONS_H */
