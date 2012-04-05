/*
 * File:  exceptions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "errors.h"
#include "exceptions.h"
#include "event_log.h"

#include "u/uprocess.h"
#include "aux/cppcast.h"

/*	SednaException is derived from std::exception, hence
	it must implement the following method:
	
	const char *what() const throw();

	SednaException defines virtual function to obtain
	error description as std::string, namely getMsg2().
	The string must not vanish when what() function
	returns. We have descriptCache member for this purpose.
*/ 
const std::string &SednaException::getMsg() const
{
	if (descriptCache.empty()) 
	{
		std::string descript = getMsg2();
		descriptCache.swap(descript);
	}
	return descriptCache;
}

std::string SednaSystemException::getMsg2() const
{
    std::string res;
	res.reserve(1024);
    res += "SEDNA Message: FATAL ERROR\n";
    res += "System error. This error means system malfunction.\n";
    res += "Details: " + err_msg + "\n";
#if (EL_DEBUG == 1)
    res += "Position: [" + file + ":" + function + ":" + cast_to_string(line) + "]\n";
#endif
    return res;
}
	
std::string SednaSystemEnvException::getMsg2() const
{
    std::string res;
	res.reserve(1024);
    res += "SEDNA Message: FATAL ERROR\n";
    res += "Environment error. This error is caused by environment (operating system) and ";
    res += "it means that the system cannot continue execution anymore.\n";
    res += "Details: " + err_msg + "\n";
#if (EL_DEBUG == 1)
    res += "Position: [" + file + ":" + function + ":" + cast_to_string(line) + "]\n";
#endif
    return res;
}

std::string SednaUserException::getMsg2() const
{
    std::string res;
	res.reserve(1024);
    res += "SEDNA Message: ERROR ";
    res += std::string(user_error_code_entries[internal_code].code) + "\n";
    res += std::string(user_error_code_entries[internal_code].descr) + "\n";
    if (err_msg.length() != 0)
    {
        res += "Details: " + err_msg + "\n";
    }
#if (EL_DEBUG == 1)
    res += "Position: [" + file + ":" + function + ":" + cast_to_string(line) + "]\n";
#endif
    return res;
}

std::string SednaUserExceptionFnError::getMsg2() const
{
    U_ASSERT(error_name.size() != 0);

    std::string res;
	res.reserve(1024);
    res += "SEDNA Message: ERROR ";
    res += error_name + "\n";
    res += "    " + (error_descr.size() == 0 ? std::string("User defined error") : error_descr) + "\n";
#if (EL_DEBUG == 1)
    res += "Position: [" + file + ":" + function + ":" + cast_to_string(line) + "]\n";
#endif
    return res;
}

std::string SednaUserEnvException::getDescription() const 
{ 
    return err_msg + 
            (explanation.length() != 0 ? " (" + explanation + ")"
                                        : ""); 
}

std::string SednaUserEnvException::getMsg2() const
{
    std::string res;
	res.reserve(1024);
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
    res += "Position: [" + file + ":" + function + ":" + cast_to_string(line) + "]\n";
#endif
    return res;
}


void sedna_soft_fault(const SednaException &e,  int component)
{
    SEDNA_SOFT_FAULT_BASE_MSG;

    char log_message[SE_SOFT_FAULT_LOG_CONTENT_LEN];
    size_t log_message_len = e.getDescription().length();

    if (log_message_len != 0)
        fprintf(stderr, "Details: %s\n", e.getDescription().c_str());
    if(log_message_len < SE_SOFT_FAULT_LOG_CONTENT_LEN)
        strcpy(log_message, e.getDescription().c_str());
    else
        strcpy(log_message, "Failed to record exception description into the log\n");
#if (EL_DEBUG == 1)
    fprintf(stderr, "Position: [%s:%s:%d]\n", e.getFile().c_str(), e.getFunction().c_str(), e.getLine());
    sprintf(log_message+(log_message_len),"\nPosition: [%s:%s:%d]\n", e.getFile().c_str(), e.getFunction().c_str(), e.getLine());
#endif
    sedna_soft_fault_log(log_message, component);
#ifdef SE_MEMORY_TRACK
    DumpUnfreed(component);
#endif

    SEDNA_SOFT_FAULT_FINALIZER;
}

void sedna_soft_fault(const char* s, int  component)
{
    SEDNA_SOFT_FAULT_BASE_MSG;
    fprintf(stderr, "Details: %s\n", s);
    sedna_soft_fault_log(s, component);

#ifdef SE_MEMORY_TRACK
	DumpUnfreed(component);
#endif

    SEDNA_SOFT_FAULT_FINALIZER;
}

