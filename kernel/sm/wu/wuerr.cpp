#define __WUDANG_SOURCES__

#include <stddef.h>
#include <string.h>
#include <assert.h>
#ifdef _WIN32
#include <windows.h>
#endif
#include <errno.h>
#include <stdarg.h>
#include "wuerr.h"
#include "wudock.h"

#define ERR_BUF_SZ	2048

static const size_t appErrorsNum = 56;
static const char *appErrorsDescription[appErrorsNum] =
{
	"Some error occured.",
	"Bad params passed to the function.",
	"The called function must be never called in this state.",
	"The module failed to perform a clean startup.",
	"The module failed to perform a clean shutdown.",
	"The data read from persistent storage is corrupt.",
	"The called function is intended for debug purposes only and is unavailable in this build.",
	"Unable to allocate memory.",
	"Function not implemented, sorry.",
	"Unknown exception catched.",
	"An instance of SednaException catched.",
	"An instance of SednaSystemExcepton catched.",
	"An instance of SednaSystemEnvException catched.",
	"An instance of SednaUserException catched.",
	"An instance of SednaUserExceptionFnError catched.",
	"An instance of SednaUserEnvException catched.",
	"An instance of SednaUserSoftException catched.",
	"The ticket is invalid.",
	"An attempt was made to reference a state table row by invalid id.",
	"State table is full, unable to allocate row.",
	"Maximum number of state table columns exceeded.",
	"Maximum size of a state table row exceeded.",
	"Maximum number of state table columns with debug info exceeded.",
	"Bad client id.",
	"Unable to assign the given id to the client since this id is already in use.",
	"Maximum number of registered clients exceeded.",
	"The client is already marked ready.",
	"The client is already marked leaving.",
	"Client set is already unlocked.",
	"The lock count of client set exceeded an implementation limit.",
	"The calling thread locked client set and is unable to mark client ready or leaving.",
	"Unable to unregister the client selected as the current client.",
	"Unable to unregister the client marked ready.",
	"An attempt was made to discard a snapshot that is currently in use.",
	"An attempt was made to discard a persistent snapshot or other special snapshot.",
	"Snapshot is damaged and can not be used.",
	"The snapshot is currently in use and can not be damaged.",
	"The snapshot is already persistent.",
	"Maximum number of snapshots exceeded.",
	"No snapshot with the given timestamp.",
	"No snapshot with the given type.",
	"No snapshot with the given ordinal number.",
	"Unable to create a new snapshot with the given timestamp since another snapshot with this timestamp already exists.",
	"Unable to advance snapshots.",
	"Currently no snapshots exist.",
	"The mutating operation is not permited since snapshots are only availible for read-only access.",
	"Timestamp is invalid.",
	"Maximum timestamp value exceeded. Either 200 years passed or someone is consuming too many timestamps.",
	"No apropriate version of the block exist.",
	"The transaction already created a working version of this block.",
	"An other active transaction created a working version of this block.",
	"The operation requires working version (currently operating on the last commited version).",
	"Unable to create versions of the temporary block.",
	"Operation was not performed due to versioning support disabled in this mode.",
	"The requested block wasn't found in buffers.",
	"Unexpected value of trid_wr_access field in block header."
};

struct ErrorProperties
{
	int error;
	const char *file;
	int line;
	const char *function;
	const char *description;
	char charBuf[ERR_BUF_SZ];
	int code;
};

/*
#ifdef _WIN32
__declspec(thread)
#else
__thread
#endif
*/
ErrorProperties errorProperties =
{
	0, NULL, -1, NULL, NULL, "", 0
};

int WuIsAppError(int error)
{
	return error>=WUERR_FIRST_ERR && error<(int)(WUERR_FIRST_ERR+appErrorsNum);
}

void WuSetLastError(int error)
{
	WuSetLastError2(NULL,-1,NULL,error);
}

void WuSetLastError2(const char *file, int line, const char *function, int error)
{
#ifdef _WIN32
	SetLastError((DWORD)error);
#else
	errno = error;
#endif
	errorProperties.error = error;
	errorProperties.file = file;
	errorProperties.line = line;
	errorProperties.function = function;
	errorProperties.description = WuIsAppError(error) ? appErrorsDescription[error-WUERR_FIRST_ERR] : NULL;
	errorProperties.code = 0;
}

int WuGetLastError()
{
#ifdef _WIN32
	return (int) GetLastError();
#else
	return errno;
#endif
}

void WuGetLastErrorProperties(WuErrorProperties *wuErrorProperties)
{
	int error = WuGetLastError();
	assert(wuErrorProperties);
	memset(wuErrorProperties, 0, sizeof *wuErrorProperties);
	wuErrorProperties->error = error;
	if (errorProperties.error == error)
	{
		wuErrorProperties->description = errorProperties.description;
		wuErrorProperties->file = errorProperties.file;
		wuErrorProperties->line = errorProperties.line;
		wuErrorProperties->function = errorProperties.function;
	}
}

static
void PackStrings(char *buf, size_t sz, ...)
{
	char *ebuf=buf+sz; size_t len=0;
	const char *input=NULL, **output=NULL;
	va_list marker;

	assert(buf && sz>0);
	va_start(marker,sz);

	while((input=va_arg(marker,const char*)))
	{
		output=va_arg(marker,const char**);
		assert(output);
		len = strlen(input);
		if (len>(size_t)((ebuf-buf)-1)) len=ebuf-buf-1;
		memcpy(buf,input,len);
		buf[len]='\0';
		*output=buf;
		buf+=len;
		if (buf+1<ebuf) ++buf;
	}

	va_end(marker);
}

#define CHECK_EXCEPTION(ETYPE, EVAR, EIN) const ETYPE * EVAR = dynamic_cast<const ETYPE * >(&(EIN))

void WuSetLastExceptionObject(const SednaException &e)
{
	int error = WUERR_SEDNA_EXCEPTION, code=0, line=e.getLine();
	std::string description(e.getDescription());
	std::string file(e.getFile());
	std::string function(e.getFunction());
	const char *cDescription=NULL, *cFile=NULL, *cFunction=NULL;

	PackStrings(errorProperties.charBuf,ERR_BUF_SZ,
				description.c_str(),&cDescription,
				file.c_str(),&cFile,
				function.c_str(),&cFunction,
				NULL);

    if (CHECK_EXCEPTION(SednaSystemEnvException, ex, e)) {
        error = WUERR_SEDNA_SYSTEM_ENV_EXCEPTION;
    } else if (CHECK_EXCEPTION(SednaSystemException, ex, e)) {
        error = WUERR_SEDNA_SYSTEM_EXCEPTION;
    } else if (CHECK_EXCEPTION(SednaUserEnvException, ex, e)) {
        error = WUERR_SEDNA_USER_ENV_EXCEPTION;
        code = ex->getCode();
    } else if (CHECK_EXCEPTION(SednaUserSoftException, ex, e)) {
        error = WUERR_SEDNA_USER_SOFT_EXCEPTION;
        code = ex->getCode();
    } else if (CHECK_EXCEPTION(SednaUserException, ex, e)) {
        error = WUERR_SEDNA_USER_EXCEPTION;
        code = ex->getCode();
    } else {
        error = WUERR_SEDNA_EXCEPTION;
    };

	WuSetLastError2(cFile,line,cFunction,error);
	errorProperties.description = cDescription;
	errorProperties.code = code;
}

void WuThrowException()
{
	const char *file="", *function="", *description="";
	int error = WuGetLastError(), line=errorProperties.line, code=errorProperties.code;
	if (WuIsAppError(error) && errorProperties.error!=error)
	{
		/*	some one set last error directly via SetLast error and errorProperties
			did not get updated */
		assert(false);
	}
	if (errorProperties.file) file=errorProperties.file;
	if (errorProperties.function) function=errorProperties.function;
	if (errorProperties.description) description=errorProperties.description;
	switch (error)
	{
#if 0
	case WUERR_SEDNA_EXCEPTION:
		/*	SednaException has pure virtual functions - unable to throw. */
		assert(0);
		break;
#endif
	case WUERR_SEDNA_SYSTEM_EXCEPTION:
		throw SednaSystemException(file,function,line,description);
		break;
	case WUERR_SEDNA_SYSTEM_ENV_EXCEPTION:
		throw SednaSystemEnvException(file,function,line,description);
		break;
	case WUERR_SEDNA_USER_EXCEPTION:
		throw SednaUserException(file,function,line,description,code);
		break;
	case WUERR_SEDNA_USER_ENV_EXCEPTION:
		throw SednaUserEnvException(file,function,line,description,true);
		break;
	case WUERR_SEDNA_USER_SOFT_EXCEPTION:
		throw SednaUserSoftException(file,function,line,description);
		break;
	default:
		if (!WuIsAppError(error))
		{
			description="host os system api failure (TODO: normal description)";
		}
		throw SednaUserEnvException(file, function, line, description, true);
	}
}
