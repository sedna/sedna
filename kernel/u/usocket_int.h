#ifndef  _USOCKET_INT_H_
#define  _USOCKET_INT_H_

#ifndef _WIN32
#include <ancillary.h>

#include <netdb.h>
#include <sys/types.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#else
#include <Winsock2.h>
#include <ws2tcpip.h>
#include <WSPiApi.h>
#endif

#endif /* _USOCKET_INT_H_ */

