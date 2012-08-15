/* TODO: Make test monolith and independant from other processes */
#include <common/socketutils/socketutils.h>
#include <stdio.h>

#ifndef _WIN32
#include <sys/types.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#else
#include <Winsock2.h>
#include <ws2tcpip.h>
#include <WSPiApi.h>
#endif


static
void test_error(const char *filename, int lineno, const char *funcname, const char *sys_call, const void* arg)
{
    fprintf(stderr, "Error at %s : %s; @%s:%d %s\n", sys_call, strerror(errno), filename, lineno, funcname);
    fflush(stderr);
//    errorCount++;
//    ASSERT_FALSE(true);
//    get_current_dir_name();
};


int main(int argc, char** argv) {
  USOCKET sock;
  int socket_optval = 1, socket_optsize = sizeof(int);
  if (0 != uSocketInit(test_error)) fprintf(stderr, "Can't init socket subsystem\n");
  sock = usocket(AF_INET,SOCK_STREAM,0,test_error);
  if (sock == U_INVALID_SOCKET) fprintf(stderr, "Can't init socket\n");
  if (U_SOCKET_ERROR == usetsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (char *) &socket_optval, socket_optsize, test_error) ) fprintf(stderr, "Can't set socket options\n");
  if (0 != uconnect_tcp(sock,5050,"localhost",test_error)) fprintf (stderr, "Can't connect to gov\n");
  
  MessageExchanger communicator(sock);
  communicator.beginSend(se_StartUp);
  communicator.writeChar(0);
  communicator.writeChar(4);
  if (0 != communicator.endSend()) fprintf(stderr, "can't send\n");
  
  return 0;
}