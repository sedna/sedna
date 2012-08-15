/* TODO: Make test monolith and independant from other processes */
#include <common/socketutils/socketutils.h>

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
  uSocketInit(test_error);
  sock = usocket(AF_INET,SOCK_STREAM,0,test_error);
  usetsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (char *) &socket_optval, socket_optsize, test_error);
  uconnect_tcp(sock,5050,"localhost",test_error);
  
  MessageExchanger communicator(sock);
  communicator.beginSend(se_StartUp);
  communicator.writeChar(0);
  communicator.writeChar(4);
  communicator.endSend();
  
  return 0;
}