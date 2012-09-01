#include "common/socketutils/socketutils.h"
#include <string>

#ifndef _WIN32
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

#include "common/protocol/sp.h"
#include "common/protocol/int_sp.h"

using namespace std;

int main(int argc, char **argv) {
  
  if (argc == 1) 
  {
    printf ("You should pass dbname as parameter to test cdb\n");
    return -2;
  }
  
  std::string dbname = std::string (argv[1]);
  std::string cdbOptions;
  cdbOptions = std::string("<sednaOptions><databaseDefaults>") 
    + std::string("<databaseName>") + dbname + std::string("</databaseName>")
    + std::string("</databaseDefaults></sednaOptions>");

  if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) return -1;
  
  USOCKET s;
  s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
  if(s == U_SOCKET_ERROR) {
      printf ("Socket creation failed\n");
      return -1;
  }

  if(uconnect_tcp(s, 5050, "localhost", __sys_call_error) != 0) {
      printf ("Connect failed\n");
      ushutdown_close_socket(s, __sys_call_error);
      return -1;
  }
        
  MessageExchanger * communicator = new MessageExchanger(s);
  
  communicator->beginSend(se_StartUp);
  communicator->writeChar(5);
  communicator->writeChar(0);
  communicator->endSend();
  
  while(!communicator->receive());
  if (communicator->getInstruction() != se_SendServiceAuth) {
      delete communicator;
      printf("wrong message\n");
      return -2;
  }
  
  communicator->beginSend(se_SendServiceAuth);
  communicator->writeString("SYSTEM");
  communicator->writeString("MANAGER");
  communicator->endSend();
  
  while(!communicator->receive());
  if (communicator->getInstruction() != se_AuthenticationOK) {
      delete communicator;
      printf("auth failed\n");
      return -3;
  }
  communicator->beginSend(se_CreateDbRequest);
  communicator->writeString(dbname);
  communicator->writeString(cdbOptions);
  communicator->endSend();

  while(!communicator->receive());
  if (communicator->getInstruction() != se_CreateDbOK) {
    delete communicator;
    printf("Database creation failed; maybe SM failed\n");
    return -4;
  }
  
  printf("Database created successfully\n");
  
  delete communicator;
  ushutdown_close_socket(s, __sys_call_error);
  
  return 0;
}