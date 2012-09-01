#include "common/socketutils/socketutils.h"

#include "u/usocket_int.h"

#include "common/protocol/sp.h"
#include "common/protocol/int_sp.h"

#include "common/protocol/messages/MStartUp.h"
#include "common/protocol/messages/MServiceAuth.h"
#include "common/protocol/messages/MUserCreateDatabase.h"

#include <string>
#include <sp_defs.h>

using namespace std;

int main(int argc, char **argv) {
  if (argc == 1) 
  {
    printf ("You should pass dbname as parameter to test cdb\n");
    return -2;
  }

  std::string dbname = std::string (argv[1]);
  std::string cdbOptions = "<databaseOptions><databaseName>" + dbname + "</databaseName></databaseOptions>";

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

  counted_ptr<MessageExchanger> communicator(new MessageExchanger(s));

  proto::StartUp(5, 0) >> *communicator;
  communicator->wait4();

  if (communicator->getInstruction() != se_SendServiceAuth) {
      return 1;
  };

  proto::ServiceAuth("SYSTEM", "MANAGER") >> *communicator;
  communicator->wait4();

  if (communicator->getInstruction() != se_AuthenticationOK) {
      return 2;
  };

  proto::UserCreateDatabase(dbname, cdbOptions) >> *communicator;
  communicator->wait4();

  if (communicator->getInstruction() != se_CreateDbOK) {
      printf("%d\n", communicator->getInstruction());
      return 3;
  };

  printf("Database created successfully\n");

  ushutdown_close_socket(s, __sys_call_error);
  return 0;
}