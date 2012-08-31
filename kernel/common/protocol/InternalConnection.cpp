#include "InternalConnection.h"
#include "int_sp.h"

#include "common/sedna.h"
#include "common/socketutils/socketutils.h"

#include "u/usocket.h"
#include "u/usocket_int.h"

MasterProcessConnection::MasterProcessConnection(const char* address, const char* port)
  : communicator(NULL)
{
    if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) {
        throw USER_EXCEPTION(SE3001);
    }

    unsigned int portNumber;

    if (sscanf(port, "%d", &portNumber) == 0) {
        throw SYSTEM_ENV_EXCEPTION("Invalid port argument");
    };
 
    USOCKET s;
    s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
    if(s == U_SOCKET_ERROR)
        throw USER_EXCEPTION (SE3001);

    if (uconnect_tcp(s, portNumber, address, __sys_call_error) != 0)
    {
        ushutdown_close_socket(s, __sys_call_error);
        throw USER_EXCEPTION (SE3003);
    }

    MessageExchanger * communicator = new MessageExchanger(s);
}

void MasterProcessConnection::nextMessage()
{
    communicator->wait4();
}

MasterProcessConnection::~MasterProcessConnection()
{
    delete communicator;
}

void MasterProcessConnection::registerOnGov(const char* ticket)
{
    communicator->beginSend(se_ConnectProcess);
    communicator->writeString(ticket);

    if (0 != communicator->endSend()) {
      throw USER_EXCEPTION2(SE3006, usocket_error_translator());;
    }

    nextMessage();

    switch (communicator->getInstruction()) {
      default:
        throw SYSTEM_ENV_EXCEPTION("Invalid message from server");
//        se
    };
}


