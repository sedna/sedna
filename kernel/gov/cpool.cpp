#include "gov/cpool.h"
#include "gov/clients.h"
#include "common/errdbg/d_printf.h"
#include "u/usem.h"
#include "u/usocket.h"

#include "u/usocket_int.h"

using namespace std;

/////////////////////////////// WORKER implementation

WorkerSocketClient * Worker::addClient(WorkerSocketClient * stream) {
        USOCKET s = stream->getSocket();

        U_SSET_SET(s, &allSet);

        if (maxfd < s) {
                maxfd = s;
        }
        
        /* We add client to separate client list not to spoil the client list iterator 
         * It's obligatory, don't forget for the second time! 
         */
        newClients.push_back(stream);
        return stream;
};


/* NOTE: Must always return self -- otherwise it will be deleted on the third step*/
SocketClient * ListenerSocket::processData() {
        USOCKET negotiation_socket_stream;
        negotiation_socket_stream = uaccept(clientSocket, __sys_call_error);

        if(negotiation_socket_stream == U_INVALID_SOCKET) {
            throw SYSTEM_EXCEPTION("Can't accept client's connection");
        }
        
        socketSetNoDelay(negotiation_socket_stream);
        
        WorkerSocketClient * newSocketStream = new ClientNegotiationManager(worker, negotiation_socket_stream);
        
        worker->addClient(newSocketStream);

        return this;
}

/* TODO: need to modify this function in the new way. But this function is NECESSARY */
/* this function is needed for socket trespassing in *nix systems */
// void Worker::createUnixListener(TRNInfo * trninfo) {
// #ifndef _WIN32
//         USOCKET listening_socket;
//         char socket_unix_address[14];
//         memset (socket_unix_address, 0, 14);
//         memcpy (socket_unix_address, "sdn", strlen("sdn"));
//         
//         u_itoa(trninfo->sess_id, socket_unix_address + strlen("sdn"), 10);
//         listening_socket = usocket(AF_UNIX, SOCK_STREAM, 0, __sys_call_error);
//         if(listening_socket==U_INVALID_SOCKET)
//                 throw SYSTEM_EXCEPTION("Can't init unix socket");
//        
//         if(ubind_unix(listening_socket, socket_unix_address, __sys_call_error) == U_SOCKET_ERROR)
//                 throw SYSTEM_EXCEPTION("Can't bind socket");
// 
//         if(ulisten(listening_socket, 100, __sys_call_error) == U_SOCKET_ERROR)
//                 throw SYSTEM_EXCEPTION("Can't set socket to a listening mode ");
// 
//         WorkerSocketClient * UnixListenerSocket = new ListenerSocket(this, listening_socket);
//         
//         addClient(UnixListenerSocket);
// #endif
//         return;
// }


/**
 * Binds a new listening socket to globally available address
 */

WorkerSocketClient * Worker::createListener() 
{
        USOCKET listening_socket;

        listening_socket = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
        if(listening_socket==U_INVALID_SOCKET) {
            throw SYSTEM_EXCEPTION("Can't init socket");
        }

        if(ubind_tcp(listening_socket, 
                     (processManager->getGlobalParameters())->global.listenPort, 
                     (processManager->getGlobalParameters())->global.bindAddress.c_str(), 
                     __sys_call_error) == U_SOCKET_ERROR) {
            throw SYSTEM_EXCEPTION("Can't bind socket");
        }

        if(ulisten(listening_socket, 100, __sys_call_error) == U_SOCKET_ERROR) {
            throw SYSTEM_EXCEPTION("Can't set socket to a listening mode ");
        }

        ownListenerSocket = new ListenerSocket(this, listening_socket);
        
        return this->addClient(ownListenerSocket);
}

void Worker::run() {
    for (;;) {
        clientList.insert(clientList.end(), newClients.begin(), newClients.end());
        newClients.clear();

        processManager->processRequests();

        memcpy(&readySet, &allSet, sizeof(readySet));

        int readyCount = uselect_read_arr(&readySet, maxfd, NULL, __sys_call_error);

        if (readyCount == U_SOCKET_ERROR) {
            U_ASSERT(false);
            throw SYSTEM_EXCEPTION(usocket_error_translator());
        }

        for (UnsortedSocketClientList::iterator i = clientList.begin(); i != clientList.end(); ) {
            WorkerSocketClient * client = *i;

            if (client->isObsolete()) {
                USOCKET socket = client->getSocket();
                U_SSET_CLR(socket, &allSet);
                i = clientList.erase(i);
                delete client;
                continue;
            }

            USOCKET socket = client->getSocket();

            if (U_SSET_ISSET(socket, &readySet)) {
                readyCount--;

                try {
                    while (client != NULL) {
                        WorkerSocketClient * nextProcessor =
                            static_cast<WorkerSocketClient *>(client->processData());

                        if (client != nextProcessor) {
                            /* NOTE: socket cannot be changed in client */
                            U_ASSERT(socket == client->getSocket());
                            delete client;
                            client = nextProcessor;
                            *i = client;
                        } else {
                            break;
                        };
                    }
                } catch (SednaClientSocketException e) {
                    U_ASSERT(client == e.ref);
                    delete client;
                    client = NULL;
                } catch (SednaGovSocketException e) {
                    e.ref->cleanupOnError();
                    elog(EL_ERROR, (e.what()));
                }

                if (client == NULL) {
                    U_SSET_CLR(socket, &allSet);
                    i = clientList.erase(i);
                    continue;
                }
            }

            ++i;

            /* If we already read all desciptors, break inner loop */
            /* NOTE: readyCount may be negative, really */
            if (readyCount <= 0) {
                break;
            };
        }
    }
}