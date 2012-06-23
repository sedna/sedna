#include "gov/cpool.h"
#include "gov/clients.h"
#include "common/errdbg/d_printf.h"


using namespace std;

/////////////////////////////// WORKER implementation

    /* TODO: this should work some other way, not like this */
// WorkerSocketClient * Worker::addClient(WorkerSocketClient * stream) {
//         USOCKET s = stream->getSocket();
// 
//         /* We add client to separate client list not to spoil the client list iterator */
//         newClients.push_back(stream);
//         
//         U_SSET_SET(s, &allSet);
// 
//         if (maxfd < s) {
//                 maxfd = s;
//         }
// 
//         return stream;
// };
// 
// void Worker::deleteClient(WorkerSocketClient * stream) {
//         USOCKET s = stream->getSocket();
//         U_SSET_CLR(s, &allSet);
//         stream->setObsolete();
// }

SocketClient * ListenerSocket::processData() {
        USOCKET negotiation_socket_stream;
        negotiation_socket_stream = uaccept(clientSocket, __sys_call_error);

        WorkerSocketClient * newSocketStream = new ClientNegotiationManager(worker, negotiation_socket_stream);

        if(negotiation_socket_stream == U_INVALID_SOCKET) {
                throw SYSTEM_EXCEPTION("Can't accept client's connection");
        }

        socketSetNoDelay(negotiation_socket_stream);
        
        /* TODO: here we need to add client too, need to fix it in the new way. Otherwise nothing will work */
//         worker->addClient(newSocketStream);

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


WorkerSocketClient * Worker::createListener() {
        USOCKET listening_socket;

        listening_socket = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
        if(listening_socket==U_INVALID_SOCKET)
                throw SYSTEM_EXCEPTION("Can't init socket");

        if(ubind_tcp(listening_socket, cfg->gov_vars.lstnr_port_number, cfg->gov_vars.lstnr_addr, __sys_call_error) == U_SOCKET_ERROR)
                throw SYSTEM_EXCEPTION("Can't bind socket");

        if(ulisten(listening_socket, 100, __sys_call_error) == U_SOCKET_ERROR)
                throw SYSTEM_EXCEPTION("Can't set socket to a listening mode ");

        ownListenerSocket = new ListenerSocket(this, listening_socket);
        
        ///////// NOTIFY THAT SERVER IS READY //////////////////////////////////
        USemaphore started_sem;
        if (0 == USemaphoreOpen(&started_sem, SEDNA_GOVERNOR_IS_READY, __sys_call_error))
        {
            USemaphoreUp(started_sem, __sys_call_error);
            USemaphoreClose(started_sem, __sys_call_error);
        }
        ///////// NOTIFY THAT SERVER IS READY //////////////////////////////////
        
        set_session_common_environment();
        
        return addClient(ownListenerSocket);
}

void Worker::run() {
    for (;;) {
        memcpy(&readySet, &allSet, sizeof(readySet));

        int readyCount = uselect_read_arr(&readySet, maxfd, NULL, __sys_call_error);

        if (readyCount == U_SOCKET_ERROR) {
            U_ASSERT(false);
            throw USER_EXCEPTION2(SE3007, usocket_error_translator());
        }

        for (SocketClientList::iterator i = clientList.begin(); i != clientList.end(); ) {
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
                try {
                    while (client != NULL) {
                        WorkerSocketClient * nextProcessor =
                            static_cast<WorkerSocketClient *>(client->processData());
                        readyCount--;

                        if (client != nextProcessor) {
                            delete client;
                            client = nextProcessor;
                            *i = client;
                        } else {
                            break;
                        };
                    }

                    if (client == NULL) {
                        U_SSET_CLR(socket, &allSet);
                        i = clientList.erase(i);
                        continue;
                    }
                } catch (SednaGovSocketException e) {
                    e.ref->cleanupOnError();
                }
            }
            ++i;

            /* If we already read all desciptors, break inner loop */
            if (readyCount == 0) {
                break;
            };
        }

        
        /* !TODO!: not sure but it seems that we should add new clients to process manager from there and then we should transmit them to
         * corresponding worker. Commented out for now */
        /* Add new clients to client list */
//         if (!newClients.empty()) {
//             clientList.insert(clientList.end(), newClients.begin(), newClients.end());
//             newClients.clear();
//                sort(clientList.begin(), clientList.end(), WorkerSocketClientGreater());
//         }
// 
//         if (runningSms.empty() && getShutdown()) {
//             for (ClientList::iterator j = clientsWaitingForShutdown.begin(); j!= clientsWaitingForShutdown.end(); j++) {
//                 (*j)->processData();
//             }
//             clientsWaitingForShutdown.clear();
//             break;
//         }
    }

    delete ownListenerSocket;
}