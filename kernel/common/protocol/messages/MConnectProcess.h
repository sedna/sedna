#ifndef _MCONNECTPROCESS_H_
#define _MCONNECTPROCESS_H_

#include "common/protocol/int_sp.h"
#include "common/socketutils/socketutils.h"

namespace proto {

struct ConnectProcess {
    enum { msgid = se_int_ConnectProcess };

    std::string ticket;

    explicit ConnectProcess(MessageExchanger & comm) { *this << comm; };

    explicit ConnectProcess(const std::string & _ticket) 
        : ticket(_ticket) {};

    MessageExchanger & operator >>(MessageExchanger & comm) {
        comm.beginSend(msgid);
        comm.writeString(ticket);
        comm.endSend();
        return comm;
    };

    MessageExchanger & operator <<(MessageExchanger & comm) {
        if (comm.getInstruction() != msgid) {
            throw proto::InvalidMessage(se_int_ConnectProcess, comm.getInstruction());
        };

        comm.readString(ticket, MAX_TICKET_SIZE);
        return comm;
    };

};

};

#endif /* _MCONNECTPROCESS_H_ */
