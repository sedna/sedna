#ifndef _MSTARTDATABASE_H_
#define _MSTARTDATABASE_H_

#include "common/protocol/int_sp.h"
#include "common/socketutils/socketutils.h"

namespace proto {

struct StartDatabase {
    enum { default_msgid = se_int_StartDatabaseInternal };

    int32_t msgid;
    std::string options;

    explicit StartDatabase(MessageExchanger & comm, int32_t msg = default_msgid) : msgid(msg) { *this << comm; };

    explicit StartDatabase(const std::string & _options, int32_t msg = default_msgid) 
        : msgid(msg), options(_options) {};

    MessageExchanger & operator >>(MessageExchanger & comm) {
        comm.beginSend(msgid);
        comm.writeString(options);
        comm.endSend();
        return comm;
    };

    MessageExchanger & operator <<(MessageExchanger & comm) {
        if (comm.getInstruction() != msgid) {
            throw proto::InvalidMessage(se_int_ConnectProcess, comm.getInstruction());
        };

        comm.readString(options, SE_SOCKET_MSG_BUF_SIZE-128);
        return comm;
    };

};

};

#endif /* _MSTARTDATABASE_H_ */
