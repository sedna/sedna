#ifndef _MSERVICEAUTH_H_
#define _MSERVICEAUTH_H_

#include "common/protocol/int_sp.h"
#include "common/socketutils/socketutils.h"

namespace proto {

struct ServiceAuth {
    enum { msgid = se_SendServiceAuth };

    std::string user;
    std::string password;

    explicit ServiceAuth(MessageExchanger & comm) { *this << comm; };

    explicit ServiceAuth(const std::string & _user, const std::string & _password) 
        : user(_user), password(_password) {};

    MessageExchanger & operator >>(MessageExchanger & comm) {
        comm.beginSend(msgid);
        comm.writeString(user);
        comm.writeString(password);
        comm.endSend();
        return comm;
    };

    MessageExchanger & operator <<(MessageExchanger & comm) {
        if (comm.getInstruction() != msgid) {
            throw proto::InvalidMessage(msgid, comm.getInstruction());
        };

        comm.readString(user, MAX_DB_NAME);
        comm.readString(password, MAX_DB_NAME);
        return comm;
    };

};

};

#endif /* _MSERVICEAUTH_H_ */
