#ifndef _MREGISTRATIONFAILED_H_
#define _MREGISTRATIONFAILED_H_

#include "common/protocol/int_sp.h"
#include "common/socketutils/socketutils.h"

namespace proto {

struct RegistrationFailed {
    enum { default_msgid = se_int_RegistrationFailed };

    int32_t msgid;
    std::string error;

    explicit RegistrationFailed(MessageExchanger & comm, int32_t msg = default_msgid) : msgid(msg) { *this << comm; };

    explicit RegistrationFailed(const std::string & _error, int32_t msg = default_msgid) 
        : msgid(msg), error(_error) {};

    MessageExchanger & operator >>(MessageExchanger & comm) {
        comm.beginSend(msgid);
        comm.writeString(error);
        comm.endSend();
        return comm;
    };

    MessageExchanger & operator <<(MessageExchanger & comm) {
        if (comm.getInstruction() != msgid) {
            throw proto::InvalidMessage(msgid, comm.getInstruction());
        };

        comm.readString(error, MAX_ERROR_LENGTH);
        return comm;
    };

};

};

#endif /* _MREGISTRATIONFAILED_H_ */
