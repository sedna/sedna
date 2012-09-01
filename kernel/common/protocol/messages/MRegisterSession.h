#ifndef _MREGISTERSESSION_H_
#define _MREGISTERSESSION_H_

#include "common/protocol/int_sp.h"
#include "common/socketutils/socketutils.h"

namespace proto {

struct RegisterSession {
    enum { msgid = se_int_SessionParametersInternal };

    int32_t session_id;
    std::string options;

    explicit RegisterSession(MessageExchanger & comm) { *this << comm; };

    explicit RegisterSession(int32_t _session_id, const std::string & _options) 
        : session_id(_session_id), options(_options) {};

    MessageExchanger & operator >>(MessageExchanger & comm) {
        comm.beginSend(msgid);
        comm.writeInt32(session_id);
        comm.writeString(options);
        comm.endSend();
        return comm;
    };

    MessageExchanger & operator <<(MessageExchanger & comm) {
        if (comm.getInstruction() != msgid) {
            throw proto::InvalidMessage(msgid, comm.getInstruction());
        };

        session_id = comm.readInt32();
        comm.readString(options, MAX_XML_PARAMS);
        return comm;
    };

};

};

#endif /* _MREGISTERSESSION_H_ */
