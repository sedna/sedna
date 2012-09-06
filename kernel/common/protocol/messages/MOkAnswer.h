#ifndef _MOKANSWER_H_
#define _MOKANSWER_H_

#include "common/protocol/int_sp.h"
#include "common/socketutils/socketutils.h"

namespace proto {

struct OkAnswer {
    enum { msgid = se_Ok };

    std::string explain;

    explicit OkAnswer(MessageExchanger & comm) { *this << comm; };

    explicit OkAnswer(const std::string & _explain) 
        : explain(_explain) {};

    MessageExchanger & operator >>(MessageExchanger & comm) {
        comm.beginSend(msgid);
        comm.writeString(explain);
        comm.endSend();
        return comm;
    };

    MessageExchanger & operator <<(MessageExchanger & comm) {
        if (comm.getInstruction() != msgid) {
            throw proto::InvalidMessage(msgid, comm.getInstruction());
        };

        comm.readString(explain, MAX_ERROR_LENGTH);
        return comm;
    };

};

};

#endif /* _MOKANSWER_H_ */
