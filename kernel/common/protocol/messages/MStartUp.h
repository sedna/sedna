#ifndef _MSTARTUP_H_
#define _MSTARTUP_H_

#include "common/protocol/int_sp.h"
#include "common/socketutils/socketutils.h"

namespace proto {

struct StartUp {
    enum { msgid = se_StartUp };

    uint8_t version_maj;
    uint8_t version_min;

    explicit StartUp(MessageExchanger & comm) { *this << comm; };

    explicit StartUp(uint8_t _version_maj, uint8_t _version_min) 
        : version_maj(_version_maj), version_min(_version_min) {};

    MessageExchanger & operator >>(MessageExchanger & comm) {
        comm.beginSend(msgid);
        comm.writeChar(version_maj);
        comm.writeChar(version_min);
        comm.endSend();
        return comm;
    };

    MessageExchanger & operator <<(MessageExchanger & comm) {
        if (comm.getInstruction() != msgid) {
            throw proto::InvalidMessage(msgid, comm.getInstruction());
        };

        version_maj = comm.readChar();
        version_min = comm.readChar();
        return comm;
    };

};

};

#endif /* _MSTARTUP_H_ */
