#ifndef _MUSERCREATEDATABASE_H_
#define _MUSERCREATEDATABASE_H_

#include "common/protocol/int_sp.h"
#include "common/socketutils/socketutils.h"

namespace proto {

struct UserCreateDatabase {
    enum { msgid = se_CreateDatabaseRequest };

    std::string dbname;
    std::string options;

    explicit UserCreateDatabase(MessageExchanger & comm) { *this << comm; };

    explicit UserCreateDatabase(const std::string & _dbname, const std::string & _options) 
        : dbname(_dbname), options(_options) {};

    MessageExchanger & operator >>(MessageExchanger & comm) {
        comm.beginSend(msgid);
        comm.writeString(dbname);
        comm.writeString(options);
        comm.endSend();
        return comm;
    };

    MessageExchanger & operator <<(MessageExchanger & comm) {
        if (comm.getInstruction() != msgid) {
            throw proto::InvalidMessage(msgid, comm.getInstruction());
        };

        comm.readString(dbname, MAX_DB_NAME);
        comm.readString(options, MAX_XML_PARAMS);
        return comm;
    };

};

};

#endif /* _MUSERCREATEDATABASE_H_ */
