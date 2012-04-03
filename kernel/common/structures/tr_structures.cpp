#include "common/structures/tr_structures.h"
#include "common/socketutils/socketutils.h"

#define CHECKBUFLEN(len) if ((len) < 0) { /* TODO: error */ return NULL; }

inline static
char * copyBufStr(const char ** buf, int * buflen) {
        uint32_t len;
        char * result;

        *buflen -= sizeof(len);
        if (*buflen < 0) {
                // TODO: error --- buffer overflow
                return NULL;
        }

        memcpy(&len, *buf, sizeof(len)); *buf += sizeof(len);
        len = ntohl(len);

        *buflen -= len;
        if (*buflen < 0) {
                // TODO: error --- buffer overflow
                return NULL;
        }

        result = (char *) malloc(len + 1);
        memcpy(result, *buf, len); *buf += len;
        result[len] = '\0';

        return result;
}



inline static
char * copyStrBuf(char * buf, int * buflen, const char * str, int strlen) {
        *buflen -= 1; CHECKBUFLEN(*buflen);
        buf[0] = '\0'; buf++;

        uint32_t len = htonl((uint32_t) strlen);
        *buflen -= sizeof(len); CHECKBUFLEN(*buflen);
        memcpy(buf, &len, sizeof(len)); buf += sizeof(len);

        *buflen -= strlen; CHECKBUFLEN(*buflen);
        memcpy(buf, str, (size_t) strlen); buf += strlen;

        return buf;
}


void SessionParameters::readSessParams(MessageExchanger* communicator) {
        protocolVersion.maj = communicator->readChar();
        protocolVersion.min = communicator->readChar();
        if (protocolVersion.maj < 5); //!TODO !FIXME: make throw!!! return -1; //incompatible protocol version
        
        communicator->readString(userName, SE_MAX_LOGIN_LENGTH);
        communicator->readString(dbName, SE_MAX_DB_NAME_LENGTH);
        
        return;
};

void SessionParameters::readAuthParams(MessageExchanger* communicator) {
        communicator->readString(password, SE_MAX_PASSWORD_LENGTH);
        return;
};

void SessionParameters::sendSessParams(MessageExchanger* communicator) {
        communicator->beginSend(se_SessionParameters);
        communicator->writeChar(protocolVersion.maj);
        communicator->writeChar(protocolVersion.min);
        communicator->writeString(userName);
        communicator->writeString(dbName);
        communicator->endSend();
        return;
}

void SessionParameters::sendAuthParams(MessageExchanger* communicator) {
        communicator->beginSend(se_AuthenticationParameters);
        communicator->writeString(password);
        communicator->endSend();
        return;
}

size_t SessionParameters::writeSessParams(char * buf, int maxbuflen) {
        const char * bufstart = buf;

        maxbuflen -= 2;
        memcpy(buf, &protocolVersion, sizeof(protocolVersion)); buf += 2;
        
        if (userName == NULL) { /* TODO: error */ return 0; }
        buf = copyStrBuf(buf, &maxbuflen, userName, strlen(userName));

        if (dbName == NULL) { /* TODO: error */ return 0; }
        buf = copyStrBuf(buf, &maxbuflen, dbName, strlen(dbName));

        return (buf - bufstart);
};

size_t SessionParameters::writeAuthParams(char * buf, int maxbuflen) {
        const char * bufstart = buf;

        if (password == NULL) { /* TODO: error */ return 0; }
        buf = copyStrBuf(buf, &maxbuflen, password, strlen(password));

        return (buf - bufstart);
};

