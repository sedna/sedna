#ifndef  _INTERNALCONNECTION_H_
#define  _INTERNALCONNECTION_H_

struct DatabaseOptions;
class MessageExchanger;

class MasterProcessConnection
{
public:
    MessageExchanger * communicator;
    DatabaseOptions* databaseOptions;

    MasterProcessConnection(const char * address, const char * port);
    ~MasterProcessConnection();

    void registerOnGov(const char * ticket);
    void nextMessage();
    void setDatabaseOptions(DatabaseOptions* dbOptions) { databaseOptions = dbOptions; };
};


#endif /* _INTERNALCONNECTION_H_ */
