#ifndef _CLIENT_H_
#define _CLIENT_H_

#include "tr/crmutils/crmbase.h"

#include <exception>

class tuple_cell;

/*
class StreamSerializer {
public:
    StreamSerializer();
    void write(const tuple_cell & tc);
};
*/

class IClient
{
public:
    virtual ~Client();

    /** @brief Close connection */
    virtual void close() = 0;

    enum { client_stream_std, client_stream_debug, client_stream_trace };

    /** @brief Sends a value to a stream
     * @param stream Output stream. Typically debug or output. */
    virtual void sendItem(int stream, const tuple_cell & tc) = 0;

    /** @brief Report an error to clent
     * @note Exception may be thrown during this call. */
    virtual void sendError(std::exception & e) = 0;

    /** @brief Send any message */
    virtual int sendMessage(int instruction) = 0;
    virtual int sendMessage(int instruction, const std::string & body) = 0;

    /** @brief Request file from server
     *  @return istream to read from a file */
    virtual std::istream * requestFile(const std::string & filename) = 0;

    /** @brief Process internal messages, return last and remove it */
    virtual int getMessage(int stream) = 0;

    /** @brief Process internal messages and return top message, not removing it */
    virtual int peekMessage(int stream) = 0;

/*
    virtual void read_msg(msg_struct *msg) = 0;
    virtual char* get_query_string(msg_struct *msg) = 0;
    virtual void respond_to_client(int instruction) = 0;
    virtual void authentication_result(bool res, const std::string& body) = 0;
    virtual void process_unknown_instruction(int instruction, bool in_transaction) = 0;
    virtual void error(int code, const std::string& body) = 0;
    virtual void show_time(u_timeb qep_time) = 0;
    virtual void show_time_ex(uint64_t qep_time) = 0;
    virtual void write_user_query_to_log() = 0;
    virtual void set_keep_alive_timeout(int sec) = 0;

    virtual void user_statement_begin() = 0;
*/
};

#endif /* _CLIENT_H_ */
