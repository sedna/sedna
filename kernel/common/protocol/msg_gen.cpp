#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <map>
#include <stdexcept>
#include <algorithm>

enum primitive_t { ANY, MESSAGE, MESSAGE_ID, MESSAGE_FIXED, BEGIN, FTYPE, END, STRING, FNAME, ARG, INT32, INT64, CHAR };

struct Field
{
    primitive_t type;
    std::string name;
    std::string tname;

    std::map<std::string, std::string> args;
};

struct Message
{
    std::string className;
    std::string messageName;
    bool fixed;

    std::vector<Field> fields;

    Message() : fixed(false) {};
};

int main()
{
    primitive_t state = ANY;
    std::vector<Message> messages;

    Message lastMessage;
    Field lastField;
    bool comment = false;

    for(;;) {
        std::string token;
        primitive_t token_val = ANY;

        if (!(std::cin >> token)) break;

        if (comment) {
            if (token == "*/") {
                comment = false;
            };

            continue;
        };

        if (token == "/*") {
            comment = true;
            continue;
        } else if (token == "message") {
            token_val = MESSAGE;
        } else if (token == "begin") {
            token_val = BEGIN;
        } else if (token == "end") {
            token_val = END;
        } else if (token == "string") {
            token_val = STRING;
        } else if (token == "int64") {
            token_val = INT64;
        } else if (token == "int32") {
            token_val = INT32;
        } else if (token == "char") {
            token_val = CHAR;
        } else if (token == "fixed") {
            token_val = MESSAGE_FIXED;
        };

        switch (state) {
            case ANY : {
                if (token_val != MESSAGE) { throw std::runtime_error("'message' expected"); };
                state = MESSAGE;
            }; break;
            case MESSAGE : {
                if (token_val != ANY) { throw std::runtime_error("Message class name expected"); };
                lastMessage.className = token;
                state = MESSAGE_ID;
            }; break;
            case MESSAGE_ID : {
                if (token_val != ANY) { throw std::runtime_error("Message id expected"); };
                lastMessage.messageName = token;
                state = BEGIN;
            }; break;
            case BEGIN : {
                if (token_val == MESSAGE_FIXED) {
                    lastMessage.fixed = true;
                } else if (token_val == BEGIN) {
                    state = FTYPE;
                } else {
                    throw std::runtime_error("'begin' expected");
                }
            } break;
            case ARG : {
                if (token_val == ANY) {
                    int eqpos = token.find('=', 0);

                    if (eqpos != token.npos) {
                        lastField.args[token.substr(0, eqpos)] = token.substr(eqpos+1, token.npos);
                        break;
                    }
                };
            };
            case FTYPE : {
                if (!lastField.name.empty()) {
                    lastMessage.fields.push_back(lastField);
                    lastField = Field();
                };

                if (token_val == END) {
                    messages.push_back(lastMessage);
                    lastMessage = Message();
                    state = ANY;
                    break;
                };

                if (token_val != STRING && token_val != INT32 && token_val != INT64 && token_val != CHAR) {
                    throw std::runtime_error("Type expected");
                };

                lastField.type = token_val;
                state = FNAME;
            }; break;
            case FNAME : {
                lastField.name = token;
                state = ARG;
            }; break;
            default:
                throw std::runtime_error("Unexpected state");
        }
    }

    if (state != ANY) {
        throw std::runtime_error("Unfinished message");
    };

    for (std::vector<Message>::iterator it = messages.begin(); it != messages.end(); ++it) {
        Message & msg = *it;
        std::string fileName = "M" + msg.className + ".h";
        std::string moduleName = "_M" + msg.className + "_H_";
        std::transform(moduleName.begin(), moduleName.end(), moduleName.begin(), toupper);

        std::ofstream outstream((std::string("messages/") + fileName).c_str());

        outstream << "#ifndef " << moduleName << "\n#define " << moduleName << "\n\n#include \"common/protocol/int_sp.h\"\n";
        outstream << "#include \"common/socketutils/socketutils.h\"\n";
        outstream << "\nnamespace proto {\n\nstruct " + msg.className + " {\n";

        if (msg.fixed) {
            outstream << "    enum { msgid = " << msg.messageName << " };\n\n";
        } else {
            outstream << "    enum { default_msgid = " << msg.messageName << " };\n\n    int32_t msgid;\n";
        };

        for (std::vector<Field>::iterator jt = msg.fields.begin(); jt != msg.fields.end(); ++jt) {
            Field & fld = *jt;

            switch (fld.type) {
              case STRING: fld.tname = "std::string"; break;
              case CHAR:   fld.tname = "char"; break;
              case INT32:  fld.tname = "int32_t"; break;
              case INT64:  fld.tname = "int64_t"; break;
              default:
                throw std::runtime_error("Unexpected type");
            };
        };

        for (std::vector<Field>::iterator jt = msg.fields.begin(); jt != msg.fields.end(); ++jt) {
            outstream << "    " << jt->tname << " " << jt->name << ";\n";
        }

        if (msg.fixed) {
            outstream << "\n    explicit " << msg.className << "(MessageExchanger & comm) { *this << comm; };\n";
        } else {
            outstream << "\n    explicit " << msg.className << "(MessageExchanger & comm, int32_t msg = default_msgid) : msgid(msg) { *this << comm; };\n";
        }

        outstream << "\n    explicit " << msg.className << "(";

        for (std::vector<Field>::iterator jt = msg.fields.begin(); jt != msg.fields.end(); ++jt) {
            if (jt != msg.fields.begin()) {
                outstream << ", ";
            };

            if (jt->type == STRING) {
                outstream << "const " << jt->tname << " & ";
            } else {
                outstream << jt->tname << " ";
            };

            outstream << "_" << jt->name;
        }

        if (!msg.fixed) {
            outstream << ", int32_t msg = default_msgid";
        }

        outstream << ") \n        : ";

        if (!msg.fixed) {
            outstream << "msgid(msg), ";
        };

        for (std::vector<Field>::iterator jt = msg.fields.begin(); jt != msg.fields.end(); ++jt) {
            if (jt != msg.fields.begin()) {
                outstream << ", ";
            };

            outstream << jt->name << "(_" << jt->name << ")";
        }

        outstream << " {};\n";

        outstream << "\n    MessageExchanger & operator >>(MessageExchanger & comm) {";
        outstream << "\n        comm.beginSend(msgid);";

        for (std::vector<Field>::iterator jt = msg.fields.begin(); jt != msg.fields.end(); ++jt) {
            Field & fld = *jt;

            switch (fld.type) {
              case STRING: outstream << "\n        comm.writeString(" << fld.name << ");"; break;
              case CHAR:   outstream << "\n        comm.writeChar(" << fld.name << ");"; break;
              case INT32:  outstream << "\n        comm.writeInt32(" << fld.name << ");"; break;
              case INT64:  outstream << "\n        comm.writeInt64(" << fld.name << ");"; break;
              default:
                throw std::runtime_error("Unexpected type");
            };
        };

        outstream << "\n        comm.endSend();";
        outstream << "\n        return comm;";
        outstream << "\n    };\n";

        outstream << "\n    MessageExchanger & operator <<(MessageExchanger & comm) {";
        outstream << "\n        if (comm.getInstruction() != msgid) {";
        outstream << "\n            throw proto::InvalidMessage(se_int_ConnectProcess, comm.getInstruction());";
        outstream << "\n        };\n";

        for (std::vector<Field>::iterator jt = msg.fields.begin(); jt != msg.fields.end(); ++jt) {
            Field & fld = *jt;

            switch (fld.type) {
              case STRING: outstream << "\n        comm.readString(" << fld.name << ", " << fld.args["max"] << ");"; break;
              case CHAR:   outstream << "\n        comm.readChar(" << fld.name << ");"; break;
              case INT32:  outstream << "\n        comm.readInt32(" << fld.name << ");"; break;
              case INT64:  outstream << "\n        comm.readInt64(" << fld.name << ");"; break;
              default:
                throw std::runtime_error("Unexpected type");
            };
        };

        outstream << "\n        return comm;";
        outstream << "\n    };\n";

        outstream << "\n};\n\n};\n\n";
        outstream << "#endif /* " << moduleName << " */\n";
    }

    return 0;
};
