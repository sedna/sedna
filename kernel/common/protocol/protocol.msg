message ConnectProcess se_int_ConnectProcess fixed
begin
  string ticket max=MAX_TICKET_SIZE
end

message StartDatabase se_int_StartDatabaseInternal
begin
  string options max=SE_SOCKET_MSG_BUF_SIZE-128
end


