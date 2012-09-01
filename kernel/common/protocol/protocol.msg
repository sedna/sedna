message ConnectProcess se_int_ConnectProcess fixed
begin
  string ticket max=MAX_TICKET_SIZE
end

message StartDatabase se_int_StartDatabaseInternal
begin
  string options max=MAX_XML_PARAMS
end

message RegistrationFailed se_int_RegistrationFailed
begin
  string error max=MAX_ERROR_LENGTH
end

message RegisterSession se_int_SessionParametersInternal fixed
begin
  int32 session_id
  string options max=MAX_XML_PARAMS
end

message StartUp se_StartUp fixed
begin
  char version_maj
  char version_min
end

message ServiceAuth se_SendServiceAuth fixed
begin
  string user max=MAX_DB_NAME
  string password max=MAX_DB_NAME
end

message UserCreateDatabase se_CreateDatabaseRequest fixed
begin
  string dbname max=MAX_DB_NAME
  string options max=MAX_XML_PARAMS
end
