#include "se_exp_common.h"
#include "se_exp_queries.h"
#include "se_exp.h"

//function checks that the database db_name is empty
// 1. document("$documents.xml") contains only db_security_data record
// 2. document("$collections.xml") is empty
// 3. document("indexes.xml") is empty
// 4. document("db_security") has only default structures
// the result is: 1 - db is empty, 0 - db contains data, -1 - error

int check_dbempty(struct SednaConnection *conn, FILE* log) {
  char *str_res=NULL;
  int res, status;
	
	if ((status = execute_retrieve_query(&str_res, conn, check_db_empty_query,log))!=SE_EXP_SUCCEED) {
		ETRACE((log,"\nERROR: Can't check emptyness of the database\n"));
		return getSednaErrorStatus(SEgetLastErrorMsg(conn));
	}
    if (!strcmp(str_res,"0")) 
		res=0;
	else
	if (!strcmp(str_res,"1"))
		res=1;
	else {
		ETRACE((log,"\nUnexpected result while checking the database\n"));
		res=SE_EXP_DEV_ERROR;
	}
    
	free(str_res);
	return res;
}




int restore_security(struct SednaConnection *conn, const char *path, FILE* log) {
  char strbuf[PATH_SIZE];

   sprintf(strbuf,"%s%s.xml",path,DB_SECURITY_DOC);
   if (bulkload_xml(conn,strbuf,DB_SECURITY_DOC_NAME_TMP,log) != 0) {
	   ETRACE((log,"\nERROR: failed to bulkload document with new security data\n"));
	   return -1;
   } 
  
   sprintf(strbuf,"UPDATE replace $p in doc('%s')/db_security_data with doc(%s)/db_security_data",DB_SECURITY_DOC,DB_SECURITY_DOC_NAME_TMP);
   if (SEexecute(conn,strbuf) != SEDNA_UPDATE_SUCCEEDED) {
	   ETRACE((log,"\nERROR: failed to update document with initial security data\n"));
	   return -1;
   }

   sprintf(strbuf,"DROP DOCUMENT %s",DB_SECURITY_DOC_NAME_TMP);
   if (SEexecute(conn,strbuf) != SEDNA_UPDATE_SUCCEEDED) {
	   ETRACE((log,"\nERROR: failed to drop temporary document with security data\n"));
	   return -1;
   } 
   return 0;
}




// function imports data from path to the specified data base
// se_import defines weather to import security information
// if se_import equals to 1 it is rquired that the database is empty
int import(const char *path,const char *url,const char *db_name,const char *login,const char *password, int sec_import) {
  struct SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;
  char strbuf[PATH_SIZE];
  char *cr_col_query = NULL;
  char *bl_docs_query = NULL;
  char *cr_indexes_query = NULL;
  char *cr_ftindexes_query = NULL;
  char *upd_sec_query = NULL;
  qbuf_t blq = {NULL,0,0};
  FILE *log = NULL;  
  int i,error_status=1;
  int value;

    sprintf(strbuf,"%s%s",path,EXP_LOG_FILE_NAME);

	if ((log=fopen(strbuf,"r"))==NULL) {
		printf("ERROR: the specified path \"%s\" does't contain Sedna DB export files\n",path);
		goto imp_error_no_conn;
	} else {
		fclose(log);
	}

	sprintf(strbuf,"%s%s",path,IMP_LOG_FILE_NAME);
	if ((log=fopen(strbuf,"w"))==NULL) {
		ETRACE((log,"ERROR: the specified path \"%s\" is not accesible for writing\n",path));
		goto imp_error_no_conn;
	}
	
	FTRACE((log,"Connecting to Sedna..."));
    if(SEconnect(&conn, url, db_name, login, password)!= SEDNA_SESSION_OPEN) {
		ETRACE((log,"ERROR: can't connect to Sedna XML DB\n%s\n", SEgetLastErrorMsg(&conn)));
		goto imp_error_no_conn;
	}
	FTRACE((log,"done\n"));

	value = SEDNA_AUTOCOMMIT_OFF;
    SEsetConnectionAttr(&conn, SEDNA_ATTR_AUTOCOMMIT, (void*)&value, sizeof(int));



    FTRACE((log,"Starting transaction..."));
	if (SEbegin(&conn)!= SEDNA_BEGIN_TRANSACTION_SUCCEEDED) {
		ETRACE((log,"ERROR: failed to begin transaction\n"));
	    goto imp_error;
	}
	FTRACE((log,"done\n"));
	
	if (sec_import==1) {
		// restoring data
		int db_empty=check_dbempty(&conn,log); 
		if (db_empty==-1) 
			goto imp_error;
		else
			if (db_empty==0) {
				ETRACE((log,"ERROR: database '%s' is not empty\n",db_name));
				goto imp_error;
			}
	}

	FTRACE((log,"Reading scripts from files..."));
	sprintf(strbuf,"%s%s",path,CR_COL_QUERY_FILE);
	if ((cr_col_query = read_query(strbuf))==NULL) 
		goto imp_error;   

	sprintf(strbuf,"%s%s",path,CR_INDEXES_QUERY_FILE);
	if ((cr_indexes_query = read_query(strbuf))==NULL) 
		goto imp_error; 

	sprintf(strbuf,"%s%s",path,CR_FTINDEXES_QUERY_FILE);
	if ((cr_ftindexes_query = read_query(strbuf))==NULL) 
		goto imp_error; 

	sprintf(strbuf,"%s%s",path,LOAD_DOCS_QUERY_FILE);
	if ((bl_docs_query = read_query(strbuf))==NULL)
		goto imp_error;
	
	/* if we merge security information
	sprintf(strbuf,"%s%s",path,CR_SEC_QUERY_FILE);
	if ((upd_sec_query = read_query(strbuf))==NULL)
		goto imp_error;
	*/
    FTRACE((log,"done\n"));

	

	FTRACE((log,"Creating collections..."));
	if (strlen(cr_col_query)==0)
		FTRACE((log,"(no collections in the database)..."));
	else
        if (execute_multiquery(&conn,cr_col_query, log)!=0) 
			goto imp_error;
	FTRACE((log,"done\n"));


	FTRACE((log,"Loading documents..."));
	if (strlen(bl_docs_query)==0)
		FTRACE((log,"(no documents in the database)..."));
	else {
		FTRACE((log,"\n"));
        if (split_query(bl_docs_query,&blq)!=0) 
			goto imp_error;
        for (i=0;i<blq.d_size;i++) {
			// blq.buf[i] = "docname" "colname"
			sprintf(strbuf,"%s%d.xml",path,i+1);
			FTRACE((log," Bulkload document %s...",blq.buf[i]));
			if (bulkload_xml(&conn,strbuf,blq.buf[i],log)!=0)
				goto imp_error;
			FTRACE((log,"done\n"));
		}
	}
	FTRACE((log,"done\n"));
	
	FTRACE((log,"Creating indexes..."));
	if (strlen(cr_indexes_query)==0)
		FTRACE((log,"(no indexes in the database)..."));
	else
		if (execute_multiquery(&conn,cr_indexes_query,log)!=0) 
			goto imp_error;
	FTRACE((log,"done\n"));


	FTRACE((log,"Creating full-text search indexes..."));
	if (strlen(cr_ftindexes_query)==0)
		FTRACE((log,"(no full-test search indexes in the database)...")); 
	else
		if (execute_multiquery(&conn,cr_ftindexes_query,log)!=0) 
			goto imp_error;
	FTRACE((log,"done\n"));


	// processing security information
	if (sec_import==1) {
		// restoring security
		FTRACE((log,"Restoring security information..."));
		if (restore_security(&conn,path,log)!=0) 
			goto imp_error;
		FTRACE((log,"done\n"));
	} else {
		// importing data
		// security information is not imported
        // may be some merge will be required in the future
		//   FTRACE((log,"Updating security information..."));
	    //   if (strlen(upd_sec_query)==0 || 1)
		//      FTRACE((log,"(no additional security information available)")) 
	    //   else
        //      if (execute_multiquery(&conn,upd_sec_query, log)!=0) 
		//	        goto imp_error;
	    //  FTRACE((log,"done\n"));
	}

	FTRACE((log,"Commiting the transaction..."));
	SEcommit(&conn);
	FTRACE((log,"done\n"));

	error_status=0;

imp_error:

	FTRACE((log,"Closing connection..."));
	SEclose(&conn);
	FTRACE((log,"done\n"));
	
//disposing dynamic memory
imp_error_no_conn:
	if (cr_col_query != NULL) free(cr_col_query);
	if (bl_docs_query != NULL) free(bl_docs_query);
	if (upd_sec_query != NULL) free(upd_sec_query);
	if (log!=NULL) fclose(log);
	for (i=0;i<blq.d_size;i++) if (blq.buf[i]!=NULL) {free(blq.buf[i]); blq.buf[i]=NULL;}
	if (blq.buf!=NULL) free(blq.buf);
	if (error_status==1)
		return -1;
	else
		return 0;
}




const char check_db_empty_query[] = "let $docs := document(\"$documents.xml\")/DOCUMENTS/SA_DOCUMENT[ @name != \"db_security_data\"] \
                                     let $cols := document(\"$collections.xml\")/COLLECTION/COLLECTION[ @name != \"$modules\"] \
                                     let $ind  := document(\"$indexes.xml\")/INDEXES/* \
                                     let $sec-users  := document(\"db_security_data\")/db_security_data/users/user[@user_name != \"SYSTEM\"] \
                                     let $sec-roles  := document(\"db_security_data\")/db_security_data/roles/role[@role_name != \"DBA\" and @role_name != \"PUBLIC\"] \
                                     let $all := ($docs, $cols, $ind, $sec-users, $sec-roles) \
                                     return if (empty($all)) then 1 else 0 ";