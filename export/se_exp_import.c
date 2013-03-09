/*
 * File:  se_exp_import.c
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "se_exp_common.h"
#include "se_exp_queries.h"
#include "se_exp.h"
#include "common/u/uhdd.h"

//function checks that the database db_name is empty
// 1. doc("$documents") contains only $db_security_data record
// 2. doc("$collections") is empty
// 3. doc("indexes") is empty
// 4. doc("db_security") has only default structures
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

// returns SEDNA_FEATURE_ENABLED if the file with the given name exists in the import directory
// else returns SEDNA_FEATURE_DISABLED
int check_feature_to_import(const char *path, const char* name, FILE* log) {
	char strbuf[PATH_SIZE];
	FILE *tmp;

    sprintf(strbuf,"%s%s",path,name);

	if ((tmp=fopen(strbuf,"r"))==NULL) {
		return SEDNA_FEATURE_DISABLED;
	} else {
		fclose(tmp);
		return SEDNA_FEATURE_ENABLED;
	}
}



int restore_security(struct SednaConnection *conn, FILE* log) {
  char strbuf[PATH_SIZE];

   sprintf(strbuf,"declare boundary-space preserve; LOAD \"%s.xml\" \"%s\"",DB_SECURITY_DOC,DB_SECURITY_DOC_NAME_TMP);

   if (execute_query(conn, strbuf, NULL, log) != SE_EXP_SUCCEED) {
	   ETRACE((log,"\nERROR: failed to bulkload document with new security data\n"));
	   return -1;
   }

   sprintf(strbuf,"UPDATE replace $p in doc('%s')/db_security_data with doc('%s')/db_security_data",DB_SECURITY_DOC,DB_SECURITY_DOC_NAME_TMP);
   if (SEexecute(conn,strbuf) != SEDNA_UPDATE_SUCCEEDED) {
	   ETRACE((log,"\nERROR: failed to update document with initial security data\n"));
	   return -1;
   }

   sprintf(strbuf,"DROP DOCUMENT '%s'",DB_SECURITY_DOC_NAME_TMP);
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
  int error_status=1;
  size_t i;
  int value;

  int ft_search_feature = SEDNA_FEATURE_DISABLED;
  int security_feature  = SEDNA_FEATURE_DISABLED;

    sprintf(strbuf,"%s%s",path,EXP_LOG_FILE_NAME);

	if ((log=fopen(strbuf,"r"))==NULL) {
		printf("ERROR: permission denied or specified path \"%s\" does not contain Sedna DB export files\n", path);
		goto imp_error_no_conn;
	} else {
		fclose(log);
	}

	sprintf(strbuf,"%s%s",path,IMP_LOG_FILE_NAME);
	if ((log=fopen(strbuf,"wb"))==NULL) {
		printf("ERROR: the specified path \"%s\" is not accesible for writing\n", path);
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

    SEsetConnectionAttr(&conn, SEDNA_ATTR_SESSION_DIRECTORY,path,(int)strlen(path));

    value = SEDNA_LOG_LESS;
    SEsetConnectionAttr(&conn, SEDNA_ATTR_LOG_AMOUNT, (void*)&value, sizeof(int));

    FTRACE((log,"Determining features to export..."));
	ft_search_feature = check_sedna_feature(&conn, check_ft_enabled_query, log);
    security_feature  = check_sedna_feature(&conn, check_sec_enabled_query, log);
	FTRACE((log,"done\n"));


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

	if (check_feature_to_import(path, CHECK_FULL_TEXT_SEARCH, log) == SEDNA_FEATURE_ENABLED) {
		sprintf(strbuf,"%s%s",path,CR_FTINDEXES_QUERY_FILE);
		if ((cr_ftindexes_query = read_query(strbuf))==NULL)
			goto imp_error;
	}

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
		//char path_buf[PATH_BUF_SIZE];
		//uGetCurrentWorkingDirectory(path_buf,PATH_BUF_SIZE-1,NULL);
		//uChangeWorkingDirectory(path, NULL);
		FTRACE((log,"\n"));
        if (split_query(bl_docs_query,&blq)!=0)
			goto imp_error;
        for (i=0;i<blq.d_size;i++) {
			/* workaround to display document_name */
			char *doc_name = blq.buf[i];
			while (*doc_name!=';' && *doc_name!='\0') doc_name++;
			if (*doc_name==';') doc_name++;
			while (*doc_name==' ') doc_name++;
			/* end */
			sprintf(strbuf,"%s%"PRIuMAX".xml",path,(uintmax_t)i+1);
			FTRACE((log," %s...",doc_name));
			if (execute_query(&conn, blq.buf[i], NULL, log) != SE_EXP_SUCCEED)
				goto imp_error;
			FTRACE((log,"done\n"));
		}
		//uChangeWorkingDirectory(path_buf,NULL);
	}
	FTRACE((log,"done\n"));

	FTRACE((log,"Creating indexes..."));
	if (strlen(cr_indexes_query)==0)
		FTRACE((log,"(no indexes in the database)..."));
	else
		if (execute_multiquery(&conn,cr_indexes_query,log)!=0)
			goto imp_error;
	FTRACE((log,"done\n"));

	if (check_feature_to_import(path, CHECK_FULL_TEXT_SEARCH, log) == SEDNA_FEATURE_ENABLED) {
		if (ft_search_feature != SEDNA_FEATURE_ENABLED) {
			ETRACE((log,"WARNING: full-text search feature in target Sedna database is disabled.\n"));
		} else {
			FTRACE((log,"Creating full-text search indexes..."));
			if (strlen(cr_ftindexes_query)==0)
				FTRACE((log,"(no full-test search indexes in the database)..."));
			else
				if (execute_multiquery(&conn,cr_ftindexes_query,log)!=0)
					goto imp_error;
			FTRACE((log,"done\n"));
		}
	}

	// processing security information
	if (sec_import==1 &&
		check_feature_to_import(path, CHECK_SECURITY, log) == SEDNA_FEATURE_ENABLED &&
        check_sedna_feature(&conn, check_sec_enabled_query, log))    // added by MarG
        {
			if (security_feature != SEDNA_FEATURE_ENABLED) {
				ETRACE((log,"WARNING: security feature in target Sedna database is disabled.\n"));
			} else {
				// restoring security
				FTRACE((log,"Restoring security information..."));
				if (restore_security(&conn,log)!=0)
					goto imp_error;
				FTRACE((log,"done\n"));
			}
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
	if(SEcommit(&conn) != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) {
		FTRACE((log, "WARNING: Commit transaction failed.Details:\n%s\n",SEgetLastErrorMsg(&conn)));
		goto imp_error;
    }
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


const char check_db_empty_query[] = "let $docs := doc(\"$documents\")/documents/document[ @name != \"$db_security_data\"] \
                                     let $cols := doc(\"$collections\")/collections/collection[ @name != \"$modules\"] \
                                     let $ind  := doc(\"$indexes\")/indexes/index \
                                     let $all := ($docs, $cols, $ind) \
                                     return if (empty($all)) then 1 else 0 ";

