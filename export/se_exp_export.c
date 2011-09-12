/*
 * File:  se_exp_export.c
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "se_exp_common.h"
#include "se_exp_queries.h"
#include "se_exp.h"


// function exports data from database to the specified directory
int export(const char * path,const char *url,const char *db_name,const char *login,const char *password) {
  struct SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;
  qbuf_t exp_docs = {NULL,0,0};
  qbuf_t load_docs = {NULL,0,0};
  qbuf_t create_colls = {NULL,0,0};
  qbuf_t create_indexes = {NULL,0,0};
  qbuf_t create_ftindexes = {NULL,0,0};
  qbuf_t create_sec = {NULL,0,0};
  int res,value;
  size_t i;
  FILE *log,*f;
  char strbuf[PATH_SIZE];
  //int error_status=1;
  int export_status = SE_EXP_FATAL_ERROR; //the worst value in the beginning of the process
  
  int ft_search_feature = SEDNA_FEATURE_DISABLED;
  int security_feature  = SEDNA_FEATURE_DISABLED;

    sprintf(strbuf,"%s%s",path,EXP_LOG_FILE_NAME);

	if ((log=fopen(strbuf,"wb"))==NULL) {
		printf("ERROR: path '%s' is not accessible for writing\n",path);
		goto exp_error_no_conn;
	}
	
	FTRACE((log,"Connecting to Sedna..."));
	if(SEconnect(&conn, url, db_name, login, password)!= SEDNA_SESSION_OPEN) {
		ETRACE((log,"ERROR: can't connect to Sedna XML DB\n%s\n", SEgetLastErrorMsg(&conn)));
		goto exp_error_no_conn;
	}
	FTRACE((log,"done\n"));
	
	value = SEDNA_AUTOCOMMIT_OFF;
    SEsetConnectionAttr(&conn, SEDNA_ATTR_AUTOCOMMIT, (void*)&value, sizeof(int));

	
    FTRACE((log,"Determining features to export..."));
	ft_search_feature = check_sedna_feature(&conn, check_ft_enabled_query, log);
    security_feature  = check_sedna_feature(&conn, check_sec_enabled_query, log);
	FTRACE((log,"done\n"));

	
    FTRACE((log,"Starting transaction..."));
	if ((res = SEbegin(&conn))!= SEDNA_BEGIN_TRANSACTION_SUCCEEDED) {
		ETRACE((log,"ERROR: failed to begin transaction\n"));
		goto exp_error;
	}
	FTRACE((log,"done\n"));
			
	FTRACE((log,"Constructing export documents script"));
	if ((export_status = fill_qbuf(&conn,&exp_docs, exp_docs_query, log))!=SE_EXP_SUCCEED) {
		goto exp_error;
	}
	FTRACE((log,"...done (%"PRIuMAX" statements)\n", (uintmax_t)exp_docs.d_size));

	FTRACE((log,"Constructing load documents script"));
	if ((export_status = fill_qbuf(&conn,&load_docs, load_docs_query, log))!=SE_EXP_SUCCEED) {
		goto exp_error;
	}
	FTRACE((log,"...done (%"PRIuMAX" statements)\n", (uintmax_t)load_docs.d_size));

	FTRACE((log,"Constructing create collections script"));
	if ((export_status = fill_qbuf(&conn,&create_colls, create_colls_query, log))!=SE_EXP_SUCCEED) {
		goto exp_error;
	}
	FTRACE((log,"...done (%"PRIuMAX" statements)\n", (uintmax_t)create_colls.d_size));

	FTRACE((log,"Constructing create indexes script"));
	if ((export_status = fill_qbuf(&conn,&create_indexes, create_indexes_query, log))!=SE_EXP_SUCCEED) {
		goto exp_error;
	}
	FTRACE((log,"...done (%"PRIuMAX" statements)\n", (uintmax_t)create_indexes.d_size));

	if (ft_search_feature == SEDNA_FEATURE_ENABLED) {
		FTRACE((log,"Constructing create full-text search indexes script"));
		if ((export_status = fill_qbuf(&conn,&create_ftindexes, create_ftindexes_query, log))!=SE_EXP_SUCCEED) {
			goto exp_error;
		}
		FTRACE((log,"...done (%"PRIuMAX" statements)\n", (uintmax_t)create_ftindexes.d_size));
	}

	if (security_feature == SEDNA_FEATURE_ENABLED) {
		FTRACE((log,"Constructing export security script"));
		if ((export_status = fill_qbuf(&conn,&create_sec, create_sec_query, log))!=SE_EXP_SUCCEED) {
			goto exp_error;
		}
		FTRACE((log,"...done (%"PRIuMAX" statements)\n", (uintmax_t)create_sec.d_size));
	}


	for (i=0;i<exp_docs.d_size;i++) {
		/* workaround to display document_name */
		char *doc_name = exp_docs.buf[i];
		while (*doc_name!=';' && *doc_name!='\0') doc_name++;
		if (*doc_name==';') doc_name++;
		while (*doc_name==' ') doc_name++;
		/* end */
		FTRACE((log,"Exporting document %"PRIuMAX" of %"PRIuMAX" [%s]...",(uintmax_t)(i+1),(uintmax_t)exp_docs.d_size,doc_name));
		sprintf(strbuf,"%s%"PRIuMAX".xml",path,(uintmax_t)(i+1));
		if ((f=fopen(strbuf,"wb"))==NULL) {
			ETRACE((log,"ERROR: can't write to file %s\n",strbuf));
			export_status = SE_EXP_FATAL_ERROR;
			goto exp_error;
		}
		if ((export_status = execute_query(&conn,exp_docs.buf[i],f,log))!=SE_EXP_SUCCEED) 
			goto exp_error;
		fclose(f);
		FTRACE((log,"done\n"));
	}

	if (security_feature == SEDNA_FEATURE_ENABLED) {
	FTRACE((log,"Exporting security data..."));
		sprintf(strbuf,"%s%s.xml",path,DB_SECURITY_DOC);
		if ((f=fopen(strbuf,"wb"))==NULL) {
			ETRACE((log,"ERROR: can't write to file %s\n",strbuf));
			export_status = SE_EXP_FATAL_ERROR;
			goto exp_error;
		}

		sprintf(strbuf,"doc('%s')",DB_SECURITY_DOC);
		if ((export_status = execute_query(&conn,strbuf,f,log))!=SE_EXP_SUCCEED) 
			goto exp_error;
		fclose(f);
		FTRACE((log,"done\n"));
	}

	FTRACE((log,"Writing XQuery scripts..."));
	sprintf(strbuf,"%s%s",path,CR_COL_QUERY_FILE);
	write_xquery_script(&create_colls,strbuf);

	sprintf(strbuf,"%s%s",path,LOAD_DOCS_QUERY_FILE);
	write_xquery_script(&load_docs,strbuf);

	sprintf(strbuf,"%s%s",path,CR_INDEXES_QUERY_FILE);
	write_xquery_script(&create_indexes,strbuf);

	if (ft_search_feature == SEDNA_FEATURE_ENABLED) {
	sprintf(strbuf,"%s%s",path,CR_FTINDEXES_QUERY_FILE);
	write_xquery_script(&create_ftindexes,strbuf);
	}

	/*sprintf(strbuf,"%s%s",path,CR_SEC_QUERY_FILE);
	write_xquery_script(&create_sec,strbuf); */
	FTRACE((log,"done\n"));

	// we doesn't need to analyze SEcommit status 
	FTRACE((log,"Commiting the transaction..."));
	if(SEcommit(&conn) != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) {
		FTRACE((log, "WARNING: Commit transaction failed.Details:\n%s\n",SEgetLastErrorMsg(&conn)));
		goto exp_error;
    }
	FTRACE((log,"done\n"));


exp_error:

	FTRACE((log,"Closing connection..."));
	SEclose(&conn);
	FTRACE((log,"done\n"));
	

//disposing dynamic memory
exp_error_no_conn:
	if (log!=NULL) fclose(log);
	for (i=0;i<exp_docs.d_size;i++) if (exp_docs.buf[i]!=NULL) {free(exp_docs.buf[i]); exp_docs.buf[i]=NULL;}
	for (i=0;i<load_docs.d_size;i++) if (load_docs.buf[i]!=NULL) {free(load_docs.buf[i]); load_docs.buf[i]=NULL;}
	for (i=0;i<create_colls.d_size;i++) if (create_colls.buf[i]!=NULL) {free(create_colls.buf[i]); create_colls.buf[i]=NULL;}
	for (i=0;i<create_sec.d_size;i++) if (create_sec.buf[i]!=NULL) {free(create_sec.buf[i]); create_sec.buf[i]=NULL;}
	if (exp_docs.buf!=NULL) free(exp_docs.buf);
	if (load_docs.buf!=NULL) free(load_docs.buf);
	if (create_colls.buf!=NULL) free(create_colls.buf);
	if (create_sec.buf!=NULL) free(create_sec.buf);
	return export_status;
}


const char check_ft_enabled_query[] = "doc(\"$ftindexes\")";
const char check_sec_enabled_query[] = "doc(\"$db_security_data\")";



const char load_docs_query[] = "declare namespace se='http://www.modis.ispras.ru/sedna'; \
                                declare option se:output \"indent=no\"; \
								for $i at $j in doc(\"$documents\")//document[@name!= \"$db_security_data\"] \
								let $col := $i/parent::collection \
								return \
								   if (empty($col)) \
									 then fn:concat(\"declare boundary-space preserve; LOAD \"\"\",$j,\".xml\"\" \"\"\",$i/@name,\"\"\"\") \
									 else fn:concat(\"declare boundary-space preserve; LOAD \"\"\",$j,\".xml\"\" \"\"\",$i/@name,\"\"\" \"\"\",$col/@name,\"\"\"\")";

const char exp_docs_query[] =  "declare namespace se='http://www.modis.ispras.ru/sedna'; \
                                declare option se:output \"indent=no\"; \
								for $i at $j in doc(\"$documents\")//document[@name!= \"$db_security_data\"] \
								let $col := $i/parent::collection \
								return \
								  if (empty($col)) \
									 then fn:concat(\"declare namespace se='http://www.modis.ispras.ru/sedna'; declare option se:output \"\"indent=no\"\"; doc(\"\"\",$i/@name,\"\"\")\") \
									 else fn:concat(\"declare namespace se='http://www.modis.ispras.ru/sedna'; declare option se:output \"\"indent=no\"\"; doc(\"\"\",$i/@name,\"\"\",\"\"\",$col/@name,\"\"\")\")";

const char create_colls_query[] = "declare namespace se='http://www.modis.ispras.ru/sedna'; \
                                   declare option se:output \"indent=no\"; \
                                   for $i in doc(\"$collections\")/collections/collection[@name != \"$modules\"] \
								   return fn:concat(\"CREATE COLLECTION \"\"\",$i/@name,\"\"\"\")";

const char create_sec_query[] = "()";

const char create_indexes_query[] = "for $i in doc(\"$indexes\")/indexes/index \
                                     return \
                                        fn:concat(\"CREATE INDEX \"\"\", $i/@name, \"\"\" ON \", \
                                        fn:concat($i/@object_type,\"(\"\"\",$i/@object_name,\"\"\")\", \"/\", $i/@on_path), \
                                        \" BY \", \
									    $i/@by_path, \
									    \" AS \",  $i/@as_type, \
										\" USING \"\"\", $i/@backend, \"\"\"\")";


const char create_ftindexes_query[] = " for $i in doc(\"$ftindexes\")/ftindexes/ftindex \
									    let $cust := <dummy>{(for $t in $i/template[position() < last()] \
															  return fn:concat(\"(\"\"\",$t/@element_name,\"\"\" , \"\"\",$t/@ft_type,\"\"\")\",\",\"), \
															  for $t in $i/template[position() = last()] \
															  return fn:concat(\"(\"\"\",$t/@element_name,\"\"\" , \"\"\",$t/@ft_type,\"\"\")\"))}</dummy>/text() \
										return  \
										  fn:concat(\"CREATE FULL-TEXT INDEX \"\"\", \
													$i/@name, \
													\"\"\" ON \", \
													$i/@object_type, \"(\"\"\", $i/@object_name, \"\"\")\", \"/\", $i/@on_path, \
													\" TYPE \"\"\", $i/@ft_type, \"\"\"\", \
													\" WITH OPTIONS \"\"\", $i/@options, \"\"\"\", \
													if (empty($cust)) then \"\" else fn:concat(\" (\",$cust,\")\")) ";
