#include "se_exp_common.h"


// function adds string to rp_buf structure
int rp_add_data(str_buf_t *rp_buf, const char *data) {
  int d_len = strlen(data);
  int new_buf_size;
 
   while (rp_buf->size < (rp_buf->d_size + d_len + 1) ) {
		new_buf_size = (rp_buf->size == 0) ? STR_BUF_INIT_SIZE : rp_buf->size*2;
		if ((rp_buf->buf = (char *) realloc(rp_buf->buf, new_buf_size*sizeof(char) )) == NULL) {
			return -1;
		} else {
			rp_buf->size = new_buf_size;
		}
	}
	strcpy((rp_buf->buf)+rp_buf->d_size,data);
	rp_buf->d_size += d_len;
	return 0;
}


// function adds string to &qbuf structure
// if required the realloc is performed
// new_buf_size is in items, not bytes! 
int qadd(qbuf_t *qbuf, const char *q) {
  int new_buf_size;
	 
	while ((qbuf->d_size + 2) > qbuf->size) {
		new_buf_size = (qbuf->size == 0) ? Q_BUF_INIT_SIZE : qbuf->size*2;
		if ((qbuf->buf = (char **) realloc(qbuf->buf, new_buf_size*sizeof(char*))) == NULL) {
			return -1;
		} else {
			qbuf->size = new_buf_size;
		}
	}
  
	if ((qbuf->buf[qbuf->d_size]=(char*) malloc(sizeof(char)*(strlen(q)+1)))==NULL) {
		return -1;
	} else {
		strcpy(qbuf->buf[qbuf->d_size],q);
		qbuf->buf[qbuf->d_size][strlen(q)]='\0';
		qbuf->d_size++;
		qbuf->buf[qbuf->d_size] = NULL;
	}

	return 0;
}


int split_query(char *query,qbuf_t *qbuf) {
 char *pdest=NULL;
 char *cur=query;
   while ((pdest=strstr(cur,DELIMITER))!=NULL) {
	  *pdest='\0';
	  qadd(qbuf,cur);
	  cur=pdest+strlen(DELIMITER);
   } 
   if (qadd(qbuf,cur)!=0) return -1;
   return 0;
}



// function reads the requested file and returns the buffer with the query 
char* read_query(char *filename) {
  FILE *f=NULL;
  int real_read;
  str_buf_t file_buf = {NULL, 0 , 0};
  char fr_buf[RF_PORTION_SIZE+1];

	if ((f = fopen(filename, "r")) == NULL) {
		return NULL; 
	}
	do {
		real_read = fread(fr_buf, sizeof(char), RF_PORTION_SIZE, f);
		fr_buf[real_read]='\0';
		if (rp_add_data(&file_buf,fr_buf) != 0) return NULL;
	} while (real_read == RF_PORTION_SIZE); 
    fclose(f);	
	return file_buf.buf;
}




char* execute_query_str(struct SednaConnection *conn, const char *query, FILE* log) {
  str_buf_t res_buf = {NULL, 0 , 0};
  int res, bytes_read;
  char buf[RESULT_PORTION_SIZE+1]="";

   res = SEexecute(conn,query);
   if ((res != SEDNA_QUERY_SUCCEEDED) && (res != SEDNA_UPDATE_SUCCEEDED) && (res != SEDNA_BULK_LOAD_SUCCEEDED)) {
		ETRACE((log, "ERROR: failed to execute query:\n\n%s\n\nDetails:\n\n%s\n\n",query,SEgetLastErrorMsg(conn)))
	    return NULL;
   }
   
   if (res == SEDNA_QUERY_SUCCEEDED) {
      res = SEnext(conn);
	  while ((res != SEDNA_RESULT_END)&&(res != SEDNA_ERROR)&&(res != SEDNA_NO_ITEM)) {
 	 	 do {
		   bytes_read = SEgetData(conn, buf, RESULT_PORTION_SIZE);
    	   buf[bytes_read] = '\0';
		   if (rp_add_data(&res_buf,buf) != 0) return NULL;
		 } while (bytes_read == RESULT_PORTION_SIZE);
		 res = SEnext(conn);
      }
	  if (res==SEDNA_ERROR) {
		 ETRACE((log,"ERROR: failed to retrieve query results\nQuery was:\n\n%s\n\n",query))
		 return NULL;
      }
   } else  {
	  ETRACE((log,"ERROR: update query was not expected\n"))
	  return NULL;
   }
   return res_buf.buf;
}





int execute_query(struct SednaConnection *conn, const char *query, FILE* f, FILE* log) {
  int res, bytes_read;
  char buf[RESULT_PORTION_SIZE+1]="";

   res = SEexecute(conn,query);
   if ((res != SEDNA_QUERY_SUCCEEDED) && (res != SEDNA_UPDATE_SUCCEEDED) && (res != SEDNA_BULK_LOAD_SUCCEEDED)) {
		ETRACE((log, "ERROR: failed to execute query:\n\n%s\n\nDetails:\n\n%s\n\n",query,SEgetLastErrorMsg(conn)))
	    return -1;
   }
   
   if (f==NULL) return 0;

   if (res == SEDNA_QUERY_SUCCEEDED) {
      res = SEnext(conn);
	  while ((res != SEDNA_RESULT_END)&&(res != SEDNA_ERROR)&&(res != SEDNA_NO_ITEM)) {
 	 	 do {
		   bytes_read = SEgetData(conn, buf, RESULT_PORTION_SIZE);
    	   buf[bytes_read] = '\0';
		   fprintf(f, "%s", buf);
		 } while (bytes_read == RESULT_PORTION_SIZE);
		 res = SEnext(conn);
      }
	  if (res==SEDNA_ERROR) {
		 ETRACE((log,"ERROR: failed to retrieve query results\nQuery was:\n\n%s\n\n",query))
		 return -1;
      }
   } else  {
	  ETRACE((log,"ERROR: update query was not expected\n"))
	  return -1;
   }
   return 0;
}






// function executes the query with Sedna.
// the result is pushed to qbuf array
int fill_qbuf(struct SednaConnection *conn, qbuf_t* qbuf, const char *query, FILE* log) {
  int res, bytes_read;
  char buf[RESULT_PORTION_SIZE+1]="";

	res = SEexecute(conn,query);
	if ((res != SEDNA_QUERY_SUCCEEDED) && (res != SEDNA_UPDATE_SUCCEEDED) && (res != SEDNA_BULK_LOAD_SUCCEEDED)) {
		ETRACE((log,"ERROR: failed to execute query:\n\n%s\n\nDetails:\n\n%s\n\n",query,SEgetLastErrorMsg(conn)))
		return -1;
	}

	// iterating over the result sequece and retrieve the result data 
	if (res == SEDNA_QUERY_SUCCEEDED) {
			res = SEnext(conn);
			while ((res != SEDNA_RESULT_END)&&(res != SEDNA_ERROR)&&(res != SEDNA_NO_ITEM)) {
				do {
					  bytes_read = SEgetData(conn, buf, RESULT_PORTION_SIZE);
    				  buf[bytes_read] = '\0';
					  if (bytes_read!=0) qadd(qbuf,buf);
			    } while (bytes_read == RESULT_PORTION_SIZE);
			    res = SEnext(conn);
		    }

			if (res==SEDNA_ERROR) {
				ETRACE((log,"ERROR: failed to retrieve query results\nQuery was:\n\n%s\n\n",query))
				return -1;
		    }
	} else  {
			ETRACE((log,"ERROR: update query is not expected\n"))
			return -1;
	}
	return 0;
}


int write_xquery_script(qbuf_t *qbuf,const char * filename) {
  FILE* file;
  int i;
	if ((file=fopen(filename,"w"))==NULL) {
		  printf("ERROR: failed to open file %s for writing",filename);
		  return -1;
	}
	for (i=0;i<(qbuf->d_size-1);i++) 
		  fprintf(file,"%s\n\\",qbuf->buf[i]);
	if (qbuf->d_size > 0) fprintf(file,"%s",qbuf->buf[qbuf->d_size-1]);
	fclose(file);
	return 0;
}




int execute_multiquery(struct SednaConnection *conn, char *query, FILE* log) {
  qbuf_t mq = {NULL,0,0};
  int i;
  if (split_query(query,&mq)!=0) return -1;
  for (i=0;i<mq.d_size;i++) 
	  if (execute_query(conn,mq.buf[i],NULL,log)!=0) return -1;
  return 0;
}





int bulkload_xml(struct SednaConnection *conn,const char *filename,const char *docname, FILE* log) {
  FILE *f=NULL;
  int real_read;
  char fr_buf[RF_PORTION_SIZE+1];

	if ((f = fopen(filename, "r")) == NULL) {
		ETRACE((log,"\nERROR: can't open file '%s' for bulkload\n",filename))
		return -1; 
	}
	do {
		real_read = fread(fr_buf, sizeof(char), RF_PORTION_SIZE, f);
		fr_buf[real_read]='\0';
		if (SEloadData(conn,fr_buf,real_read,docname,"")!=SEDNA_DATA_CHUNK_LOADED) {
			ETRACE((log,"\n\nERROR: failed while bulk load file '%s'\n%s\n",filename,SEgetLastErrorMsg(conn)))
			return -1;
		}
	} while (real_read == RF_PORTION_SIZE); 
	
	if (SEendLoadData(conn)!=SEDNA_BULK_LOAD_SUCCEEDED) {
			ETRACE((log,"\n\nERROR: failed while bulk load file '%s'\n%s\n",filename,SEgetLastErrorMsg(conn)))
			return -1;
	}
    fclose(f);	
	return 0;
}




