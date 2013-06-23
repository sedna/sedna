/*
 * File: se_exp_common.c
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "se_exp_common.h"
#include "se_exp.h"
#include <string.h>

// function adds string to rp_buf structure
int rp_add_data(str_buf_t *rp_buf, const char *data) {
    size_t d_len = strlen(data);
    size_t new_buf_size;

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
    size_t new_buf_size;

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
    size_t real_read;
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



// function checks whether the sedna error was fatal or not
int getSednaErrorStatus(const char *errorMessage) {
    int i;
    char status[7];
    for (i=0;i<6;i++) status[i]=errorMessage[i];
    status[6]='\0';

    if (!strcmp(status,"SE3006") || !strcmp(status,"SE3007"))
        return SE_EXP_FATAL_ERROR;
    else
        return SE_EXP_RB_ERROR;
}



int check_sedna_feature(struct SednaConnection *conn, const char *query, FILE* log) {
    int res;
    char needBeginCommit = 1;

    if (SEtransactionStatus(conn) != SEDNA_TRANSACTION_ACTIVE) {
        if ((res = SEbegin(conn))!= SEDNA_BEGIN_TRANSACTION_SUCCEEDED) {
            ETRACE((log,"ERROR: failed to begin transaction\n"));
        }
    }else
        needBeginCommit = 0;

    if ((res = SEexecute(conn,query))!= SEDNA_QUERY_SUCCEEDED)
        return SEDNA_FEATURE_DISABLED;
    else {
        if (needBeginCommit)
            if(SEcommit(conn) != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) {
                FTRACE((log, "WARNING: Commit transaction failed.Details:\n%s\n",SEgetLastErrorMsg(conn)));
                return SEDNA_FEATURE_DISABLED; // TODO - need to review
            }
        return SEDNA_FEATURE_ENABLED;
    }
}




// functions executes retrieve query, allocates buffer and returns it via result reference
// function returns error status
int execute_retrieve_query(char** result, struct SednaConnection *conn, const char *query, FILE* log) {
    str_buf_t res_buf = {NULL, 0 , 0};
    int res, bytes_read;
    char buf[RESULT_PORTION_SIZE+1]="";

    *result = NULL;

    res = SEexecute(conn,query);
    if ((res != SEDNA_QUERY_SUCCEEDED) && (res != SEDNA_UPDATE_SUCCEEDED) && (res != SEDNA_BULK_LOAD_SUCCEEDED)) {
        ETRACE((log, "\nERROR: failed to execute query:\n\n%s\n\nDetails:\n\n%s\n\n",query,SEgetLastErrorMsg(conn)));
        return getSednaErrorStatus(SEgetLastErrorMsg(conn));
    }

    if (res == SEDNA_QUERY_SUCCEEDED) {
        res = SEnext(conn);
        while ((res != SEDNA_RESULT_END)&&(res != SEDNA_ERROR)&&(res != SEDNA_NO_ITEM)) {
            do {
                bytes_read = SEgetData(conn, buf, RESULT_PORTION_SIZE);
                buf[bytes_read] = '\0';
                if (rp_add_data(&res_buf,buf) != 0) return SE_EXP_DEV_ERROR;
            } while (bytes_read == RESULT_PORTION_SIZE);
            res = SEnext(conn);
        }
        if (res==SEDNA_ERROR) {
            ETRACE((log,"\nERROR: failed to retrieve query results\nQuery was:\n\n%s\n\n",query));
            return getSednaErrorStatus(SEgetLastErrorMsg(conn));
        }
    } else  {
        ETRACE((log,"ERROR: update query was not expected\n"));
        return SE_EXP_DEV_ERROR;
    }
    *result = res_buf.buf;
    return SE_EXP_SUCCEED;
}


// function executes query in sedna and outputs the result to file
// if the file reference is NULL then the result is not retrieved
// function returns error status
int execute_query(struct SednaConnection *conn, const char *query, FILE* f, FILE* log) {
    int res, bytes_read;
    char buf[RESULT_PORTION_SIZE+1]="";

    res = SEexecute(conn,query);
    if ((res != SEDNA_QUERY_SUCCEEDED) && (res != SEDNA_UPDATE_SUCCEEDED) && (res != SEDNA_BULK_LOAD_SUCCEEDED)) {
        ETRACE((log, "\nERROR: failed to execute query:\n\n%s\n\nDetails:\n\n%s\n\n",query,SEgetLastErrorMsg(conn)));
        return getSednaErrorStatus(SEgetLastErrorMsg(conn));
    }

    if (f==NULL) return SE_EXP_SUCCEED;

    if (res == SEDNA_QUERY_SUCCEEDED) {
        res = SEnext(conn);
        while((res != SEDNA_RESULT_END)&&(res != SEDNA_ERROR)) {
            do {
                bytes_read = SEgetData(conn, buf, RESULT_PORTION_SIZE);
                if(bytes_read == SEDNA_ERROR) {
                    ETRACE((log,"Failed to get result from server\n%s\n", SEgetLastErrorMsg(conn)));
                    return getSednaErrorStatus(SEgetLastErrorMsg(conn));
                }
                buf[bytes_read] = '\0';
                fwrite(buf,1,bytes_read,f);
            } while (bytes_read > 0);
            res = SEnext(conn);
        }
        if (res==SEDNA_ERROR) {
            ETRACE((log,"\nERROR: failed to retrieve query results\nQuery was:\n\n%s\n\n",query));
            return getSednaErrorStatus(SEgetLastErrorMsg(conn));
        }
    }
    return SE_EXP_SUCCEED;
}


// function executes the query with Sedna.
// the result is pushed to qbuf array
// function returns error status
int fill_qbuf(struct SednaConnection *conn, qbuf_t* qbuf, const char *query, FILE* log) {
    int res, bytes_read;
    char buf[RESULT_PORTION_SIZE+1]="";

    res = SEexecute(conn,query);
    if ((res != SEDNA_QUERY_SUCCEEDED) && (res != SEDNA_UPDATE_SUCCEEDED) && (res != SEDNA_BULK_LOAD_SUCCEEDED)) {
        ETRACE((log,"\nERROR: failed to execute query:\n\n%s\n\nDetails:\n\n%s\n\n",query,SEgetLastErrorMsg(conn)));
        return getSednaErrorStatus(SEgetLastErrorMsg(conn));
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
            ETRACE((log,"\nERROR: failed to retrieve query results\nQuery was:\n\n%s\n\n",query));
            return getSednaErrorStatus(SEgetLastErrorMsg(conn));
        }
    } else  {
        ETRACE((log,"ERROR: update query is not expected\n"));
        return SE_EXP_DEV_ERROR;
    }
    return SE_EXP_SUCCEED;
}


int write_xquery_script(qbuf_t *qbuf,const char * filename) {
    FILE* file;
    size_t i;
    if ((file=fopen(filename,"wb"))==NULL) {
        printf("\nERROR: failed to open file %s for writing",filename);
        return -1;
    }

    if (qbuf->d_size > 0)
    {
        for (i = 0; i < (qbuf->d_size - 1); i++)
            fprintf(file,"%s%s",qbuf->buf[i],DELIMITER);

        fprintf(file,"%s",qbuf->buf[qbuf->d_size - 1]);
    }

    fclose(file);
    return 0;
}


// function executes update sequence
int execute_multiquery(struct SednaConnection *conn, char *query, FILE* log) {
    qbuf_t mq = {NULL,0,0};
    size_t i;
    if (split_query(query,&mq)!=0)
        return SE_EXP_DEV_ERROR;
    for (i=0;i<mq.d_size;i++) {
        int status;
        if ((status=execute_query(conn,mq.buf[i],NULL,log))!=SE_EXP_SUCCEED)
            return status;
    }
    return SE_EXP_SUCCEED;
}
