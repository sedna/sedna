/*
 * File:  exec_output.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _EXEC_OUTPUT_H
#define _EXEC_OUTPUT_H

#include <iostream>
#include <strstream>

#include "sedna.h"

#include "usocket.h"
#include "sp.h"
#include "uutils.h"
#include "base.h"


enum executor_ostream_t {eot_std, eot_sock, eot_str, eot_null};
	
	

class se_ostream
{
public:
    virtual ~se_ostream() {}

    virtual se_ostream& operator<<(se_ostream& (*pf)(se_ostream&)) = 0;
    virtual se_ostream& operator<<(const char *s)                  = 0;
    virtual se_ostream& operator<<(char c)                         = 0;
    virtual se_ostream& operator<<(bool n)                         = 0;
    virtual se_ostream& operator<<(short n)                        = 0;
    virtual se_ostream& operator<<(unsigned short n)               = 0;
    virtual se_ostream& operator<<(int n)                          = 0;
    virtual se_ostream& operator<<(unsigned int n)                 = 0;
    virtual se_ostream& operator<<(long n)                         = 0;
    virtual se_ostream& operator<<(unsigned long n)                = 0;
    virtual se_ostream& operator<<(float n)	                       = 0;
    virtual se_ostream& operator<<(double n)                       = 0;
    virtual se_ostream& operator<<(long double n)                  = 0;
    virtual se_ostream& operator<<(void * n)                       = 0;
    virtual se_ostream& put(char c)                                = 0;
    virtual se_ostream& write(char *s, int n)                      = 0;
    virtual se_ostream& writextext(char *s, int n, bool sxmlescape=false);
	virtual se_ostream& writeattribute(char *s, int n);
    virtual se_ostream& flush()                                    = 0;
    virtual void end_of_data(bool res)                             = 0;
    virtual void endline()                                         = 0;
    virtual void error(const char*)                                = 0;
	virtual se_ostream& operator<<(__int64 n);
};

class se_stdlib_ostream : public se_ostream
{
protected:
    std::ostream &o_str;
public:
    se_stdlib_ostream(std::ostream &_o_str_) : o_str(_o_str_) {}
    virtual ~se_stdlib_ostream() {}

    virtual se_ostream& operator<<(se_stdlib_ostream& (*pf)(se_stdlib_ostream&)) { (*pf)(*this); return *this; }
    virtual se_ostream& operator<<(se_ostream& (*pf)(se_ostream&))	             { (*pf)(*this); return *this; }
    virtual se_ostream& operator<<(const char *s)                                { o_str << s; return *this; }
    virtual se_ostream& operator<<(char c)                                       { o_str << c; return *this; }
    virtual se_ostream& operator<<(bool n)                                       { o_str << n; return *this; }
    virtual se_ostream& operator<<(short n)                                      { o_str << n; return *this; }
    virtual se_ostream& operator<<(unsigned short n)                             { o_str << n; return *this; }
    virtual se_ostream& operator<<(int n)                                        { o_str << n; return *this; }
    virtual se_ostream& operator<<(unsigned int n)                               { o_str << n; return *this; }
    virtual se_ostream& operator<<(long n)                                       { o_str << n; return *this; }
    virtual se_ostream& operator<<(unsigned long n)                              { o_str << n; return *this; }
    virtual se_ostream& operator<<(float n)                                      { o_str << n; return *this; }
    virtual se_ostream& operator<<(double n)                                     { o_str << n; return *this; }
    virtual se_ostream& operator<<(long double n)                                { o_str << n; return *this; }
    virtual se_ostream& operator<<(void * n)                                     { o_str << n; return *this; }
    virtual se_ostream& put(char c)                                              { o_str.put(c); return *this; }
    virtual se_ostream& write(char *s, int n)                                    { o_str.write(s, n); return *this; }
	virtual se_ostream& flush()                                                  { o_str.flush(); return *this; }
    virtual void end_of_data(bool res)                                           { o_str << std::endl; }
    virtual void endline()                                                       { o_str << std::endl; }
    virtual void error(const char* str)                                          { o_str << str << std::endl; }
};

class se_nullostream : public se_ostream
{
public:
    se_nullostream() {}
    virtual ~se_nullostream() {}

    virtual se_ostream& operator<<(se_nullostream& (*pf)(se_nullostream&)) { return *this; }
    virtual se_ostream& operator<<(se_ostream& (*pf)(se_ostream&))         { return *this; }
    virtual se_ostream& operator<<(const char *s)                          { return *this; }
    virtual se_ostream& operator<<(char c)                                 { return *this; }
    virtual se_ostream& operator<<(bool n)                                 { return *this; }
    virtual se_ostream& operator<<(short n)                                { return *this; }
    virtual se_ostream& operator<<(unsigned short n)                       { return *this; }
    virtual se_ostream& operator<<(int n)                                  { return *this; }
    virtual se_ostream& operator<<(unsigned int n)                         { return *this; }
    virtual se_ostream& operator<<(long n)                                 { return *this; }
    virtual se_ostream& operator<<(unsigned long n)                        { return *this; }
    virtual se_ostream& operator<<(float n)                                { return *this; }
    virtual se_ostream& operator<<(double n)                               { return *this; }
    virtual se_ostream& operator<<(long double n)                          { return *this; }
    virtual se_ostream& operator<<(void * n)                               { return *this; }
    virtual se_ostream& put(char c)                                        { return *this; }
    virtual se_ostream& write(char *s, int n)                              { return *this; }
	virtual se_ostream& flush()                                            { return *this; }
    virtual void end_of_data(bool res)                                     { ; }
    virtual void endline()                                                 { ; }
    virtual void error(const char* str)                                    { ; }
};


class se_socketostream : public se_ostream
{
private:
    USOCKET out_socket;

    msg_struct res_msg;

public:
    se_socketostream(USOCKET _out_socket_) : out_socket(_out_socket_) 
    {  
        res_msg.body[0] = 0;           // in this version string format is always 0
        res_msg.length = 5;   // the body contains string format - 1 byte, string length - 4 bytes and a string
    }
    virtual ~se_socketostream() {}

    virtual se_ostream& operator<<(se_socketostream& (*pf)(se_socketostream&))	{ (*pf)(*this); return *this; }
    virtual se_ostream& operator<<(se_ostream& (*pf)(se_ostream&))	{ (*pf)(*this); return *this; }
    virtual se_ostream& operator<<(const char *s)	
	{ 
    	res_msg.instruction = se_ItemPart; //ItemPart message
		int len = strlen(s);
        
		if((res_msg.length + len) > (SE_SOCKET_MSG_BUF_SIZE-5))
	    {
	    	flush();	
	    	int celoe = len/(SE_SOCKET_MSG_BUF_SIZE-5);
	    	int ost;
	    	if(celoe==0) ost = len; else ost = len%(SE_SOCKET_MSG_BUF_SIZE-5);
	    	
	    	for (int i=0;i<celoe;i++)
	    	{
	    		res_msg.length = SE_SOCKET_MSG_BUF_SIZE;
	            // the body contains string format - 1 byte, string length - 4 bytes and a string
                // construct the buf for body.	   
                int2net_int(SE_SOCKET_MSG_BUF_SIZE-5, res_msg.body+1);
                memcpy(res_msg.body+5, s+(SE_SOCKET_MSG_BUF_SIZE-5)*i, SE_SOCKET_MSG_BUF_SIZE-5);

         		if(sp_send_msg(out_socket, &res_msg) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3006);
        	} //end for

         	res_msg.length = ost+5;
         	int2net_int(ost, res_msg.body+1);
         	memcpy(res_msg.body+5, s+(SE_SOCKET_MSG_BUF_SIZE-5)*celoe, ost);
         }
         else
         {
 	         memcpy(res_msg.body+res_msg.length, s, strlen(s));
         	 res_msg.length += strlen(s);
             int2net_int(res_msg.length-5, res_msg.body+1);
         }
   	     return *this; 
	}
    virtual se_ostream& operator<<(char c){	flush();
    										res_msg.body[res_msg.length]=c;
                                        	res_msg.length += 1;  // 5+1
                                    		return *this; }
                                    		
    virtual se_ostream& operator<<(bool n)	{ flush();
    										  (n)? res_msg.body[res_msg.length]='1':res_msg.body[res_msg.length]='0';
                                              res_msg.length += 1;  // 5+1
                                              return *this; }
                                              
    virtual se_ostream& operator<<(short n)	{ flush();
    										  u_ltoa((long)n,res_msg.body+res_msg.length,10); 
                                              res_msg.length += strlen(res_msg.body+5);
                                              return *this; }
                                              
    virtual se_ostream& operator<<(unsigned short n){flush();
    												u_ultoa((unsigned long)n,res_msg.body+res_msg.length,10);
                                                    res_msg.length += strlen(res_msg.body+5);
                                                    return *this; }
                                                    
    virtual se_ostream& operator<<(int n)		{ flush();	
    											  u_ltoa((long)n,res_msg.body+res_msg.length,10);
                                                  res_msg.length += strlen(res_msg.body+5);
                                                  return *this; }
                                                   
    virtual se_ostream& operator<<(unsigned int n)	{ flush();
                                                      u_ultoa((long)n,res_msg.body+res_msg.length,10); 
                                                      res_msg.length += strlen(res_msg.body+5);
                                                      return *this; }
                                                      
    virtual se_ostream& operator<<(long n)		{ flush();
                                                  u_ltoa((long)n,res_msg.body+res_msg.length,10); 
                                                  res_msg.length += strlen(res_msg.body+5);
                                                  return *this; }
                                                    
    virtual se_ostream& operator<<(unsigned long n)	{flush();
    												 u_ultoa((long)n,res_msg.body+res_msg.length,10);
                                                     res_msg.length += strlen(res_msg.body+5);
                                                     return *this; }
                                                     
    virtual se_ostream& operator<<(float n)		{ flush();
    											  u_gcvt((double)n,10,res_msg.body+res_msg.length);
                                                  res_msg.length += strlen(res_msg.body+5);
                                                  return *this;  }
                                                  
    virtual se_ostream& operator<<(double n)		{ flush();
    												  u_gcvt(n,10,res_msg.body+res_msg.length);
                                                      res_msg.length += strlen(res_msg.body+5);
                                                      return *this;}
                                                      
	virtual se_ostream& operator<<(long double n)	{ flush();
													  u_gcvt((long double)n,10,res_msg.body+res_msg.length);
                                                      res_msg.length += strlen(res_msg.body+5);
	                                                  return *this; }
	                                                  
    virtual se_ostream& operator<<(void * n)		{ flush();
    												  sprintf(res_msg.body+res_msg.length,"%08X" ,*((int *)n)); 
                                                      res_msg.length += 4;
                                                      return *this; }
                                                      
    virtual se_ostream& put(char c)		        	{ flush();
                                                      res_msg.body[res_msg.length]=c;
                                                   	  res_msg.length += 1;
                                                	  return *this; }
                                    		
    virtual se_ostream& write(char *s, int n)		
	{
    	res_msg.instruction = se_ItemPart; //ItemPart message

		if((res_msg.length + n) > (SE_SOCKET_MSG_BUF_SIZE-5))
	    {
	    	flush();	
	    	int celoe = n/(SE_SOCKET_MSG_BUF_SIZE-5);
	    	int ost;
	    	if(celoe==0) ost = n; else ost = n%(SE_SOCKET_MSG_BUF_SIZE-5);
			for (int i=0;i<celoe;i++)
			{
				res_msg.length = SE_SOCKET_MSG_BUF_SIZE;
				// the body contains string format - 1 byte, string length - 4 bytes and a string
				// construct the buf for body.
				int2net_int(SE_SOCKET_MSG_BUF_SIZE-5, res_msg.body+1);
				memcpy(res_msg.body+5, s+(SE_SOCKET_MSG_BUF_SIZE-5)*i, SE_SOCKET_MSG_BUF_SIZE-5);
				
				if(sp_send_msg(out_socket, &res_msg)!=0) throw USER_EXCEPTION(SE3006);
			} //end for

         	res_msg.length = ost+5;
         	int2net_int(ost, res_msg.body+1);
         	memcpy(res_msg.body+5, s+(SE_SOCKET_MSG_BUF_SIZE-5)*celoe, ost);
         
         }
         else
         {
 	         memcpy(res_msg.body+res_msg.length, s, n);
         	 res_msg.length += n;
             int2net_int(res_msg.length-5, res_msg.body+1);
         }
   	     return *this; 
	}
    virtual se_ostream& flush()				
    {
       if(res_msg.length > 5)
       {
 	   res_msg.instruction = se_ItemPart; //ItemPart message
       int2net_int(res_msg.length-5, res_msg.body+1);
       
       if(sp_send_msg(out_socket, &res_msg)!=0) throw USER_EXCEPTION(SE3006);
       
       res_msg.length = 5;
       }
       return *this; 
    }
    
    virtual void end_of_data(bool res)	{ flush(); 
                                          if (res) res_msg.instruction = se_ItemEnd; //ItemEnd
                                          else res_msg.instruction = se_ResultEnd;     //ResultEnd
                                          res_msg.length = 0; 
                                          if(sp_send_msg(out_socket, &res_msg)!=0)
                                          throw USER_EXCEPTION(SE3006);
                                          
                                          res_msg.length = 5;
                                        }
    virtual void endline()				{ res_msg.body[res_msg.length]='\n'; res_msg.length +=1;}
    virtual void error(const char* str)	{ flush();
                                          res_msg.instruction = se_ErrorResponse; //ErrorResponse
                                          strcpy(res_msg.body+5, str);
                                          int2net_int(strlen(str), res_msg.body+1);      
                                  	      res_msg.length = strlen(str)+5;
                                          if(sp_send_msg(out_socket, &res_msg)!=0)
                                          throw USER_EXCEPTION(SE3006);
    
                                          res_msg.length = 5;
                                        }

};


se_ostream& endl(se_ostream& s);


#endif
