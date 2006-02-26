/*
 * File:  exec_output.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _EXEC_OUTPUT_H
#define _EXEC_OUTPUT_H

#include <iostream>
#include <strstream>
#include "usocket.h"
#include "exceptions.h"
#include "sp.h"
#include "uutils.h"
#include "base.h"


enum executor_ostream_t {eot_std, eot_sock, eot_str, eot_null};
	
	

class crmostream
{
public:
    virtual ~crmostream() {}

    virtual crmostream& operator<<(crmostream& (*pf)(crmostream&)) = 0;// { return *this; }
    virtual crmostream& operator<<(const char *s)	= 0;//{ return *this; }
    virtual crmostream& operator<<(char c)		= 0;//{ std::cout << "crmostream" << std::endl; return *this; }
    virtual crmostream& operator<<(bool n)		= 0;//{ return *this; }
    virtual crmostream& operator<<(short n)		= 0;//{ return *this; }
    virtual crmostream& operator<<(unsigned short n)	= 0;//{ return *this; }
    virtual crmostream& operator<<(int n)		= 0;//{ return *this; }
    virtual crmostream& operator<<(unsigned int n)	= 0;//{ return *this; }
    virtual crmostream& operator<<(long n)		= 0;//{ return *this; }
    virtual crmostream& operator<<(unsigned long n)	= 0;//{ return *this; }
    virtual crmostream& operator<<(float n)		= 0;//{ return *this; }
    virtual crmostream& operator<<(double n)		= 0;//{ return *this; }
    virtual crmostream& operator<<(long double n)	= 0;//{ return *this; }
    virtual crmostream& operator<<(void * n)		= 0;//{ return *this; }
    virtual crmostream& put(char c)			= 0;//{ return *this; }
    virtual crmostream& write(char *s, int n)		= 0;//{ return *this; }
    virtual crmostream& writextext(char *s, int n)	= 0;//{ return *this; }
    virtual crmostream& flush()				= 0;//{ return *this; }
//    virtual void delim()				= 0;//{}
    virtual void end_of_data(bool res)				= 0;//{}
    virtual void endline()				= 0;//{}
    virtual void error(const char*)			= 0;//{}
//    virtual void error_of_data()			= 0;//{}    
//    virtual void writeHead(int, int)			= 0;//{}
	virtual crmostream& operator<<(__int64 n)
	{
		char* z=new char[20];
		short pos=19;
		__int64 k=n;
		while (k>9)
		{
			z[pos]=(char)(k%10);
			k=k/10;
			pos--;
		}
		z[pos]=(char)(k);
		for (int i=pos;i<20;i++)
			*this<<(int)z[i];
		delete [] z;
		return *this;
	}
};

class crmstdostream : public crmostream
{
public:
    crmstdostream() {}
    virtual ~crmstdostream() {}

    virtual crmostream& operator<<(crmstdostream& (*pf)(crmstdostream&))	{ (*pf)(*this); return *this; }
    virtual crmostream& operator<<(crmostream& (*pf)(crmostream&))	{ (*pf)(*this); return *this; }
    virtual crmostream& operator<<(const char *s)	{ //std::cout << std::endl << "!!!" << std::endl; 
                                                          //printf("#%s#", s);
                                                          //std::cout << "std" << std::endl;
                                                          std::cout << s; 
                                                          return *this; }
    virtual crmostream& operator<<(char c)		{ std::cout << c; return *this; }
    virtual crmostream& operator<<(bool n)		{ std::cout << n; return *this; }
    virtual crmostream& operator<<(short n)		{ std::cout << n; return *this; }
    virtual crmostream& operator<<(unsigned short n)	{ std::cout << n; return *this; }
    virtual crmostream& operator<<(int n)		{ std::cout << n; return *this; }
    virtual crmostream& operator<<(unsigned int n)	{ std::cout << n; return *this; }
    virtual crmostream& operator<<(long n)		{ std::cout << n; return *this; }
    virtual crmostream& operator<<(unsigned long n)	{ std::cout << n; return *this; }
    virtual crmostream& operator<<(float n)		{ std::cout << n; return *this; }
    virtual crmostream& operator<<(double n)		{ std::cout << n; return *this; }
    virtual crmostream& operator<<(long double n)	{ std::cout << n; return *this; }
    virtual crmostream& operator<<(void * n)		{ std::cout << n; return *this; }
    virtual crmostream& put(char c)			{ std::cout.put(c); return *this; }
    virtual crmostream& write(char *s, int n)		{ std::cout.write(s, n); return *this; }
    virtual crmostream& writextext(char *s, int n)	;//{ return *this; }
	virtual crmostream& flush()				{ std::cout.flush(); return *this; }
//    virtual void delim()				{ std::cout << std::endl; }
    virtual void end_of_data(bool res)		{ std::cout << std::endl; }
    virtual void endline()				{ std::cout << std::endl; }
    virtual void error(const char* str)			{ std::cout << str << std::endl; }
//    virtual void error_of_data()                        { std::cout << std::endl;}
//    virtual void writeHead(int cmd, int len)		{ std::cout << cmd << len;}
};


class crmsocketostream : public crmostream
{
private:
    USOCKET out_socket;

    msg_struct res_msg;

public:
    crmsocketostream(USOCKET _out_socket_) : out_socket(_out_socket_) 
    {  
        res_msg.body[0] = 0;           // in this version string format is always 0
        res_msg.length = 5;   // the body contains string format - 1 byte, string length - 4 bytes and a string
    }
    virtual ~crmsocketostream() {}

    virtual crmostream& operator<<(crmsocketostream& (*pf)(crmsocketostream&))	{ (*pf)(*this); return *this; }
    virtual crmostream& operator<<(crmostream& (*pf)(crmostream&))	{ (*pf)(*this); return *this; }
    virtual crmostream& operator<<(const char *s)	
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

         		if(sp_send_msg(out_socket, &res_msg)!=0) throw USER_EXCEPTION(SE3006);
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
    virtual crmostream& operator<<(char c){	flush();
    										res_msg.body[res_msg.length]=c;
                                        	res_msg.length += 1;  // 5+1
                                    		return *this; }
                                    		
    virtual crmostream& operator<<(bool n)	{ flush();
    										  (n)? res_msg.body[res_msg.length]='1':res_msg.body[res_msg.length]='0';
                                              res_msg.length += 1;  // 5+1
                                              return *this; }
                                              
    virtual crmostream& operator<<(short n)	{ flush();
    										 _ltoa((long)n,res_msg.body+res_msg.length,10); 
                                              res_msg.length += strlen(res_msg.body+5);
                                              return *this; }
                                              
    virtual crmostream& operator<<(unsigned short n){flush();
    												_ultoa((unsigned long)n,res_msg.body+res_msg.length,10);
                                                    res_msg.length += strlen(res_msg.body+5);
                                                    return *this; }
                                                    
    virtual crmostream& operator<<(int n)		{ flush();	
    											  _ltoa((long)n,res_msg.body+res_msg.length,10);
                                                  res_msg.length += strlen(res_msg.body+5);
                                                  return *this; }
                                                   
    virtual crmostream& operator<<(unsigned int n)	{ flush();
                                                      _ultoa((long)n,res_msg.body+res_msg.length,10); 
                                                      res_msg.length += strlen(res_msg.body+5);
                                                      return *this; }
                                                      
    virtual crmostream& operator<<(long n)		{ flush();
                                                  _ltoa((long)n,res_msg.body+res_msg.length,10); 
                                                  res_msg.length += strlen(res_msg.body+5);
                                                  return *this; }
                                                    
    virtual crmostream& operator<<(unsigned long n)	{flush();
    												 _ultoa((long)n,res_msg.body+res_msg.length,10);
                                                     res_msg.length += strlen(res_msg.body+5);
                                                     return *this; }
                                                     
    virtual crmostream& operator<<(float n)		{ flush();
    											  _gcvt((double)n,10,res_msg.body+res_msg.length);
                                                  res_msg.length += strlen(res_msg.body+5);
                                                  return *this;  }
                                                  
    virtual crmostream& operator<<(double n)		{ flush();
    												  _gcvt(n,10,res_msg.body+res_msg.length);
                                                      res_msg.length += strlen(res_msg.body+5);
                                                      return *this;}
                                                      
	virtual crmostream& operator<<(long double n)	{ flush();
													  _gcvt((long double)n,10,res_msg.body+res_msg.length);
                                                      res_msg.length += strlen(res_msg.body+5);
	                                                  return *this; }
	                                                  
    virtual crmostream& operator<<(void * n)		{ flush();
    												  sprintf(res_msg.body+res_msg.length,"%08X" ,*((int *)n)); 
                                                      res_msg.length += 4;
                                                      return *this; }
                                                      
    virtual crmostream& put(char c)		        	{ flush();
                                                      res_msg.body[res_msg.length]=c;
                                                   	  res_msg.length += 1;
                                                	  return *this; }
                                    		
    virtual crmostream& write(char *s, int n)		
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
	virtual crmostream& writextext(char *s, int n)	;
    virtual crmostream& flush()				
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
    virtual void endline()				{ res_msg.body[res_msg.length]='\n'; res_msg.length +=1; }
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

class crmstringostream : public crmostream
{
private:
    std::strstream out_str;
public:
    crmstringostream() {}
    virtual ~crmstringostream() {}

    virtual crmostream& operator<<(crmstringostream& (*pf)(crmstringostream&))	{ (*pf)(*this); return *this; }
    virtual crmostream& operator<<(crmostream& (*pf)(crmostream&))	{ (*pf)(*this); return *this; }
    virtual crmostream& operator<<(const char *s)	{ out_str << s; 
                                                          return *this; }
    virtual crmostream& operator<<(char c)		{ out_str << c; return *this; }
    virtual crmostream& operator<<(bool n)		{ out_str << n; return *this; }
    virtual crmostream& operator<<(short n)		{ out_str << n; return *this; }
    virtual crmostream& operator<<(unsigned short n)	{ out_str << n; return *this; }
    virtual crmostream& operator<<(int n)		{ out_str << n; return *this; }
    virtual crmostream& operator<<(unsigned int n)	{ out_str << n; return *this; }
    virtual crmostream& operator<<(long n)		{ out_str << n; return *this; }
    virtual crmostream& operator<<(unsigned long n)	{ out_str << n; return *this; }
    virtual crmostream& operator<<(float n)		{ out_str << n; return *this; }
    virtual crmostream& operator<<(double n)		{ out_str << n; return *this; }
    virtual crmostream& operator<<(long double n)	{ out_str << n; return *this; }
    virtual crmostream& operator<<(void * n)		{ out_str << n; return *this; }
    virtual crmostream& put(char c)			{ out_str.put(c); return *this; }
    virtual crmostream& write(char *s, int n)		{ out_str.write(s, n); return *this; }
    virtual crmostream& writextext(char *s, int n)	;//{ return *this; }
    virtual crmostream& flush()				{ out_str.flush(); return *this; }
 //   virtual void delim()				{ out_str << std::endl; }
    virtual void end_of_data(bool res)				{ out_str << std::endl; }
    virtual void endline()				{ out_str << std::endl; }
    virtual void error(const char* str)			{ out_str << str << std::endl; }
//    virtual void error_of_data()                        { out_str << std::endl;}
//    virtual void writeHead(int cmd, int len)		{ out_str << cmd << len;}

    virtual char* str() { return out_str.str(); }
};

class crmnullostream : public crmostream
{
public:
    crmnullostream() {}
    virtual ~crmnullostream() {}

    virtual crmostream& operator<<(crmnullostream& (*pf)(crmnullostream&))	{return *this;}
    virtual crmostream& operator<<(crmostream& (*pf)(crmostream&))	{return *this;}
    virtual crmostream& operator<<(const char *s) {return *this;}
    virtual crmostream& operator<<(char c) {return *this;}
    virtual crmostream& operator<<(bool n) {return *this;}
    virtual crmostream& operator<<(short n)	{return *this;}
    virtual crmostream& operator<<(unsigned short n) {return *this;}
    virtual crmostream& operator<<(int n) {return *this;}
    virtual crmostream& operator<<(unsigned int n) {return *this;}
    virtual crmostream& operator<<(long n) {return *this;}
    virtual crmostream& operator<<(unsigned long n)	{return *this;}
    virtual crmostream& operator<<(float n) {return *this;}
    virtual crmostream& operator<<(double n) {return *this;}
    virtual crmostream& operator<<(long double n) {return *this;}
    virtual crmostream& operator<<(void * n) {return *this;}
    virtual crmostream& put(char c)	{return *this;}
    virtual crmostream& write(char *s, int n) {return *this;}
    virtual crmostream& writextext(char *s, int n) {return *this;}
	virtual crmostream& flush()	{return *this;}
//    virtual void delim() {}
    virtual void end_of_data(bool res) {}
    virtual void endline() {}
    virtual void error(const char* str)	{}
//    virtual void error_of_data() {}
//    virtual void writeHead(int cmd, int len){}
};



crmostream& endl(crmostream& s);

//crmostream& delim(crmostream& s);

crmostream& end_of_data(crmostream& s);

crmostream& _error(crmostream& s, const char* str);

struct err
{
    const char* str;

    err(const char* _str_) : str(_str_) {}
};

crmostream& operator<<(crmostream& s, const err& e);


#endif
