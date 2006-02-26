/*
 * File:  exec_output.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "exec_output.h"

crmostream& endl(crmostream& s)
{
    s.endline();
    return s;
}

crmostream& crmsocketostream::writextext(char *s, int n)		
	{ 
		int j = 0; 
		for (int i=0;i<n;i++)
		{
		 switch (s[i])
		 {
		 case '&':
			 {
				 (*this).write(s+j,i-j); 
				 (*this).write("&amp;",5);
				 j=i+1;
                                 break;
			 }
		 case '<':
			 {
				 (*this).write(s+j,i-j); 
				 (*this).write("&lt;",4);
				 j=i+1;
                                 break;
			 }
		 case '"':
			 {
				 (*this).write(s+j,i-j); 
				 (*this).write("&quot;",6);
				 j=i+1;
                                 break;
			 }
		 case '>':
			 {
				 (*this).write(s+j,i-j); 
				 (*this).write("&gt;",4);
				 j=i+1;
                                 break;
			 }
		 }//end switch
		}//end for
		
			
		(*this).write(s+j,n-j); 
		return *this; 
		
	}

crmostream& crmstdostream::writextext(char *s, int n)		
{ 
	int j=0;
		for (int i=0;i<n;i++)
		{
		 switch (s[i])
		 {
		 case '&':
			 {
				 std::cout.write(s+j,i-j); 
				 std::cout.write("&amp;",5);
				 j=i+1;
			  }break;
		 case '<':
			 {
				 std::cout.write(s+j,i-j); 
				 std::cout.write("&lt;",4);
				 j=i+1;
			 }break;
		 case '>':
			 {
				 std::cout.write(s+j,i-j); 
				 std::cout.write("&gt;",4);
				 j=i+1;
			 }break;
		 }
		}
		std::cout.write(s+j,n-j); 
		return *this; 
}

crmostream& crmstringostream::writextext(char *s, int n)
{
	int j=0;
		for (int i=0;i<n;i++)
		{
		 switch (s[i])
		 {
		 case '&':
			 {
				 out_str.write(s+j,i-j); 
				 out_str.write("&amp;",5);
				 j=i+1;
			  }break;
		 case '<':
			 {
				 out_str.write(s+j,i-j); 
				 out_str.write("&lt;",4);
				 j=i+1;
			 }break;
		 case '>':
			 {
				 out_str.write(s+j,i-j); 
				 out_str.write("&gt;",4);
				 j=i+1;
			 }break;
		 }
		}
		out_str.write(s+j,n-j); 
		return *this; 

}
