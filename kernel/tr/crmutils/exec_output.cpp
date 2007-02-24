/*
 * File:  exec_output.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/crmutils/exec_output.h"
#include "tr/executor/base/PPBase.h"

using namespace tr_globals;

void write_func(void *param, const char *str, int len)
{
    ((se_ostream*)param)->write(str,len);
}

se_ostream& se_ostream::operator<<(__int64 n)
{
	char z[20];
	short pos = 19;
	__int64 k = n;
    int i = 0;
	while (k > 9)
	{
		z[pos] = (char)(k % 10);
		k= k / 10;
		--pos;
	}
	z[pos] = (char)k;
	for (i = pos; i < 20; ++i)
		*this << (int)z[i];
	return *this;
}

se_ostream& se_ostream::writextext(char *s, int n)
{
	dynamic_context::stm.parse(s,n,write_func,this,(int)pat_element);
   /* int j = 0, i = 0;
    for (i = 0; i < n; i++)
    {
        switch (s[i])
        {
            case '&':
    	    {
    		    this->write(s+j,i-j); 
    		    this->write("&amp;",5);
    		    j = i + 1;
                break;
    	    }
            case '<':
            {
                this->write(s+j,i-j); 
                this->write("&lt;",4);
                j = i + 1;
                break;
            }
            case '>':
            {
                this->write(s+j,i-j); 
                this->write("&gt;",4);
                j = i + 1;
                break;
    	    }
			case '"':
			{
				if (sxmlescape)
				{
					this->write(s+j,i-j);
					this->write("\\\"",2);
					j = i + 1;
                break;
				}
			}
        }
    }
    this->write(s+j, n-j); */
    return *this; 
}

se_ostream& se_ostream::writeattribute(char *s, int n)
{ 
	dynamic_context::stm.parse(s,n,write_func,this,(int)pat_attribute);
	/*int j = 0, i = 0;
	for (i = 0; i < n; i++)
	{
		switch (s[i])
		{
		case '&':
			{
				this->write(s+j,i-j); 
				this->write("&amp;",5);
				j = i + 1;
				break;
			}
		case '<':
			{
				this->write(s+j,i-j); 
				this->write("&lt;",4);
				j = i + 1;
				break;
			}
		case '>':
			{
				this->write(s+j,i-j); 
				this->write("&gt;",4);
				j = i + 1;
				break;
			}
		case '\"':
			{
				this->write(s+j,i-j); 
				this->write("&quot;",6);
				j = i + 1;
				break;
			}
		}
	}
	this->write(s+j, n-j); */
	return *this; 
}

se_ostream* se_socketostream::get_debug_ostream()
{ 
    if (_p_ver.major_version < 2)
        return se_new se_nullostream();
    else
        return se_new se_debug_socketostream(*this);
}

se_ostream& endl(se_ostream& s)
{
    s.endline();
    return s;
}
