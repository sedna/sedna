/*
 * File:  base64Binary.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"
#include "base64Binary.h"
#include "e_string.h"
#include "strings.h"


//this function checks constraints on xs:base64Binary lexical representation
//if given value doesn't conform to constraints valid will be 'false'
//else valid will be true and res_it will contain canonical representation of the 
//given value.
template <class Iterator>
static inline void check_constraints_for_xs_base64Binary(Iterator &start, const Iterator &end, e_string_o_iterator<unsigned char> &res_it, bool *valid)
{
    (*valid) = false;

    if(start == end) return; //empty string is not allowed here
    
    unsigned char previous = *start;
    unsigned char value;
    unsigned char last_saved_char;
    int counter = 0;
    bool end_reached = false;
    
    while(start < end)
    {
        value = *start;
        
        if( end_reached ||                      //tail symbol '=' was reached, but we have other symbols.
           (value == ' ' && previous == ' ') || //space is not allowed on start and end positions, 
                                                //and within sequences of two or more.
           (!is_b64_char(value) && value != '=' && value != ' ')) return;
        
        if(value != ' ')
        {
            if(value != '=') 
            {
                *res_it = value;
                last_saved_char = value;
                ++counter;
                ++res_it;
            }
            else
            {
                int type_of_tail = counter - (counter / 4) * 4;       //type of tail can be either 2 ("=") or 3 ("==" or "= =").
                end_reached = true;
                
                switch(type_of_tail)
                {
                    
                    case 3: 
                        if(!is_b16_char(last_saved_char)) return;            //previous char must be from the b16 char-set;
                        *res_it = value;
                        ++res_it;
                        break;

                    case 2:
                        if(!is_b04_char(last_saved_char)) return;            //previous char must be from the b04 char-set;
                        
                        *res_it = value;                              //save current "=" before go on;
                        ++res_it;                 
                        
                        if((++start) == end ||                        //end could not be at this moment;
                           (*start != ' ' && *start != '=')) return;  //we expect "=" or " =";
                        
                        if(*start == ' ' && ((++start) == end || *start != '=')) return; //if we have " =" we must consume space and check
                                                                                         //that sting is not over and we have last "="
                        *res_it = *start;
                        ++res_it;
                        break;
                    
                    default: return;
                }
            }
        }
    	
        previous = value;
        ++start;
    }

    //if we did not reach the tail symbol "=" than we must chech that we do not have 
    //spaces at the end and that we have the whole number of the (b64 b64 b64 b64) groups. 
    if(!end_reached && (previous == ' ' || (counter - (counter / 4) * 4) != 0)) return;

    (*valid) = true;
}

tuple_cell cast_string_type_to_xs_base64Binary(const tuple_cell &c)
{ 
    if (e_string_last_blk==XNULL) 
    {
        vmm_alloc_tmp_block(&e_string_last_blk);
        e_str_blk_hdr::init(XADDR(e_string_last_blk));
        e_string_first_blk = e_string_last_blk;
    }
    
    e_string_o_iterator<unsigned char> res_it;
    xptr start_pos = res_it.pos;
    bool valid;

    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_2p(check_constraints_for_xs_base64Binary, &c, res_it, &valid);
    
    if(!valid) throw USER_EXCEPTION2(FORG0001, "The value does not conform to the lexical constraints defined for the xs:base64Binary type.");
    int reslen = get_length_of_last_str(start_pos);  //FIXME!!! Possibly it must be __int64???
    return tuple_cell::atomic_estr(xs_base64Binary, reslen, start_pos);
}





