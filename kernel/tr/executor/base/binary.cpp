/*
 * File:  base64Binary.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"
#include "binary.h"
#include "strings.h"


//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
// Alphabet: CHAR <-> VALUE, helpers, etc. 
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////


/* 
   0 A            17 R            34 i            51 z
   1 B            18 S            35 j            52 0
   2 C            19 T            36 k            53 1
   3 D            20 U            37 l            54 2
   4 E            21 V            38 m            55 3
   5 F            22 W            39 n            56 4
   6 G            23 X            40 o            57 5
   7 H            24 Y            41 p            58 6
   8 I            25 Z            42 q            59 7
   9 J            26 a            43 r            60 8
  10 K            27 b            44 s            61 9
  11 L            28 c            45 t            62 +
  12 M            29 d            46 u            63 /
  13 N            30 e            47 v
  14 O            31 f            48 w         (pad) =
  15 P            32 g            49 x
  16 Q            33 h            50 y 
*/

/// Alphabet mapping for base64Binary representation:

const unsigned char b64_value_to_char[64] =  {'A','B','C','D','E','F','G','H','I','J',
                                              'K','L','M','N','O','P','Q','R','S','T',
                                              'U','V','W','X','Y','Z','a','b','c','d',
                                              'e','f','g','h','i','j','k','l','m','n',
                                              'o','p','q','r','s','t','u','v','w','x',
                                              'y','z','0','1','2','3','4','5','6','7',
                                              '8','9','+','/'};

const unsigned char b64_char_to_value[123] = {'_','_','_','_','_','_','_','_','_','_',      //0x
                                              '_','_','_','_','_','_','_','_','_','_',      //1x
                                              '_','_','_','_','_','_','_','_','_','_',      //2x
                                              '_','_','_','_','_','_','_','_','_','_',      //3x
                                              '_','_','_', 62,'_','_','_', 63, 52, 53,      //4x
                                               54, 55, 56, 57, 58, 59, 60, 61,'_','_',      //5x
                                              '_','_','_','_','_', 0 , 1 , 2 , 3 , 4 ,      //6x
                                               5 , 6 , 7 , 8 , 9 , 10, 11, 12, 13, 14,      //7x
                                               15, 16, 17, 18, 19, 20, 21, 22, 23, 24,      //8x
                                               25,'_','_','_','_','_','_', 26, 27, 28,      //9x
                                               29, 30, 31, 32, 33, 34, 35, 36, 37, 38,      //10x
                                               39, 40, 41, 42, 43, 44, 45, 46, 47, 48,      //11x
                                               49, 50, 51};                                 //12x

/// Alphabet mapping for hexBinary representation:

const unsigned char hex_value_to_char[16]  = {'0','1','2','3','4','5','6','7','8','9',
                                              'A','B','C','D','E','F'};

inline unsigned char hex_char_to_value(unsigned char c)
{
    if('A' <= c && c <= 'F') return c - 'A' + 10;
    else return c - '0';
}

/// Helper functions and macroses:

inline bool is_b64_char(unsigned char c)
{
    return c <= 122 && b64_char_to_value[c] != '_';
}

inline bool is_b16_char(unsigned char c)
{
    return is_b64_char(c) && (b64_char_to_value[c] & 3) == 0;
}
    
inline bool is_b04_char(unsigned char c)
{
   return c == 'A'|| c == 'Q'|| c == 'g'|| c == 'w';
}

#define IS_WHITESPACE(byte) \
    (byte == ' ' || byte == '\t' || byte == '\n' || byte == '\r')

const unsigned char hex_allowed[16] = {0x00, 0x00, 0x00, 0x00,
                                       0x00, 0x00, 0xFF, 0xC0, 
                                       0x7E, 0x00, 0x00, 0x00, 
                                       0x7E, 0x00, 0x00, 0x00};
/*
00000000  // 0x00 00-07;
00000000  // 0x00 08-15;
00000000  // 0x00 16-23;
00000000  // 0x00 24-31;
00000000  // 0x00 32-39;
00000000  // 0x00 40-47;
11111111  // 0xFF 48-55;
11000000  // 0xC0 56-63;
01111110  // 0x7E 64-71;
00000000  // 0x00 72-79;
00000000  // 0x00 80-87;
00000000  // 0x00 88-95;
01111110  // 0x7E 96-103;
00000000  // 0x00 104-111;
00000000  // 0x00 112-119;
00000000  // 0x00 120-127;
*/

#define IS_BYTE_HEX_ALLOWED(byte) \
    (byte & 0x80 ? 0 : (hex_allowed[(byte >> 3)] & (0x80 >> (byte & 7))))

#define TO_UPPER_CASE(byte) \
    ('a' <= byte && byte <= 'z' \
    ? (byte + 'A' - 'a') \
    : byte)

//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
// Casting from xs:string or xs:untypedAtomic to hexBinary
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

/// This function checks constraints on xs:hexBinary lexical representation
/// if given value doesn't conform to constraints valid will be 'false'
/// else valid will be true and 'res' will contain canonical representation 
/// of the given value.
template <class Iterator>
static inline void check_constraints_for_xs_hexBinary(Iterator &start, const Iterator &end, stmt_str_buf &res, bool *valid)
{
    (*valid) = false;
    unsigned char value;             
    int counter = 0;                  //number of symbols in hexBinary must be even;
    
    while(start < end && IS_WHITESPACE(*start)) { start++; }
    
    while (start < end)
    {
        value = *start;
        if( !IS_BYTE_HEX_ALLOWED(value) ) 
        {
            if( IS_WHITESPACE(value) ) break;
            return;
        }
        res << TO_UPPER_CASE(value);
        ++counter;
        ++start;
    }
    
    while(start < end && IS_WHITESPACE(*start)) { start++; }

    if(start == end && !(counter & 1)) (*valid) = true; //chech evenness at last;
}

tuple_cell cast_string_type_to_xs_hexBinary(const tuple_cell &c)
{ 
    bool valid;
    stmt_str_buf res;

    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_2p(check_constraints_for_xs_hexBinary, &c, res, &valid);
    
    if(!valid) throw USER_EXCEPTION2(FORG0001, "The value does not conform to the lexical constraints defined for the xs:hexBinary type.");
    tuple_cell rc = res.get_tuple_cell();
    rc.set_xtype(xs_hexBinary);
    return rc;
}



//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
// Casting from xs:string or xs:untypedAtomic to base64Binary
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

/*
/// This function checks constraints on xs:base64Binary lexical representation
/// if given value doesn't conform to constraints valid will be 'false'
/// else valid will be true and 'res' will contain canonical representation 
/// of the given value.
template <class Iterator>
static inline void check_constraints_for_xs_base64Binary(Iterator &start, const Iterator &end, e_string_o_iterator<unsigned char> &res_it, bool *valid)
{
    (*valid) = false;

    while(start < end && IS_WHITESPACE(*start)) { start++; }
    if(start == end)
    {
        (*valid) = true;
        return;
    }
    
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
                int type_of_tail = (counter & 3);                    //type of tail must be either 3 ("=") or 2 ("==").
                end_reached = true;
                
                switch(type_of_tail)
                {
                    
                    case 3: 
                        if(!is_b16_char(last_saved_char)) return;    //previous char must be from the b16 char-set;
                        *res_it = value;
                        ++res_it;
                        break;

                    case 2:
                        if(!is_b04_char(last_saved_char)) return;     //previous char must be from the b04 char-set;
                        
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

    //if we did not reach the tail symbol "=" then we must chech that we do not have 
    //spaces at the end and that we have the whole number of the (b64 b64 b64 b64) groups. 
    if(!end_reached && (previous == ' ' || (counter & 3) != 0)) return;

    (*valid) = true;
}
*/

template <class Iterator>
static inline void check_constraints_for_xs_base64Binary(Iterator &start, const Iterator &end, stmt_str_buf &res, bool *valid)
{
    (*valid) = false;

    unsigned char value;
    unsigned char last_saved_char;
    int counter = 0;
    int type_of_tail = 0;
    bool tail_reached = false;
    
    while(start < end)
    {
        value = *start;

        if(!IS_WHITESPACE(value)) 
        {  
            if( (tail_reached || !is_b64_char(value)) && value != '=' ) return;
            
            if( value == '=' )
            {
                if(tail_reached && type_of_tail != 2) return;
                else if(tail_reached) type_of_tail = 0;
                else
                {
                    tail_reached = true;
                    type_of_tail = (counter & 3);                                  //type of tail must be either 3 ("=") or 2 ("==").
                    switch(type_of_tail)
                    {
                       case 3 : if(!is_b16_char(last_saved_char)) return; break;   //previous char must be from the b16 char-set.
                       case 2 : if(!is_b04_char(last_saved_char)) return; break;   //previous char must be from the b04 char-set.
                       default: return;
                    }
                }
            }

            res << value;
            ++counter;
            last_saved_char = value;
        }
        
        start++;
    }

    if((counter & 3) == 0) (*valid) = true;
}


tuple_cell cast_string_type_to_xs_base64Binary(const tuple_cell &c)
{ 
    bool valid;
    stmt_str_buf res;

    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_2p(check_constraints_for_xs_base64Binary, &c, res, &valid);
    
    if(!valid) throw USER_EXCEPTION2(FORG0001, "The value does not conform to the lexical constraints defined for the xs:base64Binary type.");
    tuple_cell rc = res.get_tuple_cell();
    rc.set_xtype(xs_base64Binary);
    return rc;
}



//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
// Casting from xs:base64Binary to xs:hexBinary.
// Note! xs:base64Binary must be in the correct canonical form!
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////


template <class Iterator>
static inline void base64_to_hex(Iterator &start, const Iterator &end, stmt_str_buf &res)
{
    bool evenness_flag = false;

    unsigned char value;
    unsigned char previous;

    while(start < end && *start != '=')
    {
        value = b64_char_to_value[*start];
        start++;

        if(evenness_flag)
        {
            res << hex_value_to_char[previous | ((value & 48) >> 4)];    //mask = 00110000 bin = 48 dec;
            if(*start != '=') res << hex_value_to_char[value & 15];      //mask = 00001111 bin = 15 dec;
        }
        else
        {
            previous = (value & 3) << 2;                                 //mask = 00000011 bin = 3  dec;
            res << hex_value_to_char[(value & 60) >> 2];                 //mask = 00111100 bin = 60 dec;
        }
        
        evenness_flag = !evenness_flag;
    }
}


tuple_cell cast_base64Binary_to_hexBinary(const tuple_cell &c)
{
    stmt_str_buf res;
    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(base64_to_hex, &c, res);
    tuple_cell rc = res.get_tuple_cell();
    rc.set_xtype(xs_hexBinary);
    return rc;
}



//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
// Casting from xs:hexBinary to xs:base64Binary.
// Note! xs:hexBinary must be in the correct canonical form!
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

template <class Iterator>
static inline void hex_to_base64(Iterator &start, const Iterator &end, stmt_str_buf &res)
{
    int evenness_flag = 0;

    unsigned char value;
    unsigned char previous;

    int counter;

    while(start < end)
    {
        value = hex_char_to_value(*start);
   
        if(evenness_flag == 0)
        {
            previous = value << 2;
            evenness_flag++;
        }
        else if(evenness_flag == 1)
        {
            res << b64_value_to_char[previous | ((value & 12) >> 2)];           //mask = 00001100 bin = 12 dec;
            ++counter;
            previous = (value & 3) << 4;                                        //mask = 00000011 bin = 3  dec;
            evenness_flag++;                                             
        }
        else if(evenness_flag == 2)
        {
            res << b64_value_to_char[previous | (value & 15)];                  //mask = 00001111 bin = 15 dec;
            ++counter;
            evenness_flag = 0;
        }
        start++;
    }

    if(evenness_flag > 0)                                                       //don't forget to write last symbol,
    {
        res << b64_value_to_char[previous];
        ++counter;
    }
                                                                                //and then create appropriate end for base64 type:
    int tail_type = (counter & 3);                                              //type of tail must be either 2 ("="), 3 ("==" or "= =") 

    if(tail_type > 1) 
    {
        res << '='; 
        if(tail_type == 2) res << '=';
    }
}


tuple_cell cast_hexBinary_to_base64Binary(const tuple_cell &c)
{
    stmt_str_buf res;
    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(hex_to_base64, &c, res);
    tuple_cell rc = res.get_tuple_cell();
    rc.set_xtype(xs_base64Binary);
    return rc;
}

