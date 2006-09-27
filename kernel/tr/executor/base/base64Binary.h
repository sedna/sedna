/*
 * File:  base64Binary.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BASE64BIMARY_H
#define _BASE64BINARY_H

#include "sedna.h"
#include "PPBase.h"

//////////////////////////////////////////////////////////////////////////////////////////
// Char <-> Value map
//////////////////////////////////////////////////////////////////////////////////////////
/* 0 A            17 R            34 i            51 z
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
  16 Q            33 h            50 y */




const unsigned char value_to_char[64] =  {'A','B','C','D','E','F','G','H','I','J',
                                          'K','L','M','N','O','P','Q','R','S','T',
                                          'U','V','W','X','Y','Z','a','b','c','d',
                                          'e','f','g','h','i','j','k','l','m','n',
                                          'o','p','q','r','s','t','u','v','w','x',
                                          'y','z','0','1','2','3','4','5','6','7',
                                          '8','9','+','/'};

const unsigned char char_to_value[123] = {'_','_','_','_','_','_','_','_','_','_',      //0x
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

//////////////////////////////////////////////////////////////////////////////////////////


inline bool is_b64_char(unsigned char c)
{
    return c <= 122 && char_to_value[c] != '_';
}

inline bool is_b16_char(unsigned char c)
{
    return is_b64_char(c) && 
  	       (char_to_value[c] - (char_to_value[c] / 4) * 4) == 0;
}
    
inline bool is_b04_char(unsigned char c)
{
   return c == 'A'|| c == 'Q'|| c == 'g'|| c == 'w';
}

tuple_cell cast_string_type_to_xs_base64Binary(const tuple_cell &c);

#endif
