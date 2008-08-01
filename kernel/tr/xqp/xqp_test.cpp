/*
 * File:  xqp_test.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <string>
#include <iostream>
#include <fstream>

#include "common/sedna.h"

#include "tr/xqp/XQuerytoLR.h"

#include "tr/xqp/XQueryTreeParser.h"
#include "tr/xqp/SORAST.h"
#include "tr/xqp/AST.h"
#include "tr/xqp/flwr.h"

using namespace std;

extern int count;

int main(int argc, char** argv){

if(argc!=2){
 cout << "XQuery parser." << endl;
 cout << "Usage: xqp filename" << endl;
 cout << endl;
 cout << "filename: file with a query" << endl;
 return 1;
}

FILE *f = fopen(argv[1], "r");
string query;
char buf[1000000];

if(f==NULL){
 cout << "No such file: " << argv[1] << endl;
 return 1;
}

query.reserve(100000);

while(!feof(f)){

 size_t len= fread(buf, sizeof(char), sizeof(buf), f);

 query.append(buf, len);
}

cout << "query in buffer" << endl;

try{
  

   //XQuerytoIR(query.c_str());
    StringVector v = parse_batch(TL_XQuery, query.c_str());

  for (int i=0; i< v.size(); i++)
      cout << v[i].c_str();
  //string tmp;
  //((SORAST*)phys_repr)->preorder(&tmp);

  cout << endl;

//  cout << "============ XQuery =============" << endl;
//  cout << query.c_str() << endl;
/*
  cout << "============== IR ===============" << endl;
  //cout << tmp.c_str() << endl;
  cout << ((SORAST*)(phys_repr->down()))->getText() << endl;
*/
 // cout << "finished" << "count=" << count << endl;
//  getchar();
}
catch (SednaUserException &e) 
 {
   d_printf2("%s", e.what());
 }

if( fclose(f) )
    cout << "The file 'data' was not closed\n" << endl;


return 0;
} 

