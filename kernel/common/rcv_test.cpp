#include "../libs/expat/expat.h"
#include "common/ipc_ops.h"
#include "common/rcv_test.h"
#include "common/errdbg/d_printf.h"

#include <map>

static std::map <std::string, int> fname_prob_table;

void rcv_test_crash_point(const char *func_name)
{
	std::map<std::string, int>::iterator it = fname_prob_table.find(std::string(func_name));
	int prob;

	if (it == fname_prob_table.end()) return;

	prob = it->second;

    if (((rand() % 100) + 1) <= prob)
    {
       elog(EL_ERROR, ("\nTr is crashed in %s due to recovery testing\n", func_name));
       d_printf2("\nTr is crashed in %s due to recovery testing\n", func_name);
       _exit(1);
    }
}

void startElement_rcv_cfg(void *cnt, const char *name, const char **atts)
{
}

void endElement_rcv_cfg(void *cfg, const char *name)
{
  int prob = atoi(erase_ws(elem_content.c_str()).c_str());
  
  if (prob < 0 || prob > 100)
  {
  	d_printf2("ignoring parameter for %s since it's out of range [0;100]\n", name);
    return;
  }

  fname_prob_table[std::string(name)] = prob;
  
  elem_content = "";
}

void characterData_rcv_cfg(void *cnt, const XML_Char *s, int len)
{
   elem_content.append(s, len);
}

void parse_test_cfg(std::string cfg_text)
{
  XML_Parser parser = XML_ParserCreate (NULL);

  XML_SetElementHandler (parser, startElement_rcv_cfg, endElement_rcv_cfg);

  XML_SetCharacterDataHandler (parser, characterData_rcv_cfg);
   
  int parse_res;

  d_printf2("rcv_cfg_text=%s\n", cfg_text.c_str());

  parse_res = XML_Parse (parser, cfg_text.c_str(), cfg_text.length(), 1);

  if(parse_res == XML_STATUS_ERROR)
    throw USER_EXCEPTION2(SE4201,  "recovery test config file");

  XML_ParserFree(parser);
}

void read_test_cfg()
{
  //find and parse rcv_test.xml
  char rcv_test_cfg_file[U_MAX_PATH + 1];
  char proc_buf[U_MAX_PATH + 1];

  uGetImageProcPath(proc_buf, __sys_call_error);
  if (proc_buf[0] == '\0') 
      throw USER_EXCEPTION(SE4081);


#ifdef _WIN32
  strcpy(rcv_test_cfg_file, proc_buf);
  strcat(rcv_test_cfg_file, "\\rcv_test.xml");
#else
  strcpy(rcv_test_file, proc_buf);
  strcat(rcv_test_cfg_file, "/rcv_test.xml");
#endif


  FILE* fs;
  char buf[1024];
  int i, size;

  d_printf2("rcv_test_cfg_file=%s\n", rcv_test_cfg_file);

  fs = fopen(rcv_test_cfg_file, "r");
  std::string cfg_text;
  cfg_text.reserve(10240);//reserve 10Kb

  if (fs != NULL)  
  {
     d_printf1("rcv_test.xml exists in image directory\n");

     while (true)
     {
        size = fread(buf, sizeof(char), 1024, fs);

        if (ferror(fs)) throw USER_EXCEPTION2(SE4044, "rcv_test.xml");

        cfg_text.append(buf, size);

        if (feof(fs)) break; 
     }

     fclose(fs);

     parse_test_cfg(cfg_text);
  }

  srand((unsigned)time(NULL));
}
