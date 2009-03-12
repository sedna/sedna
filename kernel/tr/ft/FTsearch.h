/*
 * File:  FTsearch.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FTSEARCH_H
#define _FTSEARCH_H

#include "common/sedna.h"

#include "tr/executor/base/dm_accessors.h"
#include "tr/strings/strings.h"
#include "tr/strings/e_string.h"
#include "tr/strings/e_string_iterator.h"
#include "tr/ft/ft_index_data.h"


#include <ios>
#include <sstream>
#include <fstream> //needed for dtsearch (in linux), since it defines min & max macros
#define USE_DTSEARCH_NAMESPACE
#include "dtsearch/include/dtsfc.h"
#include "tr/crmutils/crmutils.h"



//TODO: remove this and do not include dtsearch files here 
// (dstring.h defines true/false then boolean_operations do not compile)
#ifdef false
#undef false
#endif
#ifdef true
#undef true
#endif


class SednaTextInputStream {
    public:
        SednaTextInputStream(dtsFileInfo* _info_,ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_);
        ~SednaTextInputStream();
        void seek_mem(long where);
        int read_mem(void *dest, long bytes);
        void seek_estr(long where);
        int read_estr(void *dest, long bytes);
        void makeInterface(dtsInputStream& dest,xptr& node);
        static void seekCBmem(void *pData, long where);
        static long readCBmem(void *pData, void *data, long bytes);
        static void seekCBestr(void *pData, long where);
        static long readCBestr(void *pData, void *data, long bytes);
        static void releaseCB(void *pData);
        static SednaTextInputStream *safeCast(void *pData);
    protected:
        long idTextInputStream;
        long pos;
		estr_iterator *estr_it;
        dtsFileInfo* fileInfo;
		op_str_buf in_buf;
		ft_index_type cm;
		pers_sset<ft_custom_cell,unsigned short>* custom_tree;
    };

class SednaDataSource : public dtSearch::DDataSourceBase {
    public:
        SednaDataSource(ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_);
		~SednaDataSource(){delete tis;}
        virtual int getNextDoc(dtsInputStream& s);
		virtual xptr get_next_doc()=0;
		virtual int rewind();
        static void recordToFilename(char *dest,xptr _node_);
        static xptr filenameToRecord(const char *dest);
//		dtsDataSource * getInterface();
    protected:
		ft_index_type cm;
		pers_sset<ft_custom_cell,unsigned short>* custom_tree;
		SednaTextInputStream *tis;
		dtsFileInfo fileInfo;

    };
class OperationSednaDataSource : public  SednaDataSource
{
public:
	OperationSednaDataSource(ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_,PPOpIn* _op_);
private:
	virtual xptr get_next_doc();
	virtual int rewind();
	PPOpIn* op;
	tuple t;
};
class CreationSednaDataSource : public  SednaDataSource
{
public:
	CreationSednaDataSource(ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_,std::vector<xptr>* _first_nodes_);
private:
	virtual xptr get_next_doc();
	virtual int rewind();
	std::vector<xptr>* first_nodes;
	std::vector<xptr>::iterator it;
	xptr tmp;
};
class UpdateSednaDataSource : public  SednaDataSource
{
public:
	UpdateSednaDataSource(ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_,xptr_sequence * _seq_);
private:
	virtual xptr get_next_doc();
	virtual int rewind();
	xptr_sequence * seq;
	xptr_sequence::iterator it;
};

template <typename Iterator>
class SednaStringHighlighter
{
private:
	//dirty hack!!!!
#define HL_BUF_SIZE 256
	char buf[HL_BUF_SIZE];
	int buf_s;
	inline void flush_buf() {if (buf_s > 0) {result->append_mstr(buf, buf_s);buf_s = 0;} }
	inline void append_result(const char * str) { flush_buf(); result->append_mstr(str); } //FIXME: that's bad even for a dirty hack

	friend bool is_tag_char(int ch);
	int last_ch_len;
	int getch(Iterator &str_it, Iterator &str_end);
	void putch(const int ch);
	int cur_ch;
	int last_eff_ch; //last not-whitespace char thas is not tag name/attrib.
	static const int EOF_ch = -1;
	void copy_tag(Iterator &str_it, Iterator &str_end, bool copy=true);
	void copy_doc(Iterator &str_it, Iterator &str_end);
	void parse_tag();
	void parse_doc();
	bool hl_fragment;

	int ht_cnt;
	long *ht;
	long ht0; //current_word of first word in ht[]
	int current_word;
	int current_word_tok;
	int tag_l;
	int tag_w;
	int current_ht_idx;

	static const int min_words_before = 10;
	static const int max_words_before = 20;
	static const int min_words_after = 10;
	static const int max_words_after = 20;

	Iterator fragment_start;
	Iterator fragment_end;
	int fragment_start_word_tok_num;
	int fragment_start_word_num;
	bool fragment_start_split;
	bool fragment_end_split;
	int fragment_pref_ch; //FIXME: stupid workaround

	Iterator str_it;
	Iterator str_end;

	estr_buf *result;
public:
	
	SednaStringHighlighter(const Iterator &_str_it_, const Iterator &_str_end_, long* _ht_,long _ht_cnt_, bool _hl_fragment_, estr_buf *_result_);
	void run();
};

class SednaConvertJob
{
public:
	estr_buf result;
	SednaConvertJob(ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_, bool _hl_fragment_);
	void convert_node(xptr& node,long* ht,long ht_cnt);
	virtual void OnOutput(const char * txt, int length);
	static const int opentag_code = 0xE801;
	static const int closetag_code = 0xE802;
private:
	static const char * opentag_str;
	static const char * closetag_str;
	op_str_buf in_buf;
	SednaTextInputStream *tis;
	dtsFileInfo fileInfo;

	ft_index_type cm;
	pers_sset<ft_custom_cell,unsigned short>* custom_tree;
	bool hl_fragment;
};
class SednaSearchJob : public dtSearch::DSearchJob {
     public:
           
           virtual void OnError(long errorCode, const char *msg);
           virtual void OnFound(long totalFiles,
                 long totalHits, const char *name, long hitsInFile, dtsSearchResultsItem& item);
		   virtual void OnSearchingIndex(const char * indexPath);
		   SednaSearchJob(PPOpIn* _seq_,ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_,bool _hilight_=false, bool _hl_fragment_=false);		   
		   SednaSearchJob(bool _hilight_=false, bool _hl_fragment_=false);
		   void set_request(tuple_cell& request);
		   void set_file_cond_for_node(tuple_cell& node);
		   void get_next_result(tuple &t);
		   void set_index(ft_index_cell* ft_idx);
		   void reopen();
		   virtual ~SednaSearchJob();
#ifdef WIN32
		   static DWORD WINAPI ThreadFunc( void* lpParam );
#else
		   static void *ThreadFunc( void* lpParam );
#endif
	  private:
		  SednaUserException *thread_exception;
		  bool thread_up_semaphore_on_exception;
		  void stop_thread(bool ignore_errors);
		  
		  PPOpIn* seq;
		  UTHANDLE dtth;
		  UUnnamedSemaphore sem1,sem2;
		  long save_field_flags;
		  xptr res;
		  bool hilight;
		  bool hl_fragment;
		  SednaConvertJob * hl;
     };

//strict variant (with sort by relevance)
class SednaSearchJob2 {
     public:
		 //TODO: scan w/o index
		 //TODO: add highlight?
		   //SednaSearchJob2(PPOpIn* _seq_,ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_,bool _hilight_=false, bool _hl_fragment_=false);
		   SednaSearchJob2();
		   void set_request(tuple_cell& request);
		   void set_field_weights(tuple_cell& fw);
		   void get_next_result(tuple &t);
		   void set_index(ft_index_cell* ft_idx);
		   void set_max_results(long max_results);
		   void reopen();
		   virtual ~SednaSearchJob2();
	  private:
		  dtsSearchJob dts_job;
		  dtsSearchResults *dts_results;
		  long save_field_flags;
		  xptr res;
		  //TODO: consider removing these and using only fields in dts_job, check that they are NULL in the constructor
		  char *request;
		  char *field_weights;
		  char *indexesToSearch;
		  int res_id;
};



#endif
