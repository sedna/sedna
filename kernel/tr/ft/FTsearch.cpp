/*
 * File:  FTsearch.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/ft/FTsearch.h"
#include "tr/executor/base/PPBase.h"
#include "tr/tr_globals.h"
#include "tr/structures/nodeutils.h"
#include "tr/crmutils/ftserializer.h"

#define DTSEARCH_THREAD_STACK_SIZE (1024*1024)

/////////////////////////////////////////////////////////////
//
//  DSearchJob


class GenericDataSource {
    public:
        GenericDataSource();
        void makeInterface(dtsDataSource& dest);
        virtual ~GenericDataSource();
        virtual int getNextDoc(dtsInputStream& dest) = 0;
        virtual int rewind() = 0;
        static int getNextDocCB(void *pData, dtsInputStream& dest);
        static int rewindCB(void *pData);
        static GenericDataSource *safeCast(void *pData);
        void makeInterface(dtsInputStream& dest);
   };


//
//  A TextInputStream is a "document" to be indexed.
//  The SampleDataSource object will create a TextInputStream
//  for each document it wants to supply to the dtSearch Engine.
//  The dtSearch Engine will use the function pointers in a
//  dtsInputStream to access a TextInputStream.
//


const long TextInputStreamID = 0x01010101;


//
//  SampleDataSource is a data source based on a table of
//  records (below), each of which has an associated
//  line of text.
//

class SampleDataSource : public GenericDataSource {
    public:
        SampleDataSource(xptr _node_);
        virtual int getNextDoc(dtsInputStream& s);
        virtual int rewind();
        static void recordToFilename(char *dest,xptr _node_);
    protected:
        xptr node;
		int pos;
		op_str_buf in_buf;
    };
//
//  SampleDataSource is a data source based on a table of
//  records (below), each of which has an associated
//  line of text.
//



GenericDataSource::GenericDataSource()
{
}

GenericDataSource::~GenericDataSource()
{
}

int GenericDataSource::rewindCB(void *pData)
{   GenericDataSource *s = GenericDataSource::safeCast(pData);
    if (s)
        return s->rewind();
    else
        return -1;
}

int GenericDataSource::getNextDocCB(void *pData, dtsInputStream& dest)
{   GenericDataSource *s = GenericDataSource::safeCast(pData);
    if (s)
        return s->getNextDoc(dest);
    else
        return -1;
}

GenericDataSource *GenericDataSource::safeCast(void *pData)
{   GenericDataSource *s = (GenericDataSource *) pData;
    if (s)
        return s;
    return NULL;
}

void GenericDataSource::makeInterface(dtsDataSource& dest)
{   memset(&dest, 0, sizeof dest);
    dest.pData = this;
    dest.rewind = GenericDataSource::rewindCB;
    dest.getNextDoc = GenericDataSource::getNextDocCB;
}

SednaTextInputStream::SednaTextInputStream(dtsFileInfo* info,ft_index_type _cm_,ft_custom_tree_t* _custom_tree_) :
    cm(_cm_),
    custom_tree(_custom_tree_),
    idTextInputStream(TextInputStreamID),
	fileInfo(info),
	estr_it(NULL)
{
}

SednaTextInputStream::~SednaTextInputStream()
{
	if (estr_it != NULL)
		delete estr_it;
}

int SednaTextInputStream::read_mem(void *dest, long bytes)
{
	//FIXME: it's better to use data returned by fill_text_source in makeInterface, than invoke in_buf.get_size() and get_str_mem()
	if (bytes + pos >= in_buf.get_size())
		bytes = (long)in_buf.get_size() - pos;
    if (bytes > 0)
		memmove(dest, in_buf.get_str_mem() + pos, bytes);
    pos += bytes;
    return bytes;
}

void SednaTextInputStream::seek_mem(long where)
{
	pos = where;
}

int SednaTextInputStream::read_estr(void *dest, long bytes)
{
	if (bytes + pos > in_buf.get_size())
        bytes = (long)in_buf.get_size() - pos;
	long bytes_left = bytes;
    while (bytes_left-- > 0)
	{
		*(char*)dest = **estr_it;
		dest = (char*)dest + 1;
		++(*estr_it);
	}
    pos += bytes;
    return bytes;
}

void SednaTextInputStream::seek_estr(long where)
{
	if (pos > where)
		(*estr_it) -= (pos - where);
	else
		(*estr_it) += (where - pos);
	pos = where;
}

long SednaTextInputStream::readCBmem(void *pData, void *dest, long bytes)
{   SednaTextInputStream *s = SednaTextInputStream::safeCast(pData);
    if (s)
        return s->read_mem(dest, bytes);
    else
        return -1;
}

void SednaTextInputStream::seekCBmem(void *pData, long where)
{   SednaTextInputStream *s = SednaTextInputStream::safeCast(pData);
    if (s)
        s->seek_mem(where);
}

long SednaTextInputStream::readCBestr(void *pData, void *dest, long bytes)
{   SednaTextInputStream *s = SednaTextInputStream::safeCast(pData);
    if (s)
        return s->read_estr(dest, bytes);
    else
        return -1;
}

void SednaTextInputStream::seekCBestr(void *pData, long where)
{   SednaTextInputStream *s = SednaTextInputStream::safeCast(pData);
    if (s)
        s->seek_estr(where);
}

void SednaTextInputStream::releaseCB(void *pData)
{   /*SednaTextInputStream *s = safeCast(pData);
    if (s)
        delete s;*/
}

SednaTextInputStream *SednaTextInputStream::safeCast(void *pData)
{   SednaTextInputStream *s = (SednaTextInputStream *) pData;
    if (s && (s->idTextInputStream == TextInputStreamID))
        return s;
    return NULL;
}

void SednaTextInputStream::makeInterface(dtsInputStream& dest,xptr& node)
{
	in_buf.clear();
	dest.pData = this;
    dest.filename = fileInfo->filename;
	dest.typeId = it_XML;
	CHECKP(node);
	in_buf.append("<?xml version=\"1.0\" standalone=\"yes\" encoding=\"utf-8\">");
	FTSerializer::getSharedInstance()->printNodeToBuffer(node, &in_buf, cm, custom_tree);
	/*FILE *f = fopen("last_dts_file", "wb");
	fwrite(in_buf.c_str(), 1, in_buf.get_size(), f);
	fclose(f);*/
	pos = 0;
	dest.size = (long)in_buf.get_size();
	fileInfo->size=dest.size;
	text_source_t ts;
	in_buf.fill_text_source(&ts);
	if (ts.type == text_source_t::text_mem)
	{
	    dest.read = SednaTextInputStream::readCBmem;
		dest.seek = SednaTextInputStream::seekCBmem;
	}
	else
	{
		if (estr_it != NULL)
			delete estr_it;
		estr_it = se_new estr_iterator(dest.size, ts.u.data);
	    dest.read = SednaTextInputStream::readCBestr;
		dest.seek = SednaTextInputStream::seekCBestr;
	}
    dest.release = SednaTextInputStream::releaseCB;
    dest.modified.copy(fileInfo->modified);
    dest.created.copy(fileInfo->created);
}

SednaDataSource::SednaDataSource(ft_index_type _cm_,ft_custom_tree_t* _custom_tree_):
cm(_cm_),custom_tree(_custom_tree_)
{
	tis = se_new SednaTextInputStream(&fileInfo, cm ,custom_tree);

}
void SednaDataSource::recordToFilename(char *dest,xptr _node_)
{
	sprintf(dest, "%d-0x%x.xml", (int)(_node_.layer),(_node_.getOffs()));
}
xptr SednaDataSource::filenameToRecord(const char *dest)
{
	int layer;
	lsize_t addr;
	const char *fn = dest, *t = dest;
	while (*t)
	{
		if (*t == '/' || *t == '\\')
			fn = t+1;
		t++;
	}
	sscanf(fn, "%d-0x%x.xml", &layer, &addr);
	return indirectionDereferenceCP(cxptr(layer, addr));
}

int SednaDataSource::getNextDoc(dtsInputStream& dest)
{
	/*tuple t(1);
	seq->op->next(t);
		//Preliminary node analysis
   if (t.is_eos())
		return FAIL;
   tuple_cell& tc= t.cells[0];
	if (!tc.is_node())
	{
			throw USER_EXCEPTION(SE2031);
	}
    xptr node=tc.get_node();
	crmstringostream st;
	//FIXME: print xml verision & enc here??
	print_node_indent(node, st,xml);
	st<<'\x0';
	char* f=st.str();
    //in_buf.set(dm_string_value(node));
    dtsFileInfo fileInfo;
    recordToFilename(fileInfo.filename,node);
	fileInfo.size = strlen(f);
    fileInfo.modified.year = 1996;
    fileInfo.modified.month = 1;
    fileInfo.modified.day = 1;
	TextInputStream *s = se_new TextInputStream(fileInfo, f);
    s->makeInterface(dest);
     */
	xptr node=get_next_doc();
	if (node==XNULL) return -1;
	CHECKP(node);
	recordToFilename(fileInfo.filename,nodeGetIndirection(node));
//	fileInfo.size = strlen(f);
    fileInfo.modified.year = 1996;
    fileInfo.modified.month = 1;
    fileInfo.modified.day = 1;
    tis->makeInterface(dest,node);
	return 0;
}

int SednaDataSource::rewind()
{   //pos = 0;
    //if (pos) seq->op->reopen();
	return 0;
}

int SednaDataSource::getNextDocCB(void *pData, dtsInputStream& dest)
{
	SednaDataSource *ds = (SednaDataSource*)pData;
	return ds->getNextDoc(dest);
}
int SednaDataSource::rewindCB(void *pData)
{
	SednaDataSource *ds = (SednaDataSource*)pData;
	return ds->rewind();
}


CreationSednaDataSource::CreationSednaDataSource(ft_index_type _cm_,ft_custom_tree_t* _custom_tree_,std::vector<xptr>* _first_nodes_):SednaDataSource(_cm_,_custom_tree_),first_nodes(_first_nodes_),tmp(XNULL)
{
	it=first_nodes->begin();
}
xptr CreationSednaDataSource::get_next_doc()
{
	if (tmp==XNULL)
	{
		if (it!=first_nodes->end())
			tmp=*it;
		return tmp;
	}
	tmp=getNextDescriptorOfSameSort(tmp);
	if (tmp==XNULL)
	{
		if (++it==first_nodes->end())return XNULL;
		tmp=*it;
	}
	return tmp;
}
int CreationSednaDataSource::rewind()
{
	it=first_nodes->begin();
	tmp=XNULL;
	return 0;
}
UpdateSednaDataSource::UpdateSednaDataSource(ft_index_type _cm_,ft_custom_tree_t* _custom_tree_,xptr_sequence * _seq_):SednaDataSource(_cm_,_custom_tree_),seq(_seq_)
{
	it=seq->begin();
}
xptr UpdateSednaDataSource::get_next_doc()
{
	while (it!=seq->end())
	{
		xptr ptr = indirectionDereferenceCP(*it++);
		if (ptr != XNULL)
			return ptr;
	}
	return XNULL;
}
int UpdateSednaDataSource::rewind()
{
	it=seq->begin();
	return 0;
}
OperationSednaDataSource::OperationSednaDataSource(ft_index_type _cm_,ft_custom_tree_t* _custom_tree_,PPOpIn* _op_):SednaDataSource(_cm_,_custom_tree_),op(_op_),t(1)
{
}
xptr OperationSednaDataSource::get_next_doc()
{
   op->op->next(t);
		//Preliminary node analysis
   if (t.is_eos())
		return XNULL;
   tuple_cell& tc= t.cells[0];
   if (!tc.is_node())
   {
	   throw USER_EXCEPTION2(XPTY0004, "atomic values are not supported in full-text queries");
   }
   return tc.get_node();
}
int OperationSednaDataSource::rewind()
{
	op->op->reopen();
	return 0;
}
/*
/*void SednaSearchJob::reopen()
{
	seq->op->reopen();
	dtth=NULL;
}*/ /*
SednaSearchJob::~SednaSearchJob()
{
	if (dtth != NULL)
		this->stop_thread(false);
}
void SednaSearchJob::OnSearchingIndex(const char * indexPath)
{
	//std::cout<<"Searching: "<<indexPath;
}
*/

SednaSearchJobBase::~SednaSearchJobBase()
{
	if (this->indexesToSearch)
		free(this->indexesToSearch);
	if (this->request)
		free(this->request);
	if (this->file_cond)
		free(this->file_cond);
}

void SednaSearchJobBase::set_index(ft_index_cell_object* ft_idx)
{
#ifdef _WIN32
	std::string index_path1 = std::string(SEDNA_DATA) + std::string("\\data\\")
		+ std::string(tr_globals::db_name) + std::string("_files\\dtsearch\\");
#else
	std::string index_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(tr_globals::db_name) + std::string("_files/dtsearch/");
#endif
	std::string index_path = index_path1 + std::string(ft_idx->index_title);

	if (this->indexesToSearch)
		free(this->indexesToSearch);
	this->indexesToSearch = (char*)malloc(index_path.length() + 2);
	strcpy(this->indexesToSearch, index_path.c_str());
	this->indexesToSearch[index_path.length() + 1] = 0;

	dts_job.indexesToSearch = this->indexesToSearch;
	dts_job.action.searchIndexes = true;

	//FIXME: choose where it's better to do this - here or in constructor
	//if (hilight)
	//	hl=se_new SednaConvertJob(ft_idx->ftype,ft_idx->custom_tree, hl_fragment);
}

void SednaSearchJobBase::set_request(tuple_cell& request)
{
	op_str_buf buf(request);

	if (this->request)
		free(this->request);
	this->request = (char*)malloc((size_t)buf.get_size() + 1);
	strcpy(this->request, buf.c_str()); //FIXME: c_str call may be replaced with copy_to_buf (currently not implemented)

	this->dts_job.request2 = this->request;
}

void SednaSearchJobBase::set_file_cond_for_node(tuple_cell& node)
{
	char buf[64];

	Node nd = node.get_node();
	nd.checkp();
	SednaDataSource::recordToFilename(buf,nd.getIndirection());
	std::string fc = std::string("xfilter(name \"") + buf + "\")";
	int len = fc.length();
	if (this->file_cond)
		free(this->file_cond);
	this->file_cond = (char*)malloc(len + 1);
	memcpy(this->file_cond, fc.c_str(), len+1);

	dts_job.fileConditions2 = this->file_cond;
}


SednaSearchJob::SednaSearchJob(PPOpIn* _seq_,ft_index_type _cm_,ft_custom_tree_t* _custom_tree_,bool _hilight_, bool _hl_fragment_):seq(_seq_), hilight(_hilight_), hl_fragment(_hl_fragment_),
thread_exception(NULL), thread_up_semaphore_on_exception(true), data_source(NULL), res(XNULL), SednaSearchJobBase()
{
	this->data_source = se_new OperationSednaDataSource(_cm_,_custom_tree_,_seq_);
	dts_job.dataSourceToSearch = this->data_source->getInterface();
	dts_job.action.searchFiles = true;

	dtth=NULL;
	if (hilight)
	{
		if (_cm_ == ft_xml_hl)
			hl=se_new SednaConvertJob(ft_xml, _custom_tree_, hl_fragment);
		else
			hl=se_new SednaConvertJob(_cm_,_custom_tree_, hl_fragment);
	}
}

SednaSearchJob::SednaSearchJob(bool _hilight_, bool _hl_fragment_):seq(NULL),hilight(_hilight_),hl_fragment(_hl_fragment_),
													thread_exception(NULL), thread_up_semaphore_on_exception(true), data_source(NULL), res(XNULL), SednaSearchJobBase()

{
	dtth=NULL;
	if (hilight)
	{
		//FIMXE: check cm?
		hl=se_new SednaConvertJob(ft_xml, NULL, hl_fragment);
	}
}

SednaSearchJob::~SednaSearchJob()
{
	if (this->data_source)
		delete this->data_source;
}

void SednaSearchJob::set_dtsSearchAnyWords(bool v)
{
	if (v)
		dts_job.searchFlags2 |= dtsSearchAnyWords;
    else
        dts_job.searchFlags2 &= (~dtsSearchAnyWords);

    dts_job.searchFlags = (short)(dts_job.searchFlags2 & 0xffff);
}

void SednaSearchJob::set_dtsSearchAllWords(bool v)
{
	if (v)
		dts_job.searchFlags2 |= dtsSearchAllWords;
    else
        dts_job.searchFlags2 &= (~dtsSearchAllWords);

    dts_job.searchFlags = (short)(dts_job.searchFlags2 & 0xffff);
}

int SednaSearchJob::reportCB(void *pReportData, dtsMessage& message)
{
	SednaSearchJob *ssj = (SednaSearchJob*)pReportData;
	switch (message.command)
	{
	case dtsnSearchFound:
		{
		long hitsInFile = (long)message.paramA;
		const char *name = message.strParam;
		dtsSearchResultsItem *item = (dtsSearchResultsItem*)message.paramB;

		ssj->res = SednaDataSource::filenameToRecord(name);
		if (ssj->hilight)
		{
			xptr ptr = SednaDataSource::filenameToRecord(name);
			ssj->hl->convert_node(ptr,item->hits,item->hitCount);
		}

		UUnnamedSemaphoreUp(&ssj->sem1, __sys_call_error);
		ssj->thread_up_semaphore_on_exception = false;
		UUnnamedSemaphoreDown(&ssj->sem2, __sys_call_error);
		ssj->thread_up_semaphore_on_exception = true;

		message.result = dtsVetoSearchResultsItem;
		break;
		}
	default:
		break;
	}
	if (ssj->cancel_job)
		return dtsAbortImmediate;
	return dtsContinue;
}

void SednaSearchJob::execute()
{
	short fail_flag;

	dts_job.pReportCallBack = &SednaSearchJob::reportCB;
	dts_job.pReportData = this;
	this->cancel_job = false;

	dtssDoSearchJob(dts_job, fail_flag);

	//FIXME: check failflag
}

void SednaSearchJob::stop_thread(bool ignore_errors)
{
	if (dtth != NULL)
	{
		this->cancel_job = true;
		if (UUnnamedSemaphoreUp(&sem2, __sys_call_error) != 0)
			throw USER_EXCEPTION(SE4014);

		if (uThreadJoin(dtth, __sys_call_error) != 0)
			throw USER_EXCEPTION2(SE4064, "failed to join dtsearch thread"); //FIXME: this error code is (probably) wrong
		if (uCloseThreadHandle(dtth, __sys_call_error) != 0)
			throw USER_EXCEPTION(SE4063);
		dtth = NULL;
	}
	//FIXME!!!
	if (UUnnamedSemaphoreRelease(&sem1, __sys_call_error) != 0 && !ignore_errors)
		throw USER_EXCEPTION(SE4013);
	if (UUnnamedSemaphoreRelease(&sem2, __sys_call_error) != 0 && !ignore_errors)
		;//throw USER_EXCEPTION(SE4013);

}

#ifdef WIN32
DWORD WINAPI SednaSearchJob::ThreadFunc( void* lpParam )
#else
void *SednaSearchJob::ThreadFunc( void* lpParam )
#endif
{
	try
	{
		//if (((SednaSearchJob*)lpParam)->hilight)
		{
			//FIXME
			dtsOptions opts;
			short result;
			dtssGetOptions(opts, result);
			((SednaSearchJob*)lpParam)->save_field_flags = opts.fieldFlags;
			//opts.fieldFlags |= dtsoFfXmlSkipAttributes  | dtsoFfXmlHideFieldNames | dtsoFfSkipFilenameField;
			opts.fieldFlags = dtsoFfXmlHideFieldNames | dtsoFfSkipFilenameField | dtsoFfXmlSkipAttributes;
			std::string stemming_file = std::string(SEDNA_DATA) + std::string("/data/")
			                        + std::string(tr_globals::db_name) + std::string("_files/dtsearch/stemming.dat");

			strcpy(opts.stemmingRulesFile, stemming_file.c_str());

			dtssSetOptions(opts, result);
		}
		((SednaSearchJob*)lpParam)->execute();
		/*
		if (((SednaSearchJob*)lpParam)->GetErrorCount()>0)
		{
			const dtsErrorInfo * ptr= ((SednaSearchJob*)lpParam)->GetErrors();
			//for (int i=0;i<ptr->getCount();i++)
			//	std::cout<<ptr->getMessage(i);
		}*/

		((SednaSearchJob*)lpParam)->res = XNULL;
		UUnnamedSemaphoreUp(&(((SednaSearchJob*)lpParam)->sem1), __sys_call_error);
	}
	catch (SednaUserException e)
	{
		((SednaSearchJob*)lpParam)->res = XNULL;
		if (((SednaSearchJob*)lpParam)->thread_up_semaphore_on_exception)
			UUnnamedSemaphoreUp(&(((SednaSearchJob*)lpParam)->sem1), __sys_call_error);
		((SednaSearchJob*)lpParam)->thread_exception = se_new SednaUserException(e);
	}
	return 0;
}

void SednaSearchJob::get_next_result(tuple &t)
{
	if (dtth==NULL)
	{
		if (UUnnamedSemaphoreCreate(&sem1, 0, NULL, __sys_call_error) != 0)
			throw USER_EXCEPTION(SE4010);
		if (UUnnamedSemaphoreCreate(&sem2, 0, NULL, __sys_call_error) != 0)
		{
			UUnnamedSemaphoreRelease(&sem1, __sys_call_error);
			throw USER_EXCEPTION(SE4010);
		}
        uResVal rval = uCreateThread(
			ThreadFunc,                  // thread function
			this,						 // argument to thread function
			&dtth,                       // use default creation flags
			DTSEARCH_THREAD_STACK_SIZE, NULL, __sys_call_error);
		if (rval != 0)
		{
			UUnnamedSemaphoreRelease(&sem1, __sys_call_error);
			UUnnamedSemaphoreRelease(&sem2, __sys_call_error);
			throw USER_EXCEPTION(SE4060);
		}
	}
	else
	{
		if (UUnnamedSemaphoreUp(&sem2, __sys_call_error) != 0)
		{
			this->stop_thread(true);
			throw USER_EXCEPTION(SE4014);
		}
	}
	if (UUnnamedSemaphoreDown(&sem1, __sys_call_error) != 0)
	{
		this->stop_thread(true);
		throw USER_EXCEPTION(SE4015);
	}
    if (res==XNULL)
	{
		/*
		dtsOptions opts;
		short result;
		dtssGetOptions(opts, result);
		opts.fieldFlags = save_field_flags;
		dtssSetOptions(opts, result);*/
		t.set_eos();
		int res1 = UUnnamedSemaphoreRelease(&sem1, __sys_call_error);
		int res2 = UUnnamedSemaphoreRelease(&sem2, __sys_call_error);

		if (uThreadJoin(dtth, __sys_call_error) != 0)
			throw USER_EXCEPTION2(SE4064, "failed to join ftsearch thread"); //FIXME: this error code is (probably) wrong
		if (uCloseThreadHandle(dtth, __sys_call_error) != 0)
			throw USER_EXCEPTION(SE4063);
		dtth = NULL;

		if (res1 != 0 || res2 != 0)
			throw USER_EXCEPTION(SE4013);


		if (thread_exception != NULL)
			throw SednaUserException(*thread_exception);
	}
	else
	{
		if (!hilight)
			t.copy(tuple_cell::node(res));
		else
			t.copy(hl->result.content());
	}
}


static int cmp_long(const void *a, const void *b)
{
	return int(*((long*)a) - *((long*)b));
}

template <class Iterator>
SednaStringHighlighter<Iterator>::SednaStringHighlighter(const Iterator &_str_it_,
														 const Iterator &_str_end_,
														 long* _ht_,
														 long _ht_cnt_,
														 bool _hl_fragment_,
														 estr_buf *_result_) :
									str_it(_str_it_),
									str_end(_str_end_),
									ht(_ht_),
									ht_cnt(_ht_cnt_),
									hl_fragment(_hl_fragment_),
									result(_result_)

{
	result->reinit();
	buf_s = 0;
	qsort(ht, ht_cnt, sizeof(long), cmp_long);
}


template <typename Iterator>
void SednaStringHighlighter<Iterator>::run()
{
	current_word = 0;
	current_word_tok = 0;
	current_ht_idx = 0;
	tag_l = 0;
	tag_w = 0;

	if (hl_fragment)
	{
		ht0 = -1;
		Iterator str_it_save = str_it;
		Iterator str_end_save = str_end;
		//first parse_doc pass: compute ht0
		if (ht_cnt == 0)
			ht0 = 1;
		else
		{
			cur_ch = getch(str_it, str_end);
			parse_doc();
		}

		//second pass: get the fragment
		str_it = str_it_save;
		str_end = str_end_save;
		current_word = 0;
		current_word_tok = 0;
		current_ht_idx = 0;
		tag_l = 0;
		tag_w = 0;

		cur_ch = getch(str_it, str_end);
		parse_doc();

		if (fragment_pref_ch == 0)
			cur_ch = getch(fragment_start, fragment_end);
		else
			cur_ch = fragment_pref_ch;
		current_word_tok = fragment_start_word_tok_num;
		current_ht_idx = 0; //FIXME?
		if (fragment_start_split)
			append_result("...");
		copy_doc(fragment_start, fragment_end);
		if (fragment_end_split)
			append_result("...");
	}
	else
	{
		cur_ch = getch(str_it, str_end);
		copy_doc(str_it, str_end);
	}
	flush_buf();
}

SednaConvertJob::SednaConvertJob(ft_index_type _cm_,ft_custom_tree_t* _custom_tree_, bool _hl_fragment_) :
						cm(_cm_), custom_tree(_custom_tree_), hl_fragment(_hl_fragment_)
	{
}

template <class Iterator>
int SednaStringHighlighter<Iterator>::getch(Iterator &str_it, Iterator &str_end)
{
	unsigned char ch=*(str_it);
	int r;

	if (str_it >= str_end)
		return -1;

	//FIXME - check str_end

	++str_it;
	if (ch < 128)
	{
			r = ch;
			last_ch_len = 1;
	}
	else if (ch < 224) //FIXME: ch mustbe >= 192
	{
		r = ch - 192; r <<= 6;
		ch = *(str_it); r += ch - 128; ++str_it;
		last_ch_len = 2;
	}
	else if (ch < 240)
	{
		r = ch - 224; r <<= 6;
		ch = *(str_it); r += ch - 128; r <<= 6; ++str_it;
		ch = *(str_it); r += ch - 128; ++str_it;
		last_ch_len = 3;
	}
	else if (ch < 248)
	{
		r = ch - 240; r <<= 6;
		ch = *(str_it); r += ch - 128; r <<= 6; ++str_it;
		ch = *(str_it); r += ch - 128; r <<= 6; ++str_it;
		ch = *(str_it); r += ch - 128; ++str_it;
		last_ch_len = 4;
	}
	else if (ch < 252)
	{
		r = ch - 248; r <<= 6;
		ch = *(str_it); r += ch - 128; r <<= 6; ++str_it;
		ch = *(str_it); r += ch - 128; r <<= 6; ++str_it;
		ch = *(str_it); r += ch - 128; r <<= 6; ++str_it;
		ch = *(str_it); r += ch - 128; ++str_it;
		last_ch_len = 5;
	}
	else // ch mustbe < 254
	{
		r = ch - 252; r <<= 6;
		ch = *(str_it); r += ch - 128; r <<= 6; ++str_it;
		ch = *(str_it); r += ch - 128; r <<= 6; ++str_it;
		ch = *(str_it); r += ch - 128; r <<= 6; ++str_it;
		ch = *(str_it); r += ch - 128; r <<= 6; ++str_it;
		ch = *(str_it); r += ch - 128; ++str_it;
		last_ch_len = 6;
	}
	return r;
}

template <class Iterator>
void SednaStringHighlighter<Iterator>::putch(const int ch)
{
	if (buf_s > HL_BUF_SIZE-10)
		flush_buf();
	if (ch < (1 << 7)) {
		buf[buf_s++] = ch;
    } else if (ch < (1 << 11)) {
		buf[buf_s++] = ((ch >> 6) | 0xc0);
		buf[buf_s++] = ((ch & 0x3f) | 0x80);
    } else if (ch < (1 << 16)) {
		buf[buf_s++] = ((ch >> 12) | 0xe0);
		buf[buf_s++] = (((ch >> 6) & 0x3f) | 0x80);
		buf[buf_s++] = ((ch & 0x3f) | 0x80);
    } else if (ch < (1 << 21)) {
		buf[buf_s++] = ((ch >> 18) | 0xf0);
		buf[buf_s++] = (((ch >> 12) & 0x3f) | 0x80);
		buf[buf_s++] = (((ch >> 6) & 0x3f) | 0x80);
		buf[buf_s++] = ((ch & 0x3f) | 0x80);
    }
}


static inline bool is_tag_char(int ch)
{
	return ((!iswpunct(ch) && !iswspace(ch) && ch != SednaConvertJob::opentag_code && ch != SednaConvertJob::closetag_code && ch != SednaStringHighlighter<char*>::EOF_ch) || ch == '-');
}


inline static bool iswordchar(int ch);

//part of word if inside, but does not make a word by itself
//must be false for EOF and special symbols (<>)
inline static bool iswordsep(int ch)
{
	return (ch >= 768) && (ch <= 866);
}

template <class Iterator>
void SednaStringHighlighter<Iterator>::copy_tag(Iterator &str_it, Iterator &str_end, bool copy)
{
	U_ASSERT(cur_ch == SednaConvertJob::opentag_code);
	if (copy && !hl_fragment)
		putch(cur_ch);
	cur_ch = getch(str_it, str_end);
	if (cur_ch == '/')
	{
		if (copy && !hl_fragment)
			putch(cur_ch);
		cur_ch = getch(str_it, str_end);

		tag_l--;
		if (tag_l == 1)
		{
			current_word_tok += tag_w;
			while (current_ht_idx < ht_cnt && ht[current_ht_idx] <= current_word_tok)
					current_ht_idx++;
			tag_w = 0;
		}
	}
	else
	{
		tag_l++;
//		tag_w++;
		/*
		current_word_tok ++;
		while (current_ht_idx < ht_cnt && ht[current_ht_idx] <= current_word_tok)
				current_ht_idx++;*/
	}


	if (is_tag_char(cur_ch))
	{
		while (is_tag_char(cur_ch))
		{
			if (copy && !hl_fragment)
				putch(cur_ch);
			cur_ch = getch(str_it, str_end);
		}
	}
	do
	{
		if (iswordchar(cur_ch) && cur_ch != SednaConvertJob::opentag_code && cur_ch != EOF_ch)
		{
			while (iswordchar(cur_ch) && cur_ch != SednaConvertJob::opentag_code && cur_ch != EOF_ch)
			{
				if (copy && !hl_fragment)
					putch(cur_ch);
				cur_ch = getch(str_it, str_end);
			}

			while (iswspace(cur_ch) && cur_ch != EOF_ch)
			{
				if (copy && !hl_fragment)
					putch(cur_ch);
				cur_ch = getch(str_it, str_end);
			}
			//if (cur_ch == '=')
			{
//		tag_w++;
				/*
		current_word_tok ++;
		while (current_ht_idx < ht_cnt && ht[current_ht_idx] <= current_word_tok)
				current_ht_idx++;*/
			}
		}
		else if (cur_ch == '/')
		{
			if (copy && !hl_fragment)
				putch(cur_ch);
			cur_ch = getch(str_it, str_end);
			if (cur_ch == SednaConvertJob::closetag_code)
			{
				tag_l--;//FIXME
				break;
			}
		}
		else
		{
			if (cur_ch == EOF_ch || cur_ch == SednaConvertJob::closetag_code)
				break;
			if (copy && !hl_fragment)
				putch(cur_ch);
			cur_ch = getch(str_it, str_end);
		}
	} while (true);
	if (copy && !hl_fragment)
		putch(cur_ch);
	cur_ch = getch(str_it, str_end);
}

template <class Iterator>
void SednaStringHighlighter<Iterator>::parse_tag()
{
	copy_tag(str_it, str_end, false);
}

template <class Iterator>
void SednaStringHighlighter<Iterator>::copy_doc(Iterator &str_it, Iterator &str_end)
{
	while (cur_ch != EOF_ch)
	{
		if (cur_ch == SednaConvertJob::opentag_code)
		{
			copy_tag(str_it, str_end);
		}
		else if (!iswordchar(cur_ch))
		{
			putch(cur_ch);
			cur_ch = getch(str_it, str_end);
		}
		else
		{
			bool hl_word = false;
			int sep = 0;
			while (iswordsep(cur_ch))
			{
				if (sep != 0)
					putch(sep);
				sep = cur_ch;
				cur_ch = getch(str_it, str_end);
			}
			if (iswordchar(cur_ch))
			{
				current_word++;
				current_word_tok++;
				if (current_ht_idx < ht_cnt && ht[current_ht_idx] == current_word_tok)
				{
					hl_word = true;
					current_ht_idx++;
				}
				if (hl_word)
				{
					putch(SednaConvertJob::opentag_code);
					append_result("hit");
					putch(SednaConvertJob::closetag_code);
				}
				if (sep != 0)
					putch(sep);
				while (iswordchar(cur_ch) && cur_ch != SednaConvertJob::opentag_code && cur_ch != EOF_ch)
				{
					putch(cur_ch);
					cur_ch = getch(str_it, str_end);
				}
				if (hl_word)
				{
					putch(SednaConvertJob::opentag_code);
					append_result("/hit");
					putch(SednaConvertJob::closetag_code);
				}
			}
			else
			{
				if (sep != 0)
					putch(sep);
			}
		}
	}
}



#include "iswordchar.inc"

template <class Iterator>
void SednaStringHighlighter<Iterator>::parse_doc()
{
	fragment_pref_ch = 0;
	fragment_start = str_it;
	fragment_start_split = false;
	U_ASSERT(current_word_tok == 0);
	U_ASSERT(current_word == 0);
	fragment_start_word_tok_num = -1;
	fragment_start_word_num = -1;
	last_eff_ch = 0;

	//FIXME: I have no idea what's the difference between current_word and current_word_tok, and why 2 passes are needed here

	while (cur_ch != EOF_ch)
	{
		if (cur_ch == SednaConvertJob::opentag_code)
		{
			parse_tag();
		}
		else if (!iswordchar(cur_ch))
		{
			if (!iswspace(cur_ch))
				last_eff_ch = cur_ch;
			cur_ch = getch(str_it, str_end);
		}
		else
		{
			bool hl_word = false;
			current_word++;
			current_word_tok++;
			if (current_ht_idx < ht_cnt && ht[current_ht_idx] == current_word_tok)
			{
				if (ht0 == -1)
				{
					ht0 = current_word;
					//FIXME: return should return through recursive calls (but there are none of these here, for now)
					return; //end of the first pass
				}
				current_ht_idx++;
			}
			if (ht0 != -1) //FIXME?
			{
			if ((last_eff_ch == '.' && iswupper(cur_ch)) || last_eff_ch == 0)
			{ //sentence start
				if (current_word <= ht0 - min_words_before || fragment_start_word_num == -1)
				{
					fragment_start = str_it;
					fragment_start_word_tok_num = current_word_tok-1;
					fragment_start_word_num = current_word-1;
					fragment_pref_ch = cur_ch;
					fragment_start_split = false;
				}
				else
				{
					if (current_word >= ht0 + min_words_after)
					{
						//cut here
						fragment_end = str_it;
						fragment_end -= last_ch_len;
						fragment_end_split = false;
						//FIXME: return should return through recursive calls (but there are none of these here, for now)
						return;
					}
				}
			}
			else
			{
				if (current_word <= ht0)
				{
					if (current_word == ht0 - max_words_before) //FIXME
					{
						fragment_start = str_it;
						fragment_start_word_tok_num = current_word_tok-1;
						fragment_start_word_num = current_word-1;
						fragment_pref_ch = cur_ch;
						fragment_start_split = true;
					}
				}
				else
				{
					if (current_word == ht0 + max_words_after) //FIXME
					{
						//cut here
						fragment_end = str_it;
						fragment_end -= last_ch_len;
						fragment_end_split = true;
						//FIXME: return should return through recursive calls (but there are none of these here, for now)
						return;
					}
				}
			}
			}
			last_eff_ch = cur_ch; //no need to update it in the loop, we only need to know it's not punctuation
			while (iswordchar(cur_ch) && cur_ch != SednaConvertJob::opentag_code && cur_ch != EOF_ch)
				cur_ch = getch(str_it, str_end);
		}
	}
	fragment_end = str_it;
	fragment_end_split = false;
}

const char * SednaConvertJob::opentag_str = "\xEE\xA0\x81";
const char * SednaConvertJob::closetag_str = "\xEE\xA0\x82";

void SednaConvertJob::convert_node(xptr &node,long* _ht_,long _ht_cnt_)
{
	text_source_t ts;
	in_buf.clear();
	CHECKP(node);
	FTSerializer::getSharedInstance()->printNodeToBuffer(node, &in_buf, cm, custom_tree, opentag_str, closetag_str);
	in_buf.fill_text_source(&ts);
	if (ts.type == text_source_t::text_mem)
	{
		const char *str_it = ts.u.cstr;
		const char *str_it_end = str_it + get_text_size(ts);
		SednaStringHighlighter<const char *> hl(str_it, str_it_end, _ht_, _ht_cnt_, hl_fragment, &result);
		hl.run();
	}
	else
	{
		estr_iterator estr_it(get_text_size(ts), ts.u.data);
		estr_iterator estr_it_end(0, ts.u.data);
		SednaStringHighlighter<estr_iterator> hl(estr_it, estr_it_end, _ht_, _ht_cnt_, hl_fragment, &result);
		hl.run();
	}

}
void SednaConvertJob::OnOutput(const char * txt, int length)
{
	result.append_mstr(txt,length);
}

/////////////////////
// SednaSearchJob2
/////////////////////

SednaSearchJob2::SednaSearchJob2() : field_weights(NULL), dts_results(NULL), res_id(-1), SednaSearchJobBase()
{
}

void SednaSearchJob2::set_field_weights(tuple_cell& fw)
{
	op_str_buf buf(fw);

	if (this->field_weights)
		free(this->field_weights);
	this->field_weights = (char*)malloc((size_t)buf.get_size() + 1);
	strcpy(this->field_weights, buf.c_str()); //FIXME: c_str call may be replaced with copy_to_buf (currently not implemented)

	this->dts_job.fieldWeights = this->field_weights;
}

void SednaSearchJob2::get_next_result(tuple &t)
{
	if (!dts_results)
	{
		{
			//FIXME
			dtsOptions opts;
			short result;
			dtssGetOptions(opts, result);
			this->save_field_flags = opts.fieldFlags;
			//opts.fieldFlags |= dtsoFfXmlSkipAttributes  | dtsoFfXmlHideFieldNames | dtsoFfSkipFilenameField;
			opts.fieldFlags = dtsoFfXmlHideFieldNames | dtsoFfSkipFilenameField | dtsoFfXmlSkipAttributes;
			std::string stemming_file = std::string(SEDNA_DATA) + std::string("/data/")
									+ std::string(tr_globals::db_name) + std::string("_files/dtsearch/stemming.dat");

			strcpy(opts.stemmingRulesFile, stemming_file.c_str());

			dtssSetOptions(opts, result);
		}

		//TODO!: check results as filter stuff in dtsearch
		//TODO!: check delay doc info stuff in dtsearch

		dts_results = new dtsSearchResults();
		dts_job.resultsHandle = dts_results->getHandle();
		short errorFlag;
		dtssDoSearchJob(dts_job, errorFlag);
		//TODO: check errors

		dts_results->sort(dtsSortByRelevanceScore, NULL);

		this->res_id = 0;
	}

	if (res_id >= dts_results->getCount())
	{
		delete dts_results;
		dts_results = NULL;
		this->res_id = -1;

		t.set_eos();
	}
	else
	{
		dtsSearchResultsItem res_item;
		if (dts_results->getDocInfo(res_id, res_item) < 0) //FIXME: apiRef says nothing about getDocInfo return code, but c++ samples use it
			throw USER_EXCEPTION2(SE1003, "dtsearch: getDocInfo failed");
		res_id++;

		t.copy(tuple_cell::node(SednaDataSource::filenameToRecord(res_item.filename)));
	}
}

void SednaSearchJob2::set_max_results(long max_results)
{
	dts_job.maxFilesToRetrieve2 = max_results;
}

/*void SednaSearchJob2::reopen()
{
	//TODO: check what is this function supposed to do
}*/

SednaSearchJob2::~SednaSearchJob2()
{
	if (this->field_weights)
		free(this->field_weights);
	if (this->dts_results)
		delete this->dts_results;
}
