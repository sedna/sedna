#include "FTsearch.h"
#include "tr_globals.h"
using namespace dtSearch;

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
		t_str_buf in_buf;
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
        return FAIL;
}

int GenericDataSource::getNextDocCB(void *pData, dtsInputStream& dest)
{   GenericDataSource *s = GenericDataSource::safeCast(pData);
    if (s)
        return s->getNextDoc(dest);
    else
        return FAIL;
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

SednaTextInputStream::SednaTextInputStream(dtsFileInfo* info,ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_) :
    cm(_cm_),
    custom_tree(_custom_tree_),
    idTextInputStream(TextInputStreamID),
	fileInfo(info)
	{   
	 
    
}

SednaTextInputStream::~SednaTextInputStream()
{   
}

int SednaTextInputStream::read_mem(void *dest, long bytes)
{
	if (bytes + pos >= in_buf.get_size())
		bytes = in_buf.get_size() - pos;
    if (bytes > 0)
		memmove(dest, (char*)in_buf.get_ptr_to_text() + pos, bytes);
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
        bytes = in_buf.get_size() - pos;
	long bytes_left = bytes;
    while (bytes_left-- > 0)
	{
		*(char*)dest = *estr_it;
		dest = (char*)dest + 1;
		++estr_it;
	}
    pos += bytes;
    return bytes;
}

void SednaTextInputStream::seek_estr(long where)
{   
	if (pos > where)
		estr_it -= (pos - where);
	else
		estr_it += (where - pos);
	pos = where;
}

long SednaTextInputStream::readCBmem(void *pData, void *dest, long bytes)
{   SednaTextInputStream *s = SednaTextInputStream::safeCast(pData);
    if (s)
        return s->read_mem(dest, bytes);
    else
        return FAIL;
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
        return FAIL;
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
	CHECKP(node);
	print_node_to_buffer(node,in_buf,cm,custom_tree);
	pos = 0;
	dest.size = in_buf.get_size();
	fileInfo->size=dest.size;
	if (in_buf.get_type() == text_mem)
	{
	    dest.read = SednaTextInputStream::readCBmem;
		dest.seek = SednaTextInputStream::seekCBmem;
	}
	else
	{
		estr_it = e_string_iterator_first(dest.size, *(xptr*)in_buf.get_ptr_to_text());
	    dest.read = SednaTextInputStream::readCBestr;
		dest.seek = SednaTextInputStream::seekCBestr;
	}
    dest.release = SednaTextInputStream::releaseCB;
    dest.modified.copy(fileInfo->modified);
    dest.created.copy(fileInfo->created);
}

SednaDataSource::SednaDataSource(ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_):
cm(_cm_),custom_tree(_custom_tree_)
{
	tis = new SednaTextInputStream(&fileInfo, cm ,custom_tree);
	
}
void SednaDataSource::recordToFilename(char *dest,xptr _node_)
{    
	sprintf(dest, "%d-0x%x.xml", (int)(_node_.layer),(unsigned int)(_node_.addr));
}
xptr SednaDataSource::filenameToRecord(const char *dest)
{   
	int layer;
	unsigned int addr;
	const char *fn = dest, *t = dest;
	while (*t)
	{
		if (*t == '/' || *t == '\\')
			fn = t+1;
		t++;
	}
	sscanf(fn, "%d-0x%x.xml", &layer, &addr);
	return removeIndirection(xptr(layer, (void*)addr));
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
	TextInputStream *s = new TextInputStream(fileInfo, f);
    s->makeInterface(dest);
     */
	xptr node=get_next_doc();
	if (node==XNULL) return FAIL;
	CHECKP(node);
	recordToFilename(fileInfo.filename,((n_dsc*)XADDR(node))->indir);
//	fileInfo.size = strlen(f);
    fileInfo.modified.year = 1996;
    fileInfo.modified.month = 1;
    fileInfo.modified.day = 1;	
    tis->makeInterface(dest,node);
	return SUCCESS;
}

int SednaDataSource::rewind()
{   //pos = 0;
    //if (pos) seq->op->reopen();
	return SUCCESS;    
}
CreationSednaDataSource::CreationSednaDataSource(ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_,std::vector<xptr>* _first_nodes_):SednaDataSource(_cm_,_custom_tree_),first_nodes(_first_nodes_),tmp(XNULL)
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
	tmp=getNextDescriptorOfSameSortXptr(tmp);
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
	return SUCCESS;
}
UpdateSednaDataSource::UpdateSednaDataSource(ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_,xptr_sequence * _seq_):SednaDataSource(_cm_,_custom_tree_),seq(_seq_)
{
	it=seq->begin();
}
xptr UpdateSednaDataSource::get_next_doc()
{
	if (it==seq->end())
		return XNULL;
	else
		return removeIndirection(*it++);
}
int UpdateSednaDataSource::rewind()
{
	it=seq->begin();
	return SUCCESS;
}
OperationSednaDataSource::OperationSednaDataSource(ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_,PPOpIn* _op_):SednaDataSource(_cm_,_custom_tree_),op(_op_),t(1)
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
	   throw USER_EXCEPTION(SE2031);
   }
   return tc.get_node();
}
int OperationSednaDataSource::rewind()
{
	op->op->reopen();
	return SUCCESS;
}
void SednaSearchJob::OnError(long errorCode, const char *msg)
{
}
void SednaSearchJob::OnFound(long totalFiles,
                 long totalHits, const char *name, long hitsInFile, dtsSearchResultsItem& item)
{
	DSearchJob::OnFound(totalFiles, totalHits, name, hitsInFile, item);
	DSearchJob::VetoThisItem();
	res = SednaDataSource::filenameToRecord(name);
	UUnnamedSemaphoreUp(&sem1);
	UUnnamedSemaphoreDown(&sem2);
}
SednaSearchJob::SednaSearchJob(PPOpIn* _seq_,ft_index_type _cm_,pers_sset<ft_custom_cell,unsigned short>* _custom_tree_):seq(_seq_)
{
	AttachDataSource(new OperationSednaDataSource(_cm_,_custom_tree_,_seq_),true);
	dtth=NULL;
	
}
SednaSearchJob::SednaSearchJob():seq(NULL)
{
	dtth=NULL;	
}
void SednaSearchJob::set_request(tuple_cell& request)
{
	this->Request.setU8(t_str_buf(request).c_str());
}
void SednaSearchJob::get_next_result(tuple &t)
{
	if (dtth==NULL)
	{
		UUnnamedSemaphoreCreate(&sem1, 0);
		UUnnamedSemaphoreCreate(&sem2, 0);
        uCreateThread(
        ThreadFunc,                  // thread function 
        this,						 // argument to thread function 
        &dtth,                       // use default creation flags 
        0);
	}

	UUnnamedSemaphoreDown(&sem1);
    if (res==XNULL)
	{
		t.set_eos();
		dtth = NULL;
		UUnnamedSemaphoreRelease(&sem1);
		UUnnamedSemaphoreRelease(&sem2);
	}
	else
	{
		t.copy(tuple_cell::node(res));
		UUnnamedSemaphoreUp(&sem2);
	}
}
void SednaSearchJob::set_index(tuple_cell& name)
{
	ft_index_cell* ft_idx=ft_index_cell::find_index(t_str_buf(name).c_str());
	if (ft_idx==NULL)
		throw USER_EXCEPTION(SE1061);
	std::string index_path1 = std::string(SEDNA_DATA) + std::string("\\data\\")
		+ std::string(db_name) + std::string("_files\\dtsearch\\");
	std::string index_path = index_path1 + std::string(ft_idx->index_title);
	this->AddIndexToSearch(index_path.c_str());
	
}

void SednaSearchJob::reopen()
{
	seq->op->reopen();
	dtth=NULL;
}
SednaSearchJob::~SednaSearchJob()
{
}
DWORD WINAPI SednaSearchJob::ThreadFunc( void* lpParam )
{
	((SednaSearchJob*)lpParam)->Execute();
	if (((SednaSearchJob*)lpParam)->GetErrorCount()>0)
	{
		const dtsErrorInfo * ptr= ((SednaSearchJob*)lpParam)->GetErrors();
		/*for (int i=0;i<ptr->getCount();i++)
			std::cout<<ptr->getMessage(i);*/
	}

	((SednaSearchJob*)lpParam)->res = XNULL;
	UUnnamedSemaphoreUp(&(((SednaSearchJob*)lpParam)->sem1));
	return 0;
}
void SednaSearchJob::OnSearchingIndex(const char * indexPath)
{
	//std::cout<<"Searching: "<<indexPath;
}


