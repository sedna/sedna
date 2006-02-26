#include "FTindex.h"
#include "tr_globals.h"
#include "dtsearch.h"
#include "uhdd.h"



SednaIndexJob::SednaIndexJob(ft_index_cell* _ft_idx_) : ft_idx(_ft_idx_)
{
	std::string index_path1 = std::string(SEDNA_DATA) + std::string("\\data\\")
		+ std::string(db_name) + std::string("_files\\dtsearch\\");
	std::string index_path = index_path1 + std::string(ft_idx->index_title);
	uMkDir(index_path1.c_str());
	uMkDir(index_path.c_str());
	this->IndexPath.set(index_path.c_str());

	this->SuppressMessagePump();
}
void SednaIndexJob::set_index_name(tuple_cell& request)
{
	this->IndexName.set(t_str_buf(request).c_str());
}
void SednaIndexJob::create_index(std::vector<xptr> *first_nodes)
{
	this->SetActionAdd();
	this->SetActionCreate();
	AttachDataSource(new CreationSednaDataSource(ft_idx->ftype, ft_idx->custom_tree, first_nodes), true);

	this->Execute();
}
void SednaIndexJob::OnError(long a, const char * b, const char * c, const char *d)
{
	std::cout<<"error"<<c;
}
void SednaIndexJob::clear_index()
{
	std::string index_path1 = std::string(SEDNA_DATA) + std::string("\\data\\")
		+ std::string(db_name) + std::string("_files\\dtsearch\\");
	std::string index_path = index_path1 + std::string(ft_idx->index_title);

	short result;
	dtssDeleteIndex((char *)index_path.c_str(), result);
}
void SednaIndexJob::update_index(xptr_sequence* upserted)
{
	this->SetActionAdd();
	AttachDataSource(new UpdateSednaDataSource(ft_idx->ftype, ft_idx->custom_tree, upserted), true);

	this->Execute();
}
void SednaIndexJob::delete_from_index(xptr_sequence* deleted)
{
	std::string list_path1 = std::string(SEDNA_DATA) + std::string("\\data\\")
		+ std::string(db_name) + std::string("_files\\dtsearch\\");
	std::string list_path = list_path1 + std::string(ft_idx->index_title) + 
		std::string("\\remove_list");

	UFile f = uCreateFile(list_path.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_WRITE_THROUGH, NULL);

	xptr_sequence::iterator it = deleted->begin();
	char tmp_buf[32];
	while (it != deleted->end())
	{
		int wr;
		SednaDataSource::recordToFilename(tmp_buf, *it);
		uWriteFile(f, tmp_buf, strlen(tmp_buf), &wr);
		tmp_buf[0] = '\n';
		uWriteFile(f, tmp_buf, 1, &wr);

		it++;
	}
	uCloseFile(f);
	this->ToRemoveListName.setU8(list_path.c_str());
	this->SetActionRemoveListed(true);

	this->Execute();
	uDeleteFile(list_path.c_str());
}
