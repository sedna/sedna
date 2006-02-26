#ifndef _FT_INDEX_H
#define _FT_INDEX_H
#include "FTsearch.h"
#include "xptr_sequence.h"

class SednaIndexJob : public dtSearch::DIndexJob {
     public:
           
           //SednaIndexJob(PPOpIn* _seq_);
		   SednaIndexJob(ft_index_cell* _ft_idx_);
		   void set_index_name(tuple_cell& request);
		   void create_index(std::vector<xptr> *first_nodes);
		   void clear_index();
		   void update_index(xptr_sequence* upserted);
		   void delete_from_index(xptr_sequence* deleted);
		   virtual void OnError(long, const char *, const char *, const char *);

	  private:
		  PPOpIn* seq;
		  const ft_index_cell *ft_idx;
		  
     };
#endif