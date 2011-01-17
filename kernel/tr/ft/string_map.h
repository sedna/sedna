/*
 * File:  string_map.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

//based on pers_map.h
//TODO: try using something other that rb-trees

#ifndef STRING_MAP_H
#define STRING_MAP_H

class MallocAllocator
{
private:
	static const int maxp = 10000;
	static const unsigned int maxsz = 100*1024*1024;
	static const int onesz = 50*1024*1024; //TODO
	int pind;
	void *ptrs[maxp];
	size_t cursize;
	size_t left;
public:
	typedef char* ptr_t;

	MallocAllocator() : pind(-1), cursize(0), left(0) {}

	ptr_t alloc(size_t size) { 
		if (left >= size)
		{
			left -= size;
			return &((char*)ptrs[pind])[left];
		}
		left = 1024*1024;
		if (left < size)
			left = size;
		if (cursize + left > maxsz || pind >= maxp)
			return null_ptr();
		cursize += left;
		pind++;
		ptrs[pind] = malloc(left);
		if (ptrs[pind] == NULL)
		{
			left = 0;
			return NULL;
		}
		left -= size;
		return &((char*)ptrs[pind])[left];
	}
	inline static ptr_t null_ptr() { return NULL; }
	inline char* deref(ptr_t ptr) const { return (char*)ptr; }
	void release()
	{
		for (int i = 0; i <= pind; i++)
			free(ptrs[i]);
		pind = -1;
		cursize = 0;
		left = 0;
	}
};

//TODO: spec interface
//TODO: make this work for non null-terminated strings?
//stuff needed from AllocatorT: ptr_t, deref() const, (ptr_t+int), static null_ptr(), null_ptr() must deref to NULL, copy constructor must produce same allocator
//all returned pointers are obtained using deref()
template<typename T, typename AllocatorT = MallocAllocator> class string_map
{
	AllocatorT *allocator;
	typedef unsigned short str_len_t;
public:
	struct pers_sset_entry
	{
		T obj;
		typename AllocatorT::ptr_t s_left;
		typename AllocatorT::ptr_t s_right;
		typename AllocatorT::ptr_t s_parent;
		str_len_t str_len;
		bool black;
#ifdef _MSC_VER
#pragma warning( disable : 4200 )
#endif
        char str[];
#ifdef _MSC_VER
#pragma warning( default : 4200 )
#endif
		
		inline const char *get_str() const { return (this->str);}
		inline str_len_t get_str_len() const { return (this->str_len);}
		
		inline typename AllocatorT::ptr_t left_ptr() {return (this->s_left);}
		inline typename AllocatorT::ptr_t right_ptr() {return (this->s_right);}
		inline typename AllocatorT::ptr_t parent_ptr() {return (this->s_parent);}
		inline pers_sset_entry* left(const AllocatorT &allocator) {return (pers_sset_entry*)allocator.deref(this->s_left);}
		inline pers_sset_entry* right(const AllocatorT &allocator) {return (pers_sset_entry*)allocator.deref(this->s_right);}
		inline pers_sset_entry* parent(const AllocatorT &allocator) {return (pers_sset_entry*)allocator.deref(this->s_parent);}
		inline void set_left(typename AllocatorT::ptr_t entry) 
		{
			this->s_left=entry;
		}
		inline void set_right(typename AllocatorT::ptr_t entry) 
		{
			this->s_right=entry;
		}
		inline void set_parent(typename AllocatorT::ptr_t entry) 
		{
			this->s_parent=entry;
		}
		static void init (pers_sset_entry* res, const char *str, str_len_t str_len, const T obj)
		{
			res->str_len = str_len;
			memcpy(res->str, str, str_len+1);
			res->obj=obj;
			res->s_parent=AllocatorT::null_ptr();
			res->s_left=AllocatorT::null_ptr();
			res->s_right=AllocatorT::null_ptr();
		}
	};
	typename AllocatorT::ptr_t root;
	static typename AllocatorT::ptr_t init(AllocatorT *allocator);
	static string_map *get_map(typename AllocatorT::ptr_t ptr, const AllocatorT &alloc)
	{
		return (string_map*)alloc.deref(ptr);
	}
	T *find (const char *str);

	pers_sset_entry* get (const char *str);

	//str is copied into string_map memory
	pers_sset_entry* put (const char *str, T obj);
	void tree_insert(typename AllocatorT::ptr_t entry_ptr);
	void left_rotate(typename AllocatorT::ptr_t entry_ptr);
	void right_rotate(typename AllocatorT::ptr_t entry_ptr);
	void rb_insert(typename AllocatorT::ptr_t entry_ptr);
	//void rb_delete(pers_sset_entry* entry);
	//void rb_fixup(pers_sset_entry* entry,pers_sset_entry* parent);
	pers_sset_entry* rb_minimum(pers_sset_entry* entry);
	pers_sset_entry* rb_maximum(pers_sset_entry* entry);
	pers_sset_entry* rb_successor(pers_sset_entry* entry);
	pers_sset_entry* rb_predecessor(pers_sset_entry* entry);

	static inline int mystrcmp(const char *a, str_len_t a_len, const char *b, str_len_t b_len)
	{
		int mlen = a_len < b_len ? a_len : b_len, res = memcmp(a, b, mlen);
		if (res == 0)
			return (int)a_len - (int)b_len;
		return res;
	}

	static inline bool less(pers_sset_entry* a, const char *str, str_len_t str_len, const AllocatorT &allocator)
	{
		return mystrcmp(a->get_str(), a->get_str_len(), str, str_len) < 0;
	}
	static inline bool equals(pers_sset_entry* a, const char *str, str_len_t str_len, const AllocatorT &allocator)
	{
		return mystrcmp(a->get_str(), a->get_str_len(), str, str_len) == 0;
	}
	static inline bool less(pers_sset_entry* a, pers_sset_entry* b, const AllocatorT &allocator)
	{
		return mystrcmp(a->get_str(), a->get_str_len(), b->get_str(), b->get_str_len()) < 0;
	}
	static inline bool less(typename AllocatorT::ptr_t a_ptr, typename AllocatorT::ptr_t b_ptr, const AllocatorT &allocator)
	{
		return less( (pers_sset_entry*)allocator.deref(a_ptr), (pers_sset_entry*)allocator.deref(b_ptr), allocator);
	}
	static inline pers_sset_entry* get_entry(typename AllocatorT::ptr_t entry_ptr, const AllocatorT &allocator)
	{
		return ((pers_sset_entry*)allocator.deref(entry_ptr));
	}
	static inline typename AllocatorT::ptr_t get_left_ptr(typename AllocatorT::ptr_t entry_ptr, const AllocatorT &allocator)
	{
		return get_entry(entry_ptr, allocator)->left_ptr();
	}
	static inline typename AllocatorT::ptr_t get_right_ptr(typename AllocatorT::ptr_t entry_ptr, const AllocatorT &allocator)
	{
		return get_entry(entry_ptr, allocator)->right_ptr();
	}
};

template<typename T, typename AllocatorT> T *string_map<T,AllocatorT>::find (const char *str)
{
	pers_sset_entry* x=(pers_sset_entry*)allocator->deref(root);
	while (x!=NULL )
	{
		if (strcmp(allocator->deref(x->str), str)<0) x=x->right(*allocator);
		else
			if (!strcmp(allocator->deref(x->str), str))
			{
				return &x->obj;
				break;
			}
			else
				x=x->left(*allocator);
	}
	return NULL;
}
template<class T, class AllocatorT> typename string_map<T,AllocatorT>::pers_sset_entry* string_map<T,AllocatorT>::put (const char *str, T obj)
{
	typename AllocatorT::ptr_t res_ptr;
	typename AllocatorT::ptr_t str_ptr;
	pers_sset_entry *res;
	int str_len = strlen(str);


	res_ptr = allocator->alloc(sizeof(pers_sset_entry) + str_len + 1);
	if (res_ptr == AllocatorT::null_ptr())
		return NULL;
	str_ptr = res_ptr + sizeof(pers_sset_entry);
	res = (pers_sset_entry *)allocator->deref(res_ptr);

	pers_sset_entry::init(res,str,str_len,obj);
	rb_insert(res_ptr);

	return res;
}
template<class T, class AllocatorT> void string_map<T,AllocatorT>::left_rotate(typename AllocatorT::ptr_t entry_ptr)
{
	pers_sset_entry* entry = (pers_sset_entry*)allocator->deref(entry_ptr);
	typename AllocatorT::ptr_t y_ptr = entry->right_ptr();
	pers_sset_entry* y=(pers_sset_entry*)allocator->deref(y_ptr);
	entry->set_right(y->left_ptr());
	if (entry->right(*allocator)!=NULL)
		entry->right(*allocator)->set_parent(entry_ptr);
	y->set_parent(entry->parent_ptr());
	if (y->parent(*allocator)==NULL)
		root=y_ptr;
	else
	{
		if (entry==y->parent(*allocator)->left(*allocator)) y->parent(*allocator)->set_left(y_ptr);
		else y->parent(*allocator)->set_right(y_ptr);
	}
	y->set_left(entry_ptr);
	entry->set_parent(y_ptr);
}
template<class T, class AllocatorT> void string_map<T,AllocatorT>::right_rotate(typename AllocatorT::ptr_t entry_ptr)
{
	pers_sset_entry* entry = (pers_sset_entry*)allocator->deref(entry_ptr);
	typename AllocatorT::ptr_t y_ptr = entry->left_ptr();
	pers_sset_entry* y=(pers_sset_entry*)allocator->deref(y_ptr);
	entry->set_left(y->right_ptr());
	if (entry->left(*allocator)!=NULL)
		entry->left(*allocator)->set_parent(entry_ptr);
	y->set_parent(entry->parent_ptr());
	if (y->parent(*allocator)==NULL)
		root=y_ptr;
	else
	{
		if (entry==y->parent(*allocator)->left(*allocator)) y->parent(*allocator)->set_left(y_ptr);
		else y->parent(*allocator)->set_right(y_ptr);
	}
	y->set_right(entry_ptr);
	entry->set_parent(y_ptr);
}
template<class T, class AllocatorT> void string_map<T,AllocatorT>::tree_insert(typename AllocatorT::ptr_t entry_ptr)
{
	pers_sset_entry* entry = (pers_sset_entry*)allocator->deref(entry_ptr);
	typename AllocatorT::ptr_t y_ptr=AllocatorT::null_ptr();
	typename AllocatorT::ptr_t x_ptr=root;
	while (x_ptr!=AllocatorT::null_ptr())
	{
		y_ptr=x_ptr;
		if (less(entry_ptr,x_ptr,*allocator)) x_ptr=get_left_ptr(x_ptr, *allocator);
		else x_ptr=get_right_ptr(x_ptr, *allocator);
	}
	if (y_ptr==AllocatorT::null_ptr())
		root=entry_ptr;
	else
	{
		entry->set_parent(y_ptr);
		if (less(entry_ptr,y_ptr, *allocator)) get_entry(y_ptr, *allocator)->set_left(entry_ptr);
		else get_entry(y_ptr, *allocator)->set_right(entry_ptr);
	}
}
template<class T, class AllocatorT> void string_map<T,AllocatorT>::rb_insert(typename AllocatorT::ptr_t entry_ptr)
{
	tree_insert(entry_ptr);
	typename AllocatorT::ptr_t x_ptr = entry_ptr;
	pers_sset_entry* x=get_entry(x_ptr, *allocator);
	x->black=false;
	while (x_ptr!=root && !x->parent(*allocator)->black)
	{
		if (x->parent(*allocator)==x->parent(*allocator)->parent(*allocator)->left(*allocator))
		{
			pers_sset_entry* y=x->parent(*allocator)->parent(*allocator)->right(*allocator);
			if (y && !y->black)
			{
				x->parent(*allocator)->black=true;
				y->black=true;
				x->parent(*allocator)->parent(*allocator)->black=false;
				x_ptr=x->parent(*allocator)->parent_ptr();
				x = get_entry(x_ptr, *allocator);
			}
			else
			{
				if (x_ptr==x->parent(*allocator)->right_ptr())
				{
					x_ptr=x->parent_ptr();
					x = get_entry(x_ptr, *allocator);
					left_rotate(x_ptr);
				}
				x->parent(*allocator)->black=true;
				x->parent(*allocator)->parent(*allocator)->black=false;
				right_rotate(x->parent(*allocator)->parent_ptr());
			}
		}
		else
		{
			pers_sset_entry* y=x->parent(*allocator)->parent(*allocator)->left(*allocator);
			if (y && !y->black)
			{
				x->parent(*allocator)->black=true;
				y->black=true;
				x->parent(*allocator)->parent(*allocator)->black=false;
				x_ptr=x->parent(*allocator)->parent_ptr();
				x = get_entry(x_ptr, *allocator);
			}
			else
			{
				if (x==x->parent(*allocator)->left(*allocator))
				{
					x_ptr=x->parent_ptr();
					x = get_entry(x_ptr, *allocator);
					right_rotate(x_ptr);
				}
				x->parent(*allocator)->black=true;
				x->parent(*allocator)->parent(*allocator)->black=false;
				left_rotate(x->parent(*allocator)->parent_ptr());
			}
		}
	}
	get_entry(root, *allocator)->black=true;
}
template<class T, class AllocatorT> typename string_map<T,AllocatorT>::pers_sset_entry* string_map<T,AllocatorT>::rb_minimum(pers_sset_entry* entry)
{
	pers_sset_entry* x=entry;
	pers_sset_entry* y=(x!=NULL)?x->left(*allocator):NULL;
	while (y!=NULL)
	{
		x=y;
		y=x->left(*allocator);
	}
	return x;
}
template<class T, class AllocatorT> typename string_map<T,AllocatorT>::pers_sset_entry* string_map<T,AllocatorT>::rb_maximum(pers_sset_entry* entry)
{
	pers_sset_entry* x=entry;
	pers_sset_entry* y=(x!=NULL)?x->right(*allocator):NULL;
	while (y!=NULL)
	{
		x=y;
		y=x->right(*allocator);
	}
	return x;
}
template<class T, class AllocatorT> typename string_map<T,AllocatorT>::pers_sset_entry* string_map<T,AllocatorT>::rb_successor(pers_sset_entry* entry)
{
	pers_sset_entry* x=entry->right(*allocator);
	if (x!=NULL)
		return rb_minimum(x);
	x=entry;
	pers_sset_entry* y=x->parent(*allocator);
	while (y!=NULL && x==y->right(*allocator))
	{
		x=y;
		y=x->parent(*allocator);
	}
	return y;
}
template<class T, class AllocatorT> typename string_map<T,AllocatorT>::pers_sset_entry* string_map<T,AllocatorT>::rb_predecessor(pers_sset_entry* entry)
{
	pers_sset_entry* x=entry->left(*allocator);
	if (x!=NULL)
		return rb_maximum(x);
	x=entry;
	pers_sset_entry* y=x->parent(*allocator);
	while (y!=NULL && x==y->left(*allocator))
	{
		x=y;
		y=x->parent(*allocator);
	}
	return y;
}
template<class T, class AllocatorT> typename AllocatorT::ptr_t string_map<T,AllocatorT>::init(AllocatorT *allocator)
{
	typename AllocatorT::ptr_t ptr = allocator->alloc(sizeof(string_map));
	if (ptr == allocator->null_ptr())
		return ptr;
	string_map *strmap = (string_map *)allocator->deref(ptr);
	strmap->root=NULL;
	strmap->allocator = allocator;
	return ptr;
}
template<class T, class AllocatorT> typename string_map<T,AllocatorT>::pers_sset_entry* string_map<T,AllocatorT>::get(const char *str)
{
	pers_sset_entry* x=get_entry(root, allocator);
	while (x!=NULL)
	{
		if (less(x,str)) x=x->right(allocator);
		else
			if (equals(x,str))
			{
				return x;
				break;
			}
			else
				x=x->left(allocator);
	}
	return NULL;
}





/*template<class T, class X> void pers_sset<T,X>::sset_free(pers_sset<T,X>* set)
{
	scm_free(set->mem_pool,IS_PH_PTR(set));
	scm_free(set,IS_PH_PTR(set));	
}

template<class T, class X> void pers_sset<T,X>::clear()
{
	mem_pool_cnt=0;
	mem_free_place=0;
	root=NULL;
}*/

/*
template<class T, class X> void pers_sset<T,X>::rb_delete(pers_sset_entry* entry)
{
	pers_sset_entry* y=entry;
	if (entry->left(mem_pool)!=NULL && entry->right(mem_pool)!=NULL)
		y=rb_successor(y);
	pers_sset_entry* x=y->left(mem_pool);
	if (y->left(mem_pool)==NULL)
		x=y->right(mem_pool);
	if (x!=NULL)
		x->set_parent(mem_pool,y->parent(mem_pool));
	if (y->parent(mem_pool)==NULL)
		root=x;
	else
	{
		if (y==y->parent(mem_pool)->left(mem_pool))
			y->parent(mem_pool)->set_left(mem_pool,x);
		else
			y->parent(mem_pool)->set_right(mem_pool,x);
	}
	if (y!=entry)
		entry->obj=y->obj;
	if (y->black)
	{
		rb_fixup(x,y->parent(mem_pool));
	}
	// y deletion
	*((int*)y)=mem_free_place;
	mem_free_place=((int)y-(int)mem_pool)/sizeof(pers_sset_entry)+1;
	mem_pool_cnt--;

}
template<class T, class X> void pers_sset<T,X>::rb_fixup(pers_sset_entry* entry,pers_sset_entry* parent)
{
	pers_sset_entry* x=entry;
	pers_sset_entry* px=parent;
	while (x!=root && (x==NULL || x->black))
	{
		if (x==px->left(mem_pool))
		{
			pers_sset_entry* w=px->right(mem_pool);
			if (!w->black)
			{
				w->black=true;
				px->black=false;
				left_rotate(px);
				w=px->right(mem_pool);
			}
			if (
				(w->left(mem_pool)==NULL||w->left(mem_pool)->black)
				&&
				(w->right(mem_pool)==NULL||w->right(mem_pool)->black))
			{
				w->black=false;
				x=px;
				px=px->parent(mem_pool);
			}
			else
			{
				if (w->right(mem_pool)==NULL||w->right(mem_pool)->black)
				{
					w->left(mem_pool)->black=true;
					w->black=false;
					right_rotate(w);
					w=px->right(mem_pool);
				}
				w->black=px->black;
				px->black=true;
				if (w->right(mem_pool)!=NULL)
					w->right(mem_pool)->black=true;
				left_rotate(px);
				x=root;
			}
		}
		else		
		{
			pers_sset_entry* w=px->left(mem_pool);
			if (!w->black)
			{
				w->black=true;
				px->black=false;
				right_rotate(px);
				w=px->left(mem_pool);
			}
			if (
				(w->left(mem_pool)==NULL||w->left(mem_pool)->black)
				&&
				(w->right(mem_pool)==NULL||w->right(mem_pool)->black))
			{
				w->black=false;
				x=px;
				px=px->parent(mem_pool);
			}
			else
			{
				if (w->left(mem_pool)==NULL||w->left(mem_pool)->black)
				{
					w->right(mem_pool)->black=true;
					w->black=false;
					left_rotate(w);
					w=px->left(mem_pool);
				}
				w->black=px->black;
				px->black=true;
				if (w->left(mem_pool)!=NULL)
					w->left(mem_pool)->black=true;
				right_rotate(px);
				x=root;
			}
		}
	}
	if (x!=NULL) x->black=true;
}*/

#endif
