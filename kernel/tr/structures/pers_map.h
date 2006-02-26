/*
 * File:  pers_map.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef PERSMAP_H
#define PERSMAP_H

#include "pers_heap.h"
#include "base.h"

inline void *scm_malloc(size_t size,bool persistent)
{
    
	return (persistent)?pers_malloc(size):malloc(size);
}

inline void scm_free(void *membloc,bool persistent)
{
	(persistent)? pers_free(membloc):free(membloc);
}


template<class T, class X> struct pers_sset
{
	struct pers_sset_entry
	{
		T* obj;	
		X s_left;
		X s_right;
		X s_parent;
		bool black;
		
		
		inline pers_sset_entry* left(pers_sset_entry* mem_pool) {return (this->s_left)? mem_pool+(this->s_left-1) : 0;}
		inline pers_sset_entry* right(pers_sset_entry* mem_pool) {return (this->s_right)?mem_pool+(this->s_right-1):0;}
		inline pers_sset_entry* parent(pers_sset_entry* mem_pool) {return (this->s_parent)?mem_pool+(this->s_parent-1):0;}
		inline void set_left(pers_sset_entry* mem_pool,pers_sset_entry* entry) 
		{
			if (entry) this->s_left=entry->shift(mem_pool);
			else
			this->s_left=0;
		}
		inline void set_right(pers_sset_entry* mem_pool,pers_sset_entry* entry) 
		{
			if (entry) this->s_right=entry->shift(mem_pool);
			else
			this->s_right=0;
		}
		inline void set_parent(pers_sset_entry* mem_pool,pers_sset_entry* entry) 
		{
			if (entry) this->s_parent=entry->shift(mem_pool);
			else
			this->s_parent=0;
		}
		inline X shift(pers_sset_entry* mem_pool) {return (X)( ((int)this-(int)mem_pool)/sizeof(pers_sset_entry))+1;}
		static void init (pers_sset_entry* res,T* obj)
		{
			res->obj=obj;
			res->s_parent=0;
			res->s_left=0;
			res->s_right=0;
		}
		
	} *root;

	pers_sset_entry* mem_pool;
	int mem_pool_cnt;
	int mem_pool_size;
	int mem_free_place;
	//static pers_sset_entry* mem_pool;
	/*initialization of persisten map*/
	//static pers_sset* init();
	static pers_sset* init(bool persistent=true);
	static void free(pers_sset<T,X>* set);
	bool add(T* o);
	T* find (void const* p1,void const* p2);
	pers_sset_entry* get (void const* p1,void const* p2);
	void put (T* obj);
	void tree_insert(pers_sset_entry* entry);
	void left_rotate(pers_sset_entry* entry);
	void right_rotate(pers_sset_entry* entry);
	void rb_insert(pers_sset_entry* entry);
	void rb_delete(pers_sset_entry* entry);
	void rb_fixup(pers_sset_entry* entry,pers_sset_entry* parent);
	pers_sset_entry* rb_minimum(pers_sset_entry* entry);
	pers_sset_entry* rb_successor(pers_sset_entry* entry);
	inline bool less(typename pers_sset<T,X>::pers_sset_entry* p1,typename pers_sset<T,X>::pers_sset_entry* p2)
		{
			return (p1->obj->less(p2->obj));
		}
};


template<class T,class X> bool pers_sset<T,X>::add(T* o)
{
	return true;
}

template<class T, class X> T* pers_sset<T,X>::find (void const* p1, void const* p2)
{
	
	pers_sset_entry* x=root;
	while (x!=NULL )
	{
		if (x->obj->less(p1,p2)) x=x->right(mem_pool);
		else
			if (x->obj->equals(p1,p2))
			{
				return x->obj;
				break;
			}
			else
				x=x->left(mem_pool);
	}
	return NULL;
}
template<class T, class X> void pers_sset<T,X>::put (T*  obj)
{
	if (mem_pool_cnt==mem_pool_size)
	{
		pers_sset_entry* tmp=(pers_sset_entry*)scm_malloc(sizeof(pers_sset_entry)*2*mem_pool_size,IS_PH_PTR(mem_pool));
		memset(tmp+mem_pool_cnt,0,mem_pool_cnt);
		int cnt=root-mem_pool;
		memcpy(tmp,mem_pool,mem_pool_cnt*sizeof(pers_sset_entry));
		scm_free(mem_pool,IS_PH_PTR(mem_pool));
		mem_pool_size*=2;
		mem_pool=tmp;
		root=mem_pool+cnt;
	}
	pers_sset_entry* res;
	if (!mem_free_place)
	{
		res=mem_pool+mem_pool_cnt;
		
	}
	else
	{
		res=mem_pool+mem_free_place-1;
		int n_place=*((int*)res);
		mem_free_place=n_place;
	}
	pers_sset_entry::init(res,obj);
	mem_pool_cnt++;
	rb_insert(res);
}


template<class T, class X> void pers_sset<T,X>::left_rotate(pers_sset_entry* entry)
{
	pers_sset_entry* y=entry->right(mem_pool);
	entry->set_right(mem_pool,y->left(mem_pool));
	if (entry->right(mem_pool)!=NULL)
		entry->right(mem_pool)->set_parent(mem_pool,entry);
	y->set_parent(mem_pool,entry->parent(mem_pool));
	if (y->parent(mem_pool)==NULL)
		root=y;
	else
	{
		if (entry==y->parent(mem_pool)->left(mem_pool)) y->parent(mem_pool)->set_left(mem_pool,y);
		else y->parent(mem_pool)->set_right(mem_pool,y);
	}
	y->set_left(mem_pool,entry);
	entry->set_parent(mem_pool,y);
}

template<class T, class X> void pers_sset<T,X>::right_rotate(pers_sset_entry* entry)
{
	pers_sset_entry* y=entry->left(mem_pool);
	entry->set_left(mem_pool,y->right(mem_pool));
	if (entry->left(mem_pool)!=NULL)
		entry->left(mem_pool)->set_parent(mem_pool,entry);
	y->set_parent(mem_pool,entry->parent(mem_pool));
	if (y->parent(mem_pool)==NULL)
		root=y;
	else
	{
		if (entry==y->parent(mem_pool)->left(mem_pool)) y->parent(mem_pool)->set_left(mem_pool,y);
		else y->parent(mem_pool)->set_right(mem_pool,y);
	}
	y->set_right(mem_pool,entry);
	entry->set_parent(mem_pool,y);
}
template<class T, class X> void pers_sset<T,X>::tree_insert(pers_sset_entry* entry)
{
	pers_sset_entry* y=NULL;
	pers_sset_entry* x=root;
	while (x!=NULL)
	{
		y=x;
		if (less(entry,x)) x=x->left(mem_pool);
		else x=x->right(mem_pool);
	}
	if (y==NULL)
		root=entry;
	else
	{
		entry->set_parent(mem_pool,y);
		if (less(entry,y))	y->set_left(mem_pool,entry);
		else y->set_right(mem_pool,entry);
	}
}
template<class T, class X> void pers_sset<T,X>::rb_insert(pers_sset_entry* entry)
{
	tree_insert(entry);
	pers_sset_entry* x=entry;
	x->black=false;
	while (x!=root && !x->parent(mem_pool)->black)
	{
		if (x->parent(mem_pool)==x->parent(mem_pool)->parent(mem_pool)->left(mem_pool))
		{
			pers_sset_entry* y=x->parent(mem_pool)->parent(mem_pool)->right(mem_pool);
			if (y && !y->black)
			{
				x->parent(mem_pool)->black=true;
				y->black=true;
				x->parent(mem_pool)->parent(mem_pool)->black=false;
				x=x->parent(mem_pool)->parent(mem_pool);
			}
			else
			{
				if (x==x->parent(mem_pool)->right(mem_pool))
				{
					x=x->parent(mem_pool);
					left_rotate(x);
				}
				x->parent(mem_pool)->black=true;
				x->parent(mem_pool)->parent(mem_pool)->black=false;
				right_rotate(x->parent(mem_pool)->parent(mem_pool));
			}
		}
		else
		{
			pers_sset_entry* y=x->parent(mem_pool)->parent(mem_pool)->left(mem_pool);
			if (y && !y->black)
			{
				x->parent(mem_pool)->black=true;
				y->black=true;
				x->parent(mem_pool)->parent(mem_pool)->black=false;
				x=x->parent(mem_pool)->parent(mem_pool);
			}
			else
			{
				if (x==x->parent(mem_pool)->left(mem_pool))
				{
					x=x->parent(mem_pool);
					right_rotate(x);
				}
				x->parent(mem_pool)->black=true;
				x->parent(mem_pool)->parent(mem_pool)->black=false;
				left_rotate(x->parent(mem_pool)->parent(mem_pool));

			}
		}
	}
	root->black=true;
}

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
}
template<class T, class X> typename pers_sset<T,X>::pers_sset_entry* pers_sset<T,X>::rb_minimum(pers_sset_entry* entry)
{
	pers_sset_entry* x=entry;
	pers_sset_entry* y=(x!=NULL)?entry->left(mem_pool):NULL;
	while (y!=NULL)
	{
		x=y;
		y=x->left(mem_pool);
	}
	return x;
}
template<class T, class X> typename pers_sset<T,X>::pers_sset_entry* pers_sset<T,X>::rb_successor(pers_sset_entry* entry)
{
	pers_sset_entry* x=entry->right(mem_pool);
	if (x!=NULL)
		return rb_minimum(x);
	x=entry;
	pers_sset_entry* y=x->parent(mem_pool);
	while (y!=NULL && x==y->right(mem_pool))
	{
		x=y;
		y=x->parent(mem_pool);
	}
	return y;

}
template<class T, class X> pers_sset<T,X>* pers_sset<T,X>::init(bool persistent)
{
	pers_sset* ps=(pers_sset*)scm_malloc(sizeof(pers_sset),persistent);
	ps->mem_pool=(pers_sset_entry*)scm_malloc(sizeof(pers_sset_entry)*20,persistent);
	ps->mem_pool_size=20;
	ps->mem_pool_cnt=0;
	ps->mem_free_place=0;
	ps->root=NULL;
	return ps;
}

template<class T, class X> void pers_sset<T,X>::free(pers_sset<T,X>* set)
{
	scm_free(set->mem_pool,IS_PH_PTR(set));
	scm_free(set,IS_PH_PTR(set));	
}

template<class T, class X> typename pers_sset<T,X>::pers_sset_entry* pers_sset<T,X>::get (void const* p1, void const* p2)
{
	
	pers_sset_entry* x=root;
	while (x!=NULL )
	{
		if (x->obj->less(p1,p2)) x=x->right(mem_pool);
		else
			if (x->obj->equals(p1,p2))
			{
				return x;
				break;
			}
			else
				x=x->left(mem_pool);
	}
	return NULL;
}
#endif

