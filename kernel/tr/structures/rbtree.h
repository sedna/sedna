/*
 * File:  rbtree.h
 * Copyright (C) 2004-2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef PERSMAP_H
#define PERSMAP_H

#include "common/sedna.h"

template<class T> 
struct sedna_rbtree
{
	struct sedna_rbtree_entry
	{
		T* obj;	
		unsigned int s_left;
		unsigned int s_right;
		unsigned int s_parent;
		bool black;
		
		inline sedna_rbtree_entry* left(sedna_rbtree_entry* mem_pool) {return (this->s_left)? mem_pool+(this->s_left-1) : 0;}
		inline sedna_rbtree_entry* right(sedna_rbtree_entry* mem_pool) {return (this->s_right)?mem_pool+(this->s_right-1):0;}
		inline sedna_rbtree_entry* parent(sedna_rbtree_entry* mem_pool) {return (this->s_parent)?mem_pool+(this->s_parent-1):0;}
		inline void set_left(sedna_rbtree_entry* mem_pool,sedna_rbtree_entry* entry) 
		{
			if (entry) this->s_left=entry->shift(mem_pool);
			else
			this->s_left=0;
		}
		inline void set_right(sedna_rbtree_entry* mem_pool,sedna_rbtree_entry* entry) 
		{
			if (entry) this->s_right=entry->shift(mem_pool);
			else
			this->s_right=0;
		}
		inline void set_parent(sedna_rbtree_entry* mem_pool,sedna_rbtree_entry* entry) 
		{
			if (entry) this->s_parent=entry->shift(mem_pool);
			else
			this->s_parent=0;
		}
		inline unsigned int shift(sedna_rbtree_entry* mem_pool) {return (unsigned int)(this - mem_pool) + 1;}
		static void init (sedna_rbtree_entry* res,T* obj)
		{
			res->obj=obj;
			res->s_parent=0;
			res->s_left=0;
			res->s_right=0;
		}
		
	} *root;

	sedna_rbtree_entry* mem_pool;
    int mem_pool_size;
	int mem_pool_cnt;
	int mem_free_place;

	static sedna_rbtree* init();

    inline sedna_rbtree() : root(NULL), mem_pool_size(20), mem_pool_cnt(0), mem_free_place(0) {
        this->mem_pool=(sedna_rbtree_entry*)malloc(sizeof(sedna_rbtree_entry)*mem_pool_size);
    };

	static void sset_free(sedna_rbtree<T>* set);
	bool add(T* o);
	T* find (void const* p1,void const* p2);
	sedna_rbtree_entry* get (void const* p1,void const* p2);
	void put (T* obj);
	void tree_insert(sedna_rbtree_entry* entry);
	void left_rotate(sedna_rbtree_entry* entry);
	void right_rotate(sedna_rbtree_entry* entry);
	void rb_insert(sedna_rbtree_entry* entry);
	void rb_delete(sedna_rbtree_entry* entry);
	void rb_fixup(sedna_rbtree_entry* entry,sedna_rbtree_entry* parent);
	sedna_rbtree_entry* rb_minimum(sedna_rbtree_entry* entry);
	sedna_rbtree_entry* rb_maximum(sedna_rbtree_entry* entry);
	sedna_rbtree_entry* rb_successor(sedna_rbtree_entry* entry);
	sedna_rbtree_entry* rb_predecessor(sedna_rbtree_entry* entry);
	void clear();
	inline bool less(typename sedna_rbtree<T>::sedna_rbtree_entry* p1,typename sedna_rbtree<T>::sedna_rbtree_entry* p2)
		{
			return (p1->obj->less(p2->obj));
		}
};


template<class T> bool sedna_rbtree<T>::add(T* o)
{
	return true;
}

template<class T> T* sedna_rbtree<T>::find (void const* p1, void const* p2)
{
	
	sedna_rbtree_entry* x=root;
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
template<class T> void sedna_rbtree<T>::put (T*  obj)
{
	if (mem_pool_cnt==mem_pool_size)
	{
		sedna_rbtree_entry* tmp=(sedna_rbtree_entry*)malloc(sizeof(sedna_rbtree_entry)*2*mem_pool_size);
		memset(tmp+mem_pool_cnt,0,mem_pool_cnt);
		int cnt=root-mem_pool;
		memcpy(tmp,mem_pool,mem_pool_cnt*sizeof(sedna_rbtree_entry));
		free(mem_pool);
		mem_pool_size*=2;
		mem_pool=tmp;
		root=mem_pool+cnt;
	}
	sedna_rbtree_entry* res;
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
	sedna_rbtree_entry::init(res,obj);
	mem_pool_cnt++;
	rb_insert(res);
}


template<class T> void sedna_rbtree<T>::left_rotate(sedna_rbtree_entry* entry)
{
	sedna_rbtree_entry* y=entry->right(mem_pool);
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

template<class T> void sedna_rbtree<T>::right_rotate(sedna_rbtree_entry* entry)
{
	sedna_rbtree_entry* y=entry->left(mem_pool);
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
template<class T> void sedna_rbtree<T>::tree_insert(sedna_rbtree_entry* entry)
{
	sedna_rbtree_entry* y=NULL;
	sedna_rbtree_entry* x=root;
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
template<class T> void sedna_rbtree<T>::rb_insert(sedna_rbtree_entry* entry)
{
	tree_insert(entry);
	sedna_rbtree_entry* x=entry;
	x->black=false;
	while (x!=root && !x->parent(mem_pool)->black)
	{
		if (x->parent(mem_pool)==x->parent(mem_pool)->parent(mem_pool)->left(mem_pool))
		{
			sedna_rbtree_entry* y=x->parent(mem_pool)->parent(mem_pool)->right(mem_pool);
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
			sedna_rbtree_entry* y=x->parent(mem_pool)->parent(mem_pool)->left(mem_pool);
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

template<class T> void sedna_rbtree<T>::rb_delete(sedna_rbtree_entry* entry)
{
	sedna_rbtree_entry* y=entry;
	if (entry->left(mem_pool)!=NULL && entry->right(mem_pool)!=NULL)
		y=rb_successor(y);
	sedna_rbtree_entry* x=y->left(mem_pool);
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
	mem_free_place=(y - mem_pool) + 1;
	mem_pool_cnt--;

}
template<class T> void sedna_rbtree<T>::rb_fixup(sedna_rbtree_entry* entry,sedna_rbtree_entry* parent)
{
	sedna_rbtree_entry* x=entry;
	sedna_rbtree_entry* px=parent;
	while (x!=root && (x==NULL || x->black))
	{
		if (x==px->left(mem_pool))
		{
			sedna_rbtree_entry* w=px->right(mem_pool);
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
			sedna_rbtree_entry* w=px->left(mem_pool);
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
template<class T> typename sedna_rbtree<T>::sedna_rbtree_entry* sedna_rbtree<T>::rb_minimum(sedna_rbtree_entry* entry)
{
	sedna_rbtree_entry* x=entry;
	sedna_rbtree_entry* y=(x!=NULL)?entry->left(mem_pool):NULL;
	while (y!=NULL)
	{
		x=y;
		y=x->left(mem_pool);
	}
	return x;
}
template<class T> typename sedna_rbtree<T>::sedna_rbtree_entry* sedna_rbtree<T>::rb_maximum(sedna_rbtree_entry* entry)
{
	sedna_rbtree_entry* x=entry;
	sedna_rbtree_entry* y=(x!=NULL)?entry->right(mem_pool):NULL;
	while (y!=NULL)
	{
		x=y;
		y=x->right(mem_pool);
	}
	return x;
}
template<class T> typename sedna_rbtree<T>::sedna_rbtree_entry* sedna_rbtree<T>::rb_successor(sedna_rbtree_entry* entry)
{
	sedna_rbtree_entry* x=entry->right(mem_pool);
	if (x!=NULL)
		return rb_minimum(x);
	x=entry;
	sedna_rbtree_entry* y=x->parent(mem_pool);
	while (y!=NULL && x==y->right(mem_pool))
	{
		x=y;
		y=x->parent(mem_pool);
	}
	return y;

}
template<class T> typename sedna_rbtree<T>::sedna_rbtree_entry* sedna_rbtree<T>::rb_predecessor(sedna_rbtree_entry* entry)
{
	sedna_rbtree_entry* x=entry->left(mem_pool);
	if (x!=NULL)
		return rb_maximum(x);
	x=entry;
	sedna_rbtree_entry* y=x->parent(mem_pool);
	while (y!=NULL && x==y->left(mem_pool))
	{
		x=y;
		y=x->parent(mem_pool);
	}
	return y;

}

template<class T> sedna_rbtree<T>* sedna_rbtree<T>::init()
{
	sedna_rbtree* ps=(sedna_rbtree*)malloc(sizeof(sedna_rbtree));
	ps->mem_pool=(sedna_rbtree_entry*)malloc(sizeof(sedna_rbtree_entry)*20);
	ps->mem_pool_size=20;
	ps->mem_pool_cnt=0;
	ps->mem_free_place=0;
	ps->root=NULL;
	return ps;
}


template<class T> void sedna_rbtree<T>::sset_free(sedna_rbtree<T>* set)
{
	free(set->mem_pool);
	free(set);	
}

template<class T> void sedna_rbtree<T>::clear()
{
	mem_pool_cnt=0;
	mem_free_place=0;
	root=NULL;
}

template<class T> typename sedna_rbtree<T>::sedna_rbtree_entry* sedna_rbtree<T>::get (void const* p1, void const* p2)
{
	
	sedna_rbtree_entry* x=root;
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

