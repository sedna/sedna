/* Unfortunately hash_map is incompatible across 
 * windows and linux. The differenece is subtle
 * (header file, namespace and the way to setup
 * custom hash function) so we can hide these details
 * with conditional defines.
 *
 * We define the following macroses:
 *
 * U_HASH_MAP(KEY_TYPE, VALUE_TYPE) - hash_map template instantiation
 * 				      with selected type parameters
 *
 * U_HASH_MAP_W_CUSTOM_HASH_FN(KEY_TYPE, VALUE_TYPE, HASH_FUNCTION) -
 * 				    hash_map template instantiation
 * 				    with selected type parameters
 * 				    and custom hash function; 
 * 				    HASH_FUNCTION must be a class
 * 				    with overloaded operator
 * 				    
 * 				    size_t operator() (const KEY_TYPE &) const
 *
 * 				    which computes a hash function of the
 * 				    given key
 * 				    
 */

#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef UHASH_MAP_H_INCLUDED
#define UHASH_MAP_H_INCLUDED

#ifdef _WIN32

#include <hash_map>

namespace __sedna_U
{
	template<typename key_t,
		typename hash_fn_t>
	class hash_fn_2_hash_compare_adaptor: 
		public stdext::hash_compare<key_t>
	{
		hash_fn_t m_hash_fn;
	public:
		bool operator() (const key_t& kvala, const key_t& kvalb) const
		{
			return stdext::hash_compare<key_t>::operator() (kvala, kvalb);
		}

		size_t operator() (const key_t& kval) const
		{
			return m_hash_fn.operator () (kval);
		}
	};
};

#define U_HASH_MAP(KEY_TYPE, VALUE_TYPE) \
	stdext::hash_map<KEY_TYPE, VALUE_TYPE>

#define U_HASH_MAP_W_CUSTOM_HASH_FN(KEY_TYPE, VALUE_TYPE, HASH_FUNCTION) \
	stdext::hash_map<KEY_TYPE, VALUE_TYPE, __sedna_U:: \
		hash_fn_2_hash_compare_adaptor<KEY_TYPE, HASH_FUNCTION> >

#else

#include <ext/hash_map>

#define U_HASH_MAP(KEY_TYPE, VALUE_TYPE) \
	__gnu_cxx::hash_map<KEY_TYPE, VALUE_TYPE>

#define U_HASH_MAP_W_CUSTOM_HASH_FN(KEY_TYPE, VALUE_TYPE, HASH_FUNCTION) \
	__gnu_cxx::hash_map<KEY_TYPE, VALUE_TYPE, HASH_FUNCTION>

#endif

#endif
