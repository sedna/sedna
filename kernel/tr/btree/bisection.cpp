/*
 * File:  bisection.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/btree/btintern.h"

/* Search the object table inside single page for the given object via bisection algorithm, supposing the
   objects are value-ordered. Returns true setting the obj_idx
   to the index of hit element in object table if the object was found; returns false setting the obj_idx
   to the index of nearest bigger element in object table otherwise. In case the searched object is bigger
   than all objects in the object table, obj_idx is set to BT_RIGHTMOST, in case smaller than all objects -
   to BT_LEFTMOST */
template<typename object>
bool bt_locate_obj_bisection_tmpl(const object* ar, shft ar_size, const object &obj, shft /*out*/ &obj_idx)
{
    if (ar_size <= 0)
    {
        obj_idx = BT_LEFTMOST;
        return false;
	}

    int     rc;
    shft    l_idx = 0;
    shft    r_idx = ar_size - 1;
    shft    m_idx = l_idx + (r_idx - l_idx) / 2;

    while (l_idx != m_idx)
    {
        rc = bt_cmp_obj_tmpl<object>(*(ar + m_idx), obj);
        if (!rc)
        { /* ! obj found */
            obj_idx = m_idx;
            return true;
        }
        else if (rc > 0) r_idx = m_idx;
        else l_idx = m_idx;
        m_idx = l_idx + (r_idx - l_idx) / 2;
    }

    /* now m_idx == l_idx */
    if (l_idx < r_idx)
    {
        rc = bt_cmp_obj_tmpl<object>(*(ar + l_idx), obj);

        if (!rc)
        { /* obj found */
            obj_idx = l_idx;
            return true;
		}
        else if (rc > 0)
        {
            if (l_idx != 0) throw USER_EXCEPTION2(SE1008, "Incorrect bisection algorithm functioning");
			obj_idx = BT_LEFTMOST;
			return false;
		}
        else
        {
            rc = bt_cmp_obj_tmpl<object>(*(ar + r_idx), obj);
            if (!rc)
            { /* obj found */
                obj_idx = r_idx;
                return true;
			}
            else if (rc > 0)
            {
                obj_idx = r_idx;
                return false;
			}
            else
            { /* the searched obj is above all objs in the page */
                if (r_idx != ar_size - 1) throw USER_EXCEPTION2(SE1008, "Incorrect bisection algorithm functioning");
                obj_idx = BT_RIGHTMOST;
                return false;
            }
        }
    }
    else
    { /* ! must be l_idx == r_idx */
        rc = bt_cmp_obj_tmpl<object>(*(ar + r_idx), obj);
        if (!rc)
        { /* obj found */
            obj_idx = r_idx;
            return true;
        }
        else if (rc > 0)
        {
            if (r_idx != 0) throw USER_EXCEPTION2(SE1008, "Incorrect bisection algorithm functioning");
            obj_idx = BT_LEFTMOST;
            return false;
        }
        else
        { /* the searched obj is above all objs in the page */
            obj_idx = BT_RIGHTMOST;
            return false;
        }
    }
}

/* Search the key table inside single page for the given key via bisection algorithm. Returns true setting the key_idx
   to the index of hit element in key table if the key was found; returns false setting the key_idx
   to the index of nearest bigger element in key table otherwise. In case the searched key is bigger
   than all keys in the key table, key_idx is set to BT_RIGHTMOST, in case smaller than all keys -
   to BT_LEFTMOST */
bool bt_locate_key_bisection(char* pg, const char* ar, shft ar_size, shft ar_el_size, const bt_key& key, shft &key_idx,bool with_bt)
{
    if (ar_size <= 0)
    {
        key_idx = BT_RIGHTMOST;
        return false;
    }

    int     rc;
    shft    r_idx = ar_size - 1;
	shft    l_idx = (with_bt)?0:r_idx;
    shft    m_idx = l_idx + (r_idx - l_idx) / 2;

    while (l_idx != m_idx)
    {
        rc = bt_cmp_key(pg, ar + m_idx * ar_el_size, key);
        if (!rc)
        { /* ! key found */
            key_idx = m_idx;
            return true;
        }
        else if (rc > 0) r_idx = m_idx;
        else l_idx = m_idx;
		m_idx = l_idx + (r_idx - l_idx) / 2;
	}

	/* now m_idx == l_idx */
    if (l_idx < r_idx)
    {
        rc = bt_cmp_key(pg, ar + l_idx * ar_el_size, key);
        if (!rc)
        { /* key found */
            key_idx = l_idx;
            return true;
        }
        else if (rc > 0)
        {
            if (l_idx != 0) throw USER_EXCEPTION2(SE1008, "Incorrect bisection algorithm functioning");
            key_idx = BT_LEFTMOST;
            return false;
        }
        else
        {
            rc = bt_cmp_key(pg, ar + r_idx * ar_el_size, key);
            if (!rc)
            { /* key found */
                key_idx = r_idx;
                return true;
            }
            else if (rc > 0)
            {
                key_idx = r_idx;
                return false;
            }
            else
            { /* the searched key is above all keys in the page */
                if (r_idx != ar_size - 1) throw USER_EXCEPTION2(SE1008, "Incorrect bisection algorithm functioning");
                key_idx = BT_RIGHTMOST;
                return false;
            }
        }
    }
    else
    { /* ! must be l_idx == r_idx */
        rc = bt_cmp_key(pg, ar + r_idx * ar_el_size, key);
        if (!rc)
        { /* key found */
            key_idx = r_idx;
            return true;
        }
        else if (rc > 0)
        {
            if (r_idx != 0) throw USER_EXCEPTION2(SE1008, "Incorrect bisection algorithm functioning");
            key_idx = BT_LEFTMOST;
            return false;
        }
        else
        { /* the searched key is above all keys in the page */
            key_idx = BT_RIGHTMOST;
            return false;
        }
    }
}



#define MAKE_IMPLS(t) template bool bt_locate_obj_bisection_tmpl<t>(const t* ar, shft ar_size, const t &obj, shft /*out*/ &obj_idx);

#include "tr/btree/make_impl.h"
