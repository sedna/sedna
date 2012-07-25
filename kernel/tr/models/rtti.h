#ifndef _RTTI_H_
#define _RTTI_H_

struct class_info_t {
    const char * name;
    int clsid;
    const class_info_t * parent_cls_info;
};

typedef const class_info_t * clsinfo_t;

/**
 * @brief A base class for objects with extended custom RTTI information
 */
class ObjectBase
{
protected:
    clsinfo_t m_info;
public:
    inline ObjectBase(clsinfo_t info) : m_info(info) {};
    inline virtual ~ObjectBase() {};
    inline clsinfo_t info() const { return m_info; };
};

static ObjectBase * const null_obj = (ObjectBase *)(NULL);

#define RTTI_DECL(ID, PUBLIC_PARENT) \
public:\
  static const class_info_t cls_info; \
  enum clsid_t { clsid = (ID) }; \
  typedef PUBLIC_PARENT parent_t; \
private:

#define RTTI_DEF(C) \
const class_info_t C::cls_info = {#C, C::clsid, &(C::parent_t::cls_info)}; \

#define RTTI_DEF_BASE(C) \
const class_info_t C::cls_info = {#C, C::clsid, NULL}; \

#define SELF_RTTI_REF &cls_info

template<class T> inline static
bool instanceof(ObjectBase * obj) { return obj != null_obj && obj->info()->clsid == T::clsid; };


#define CASE_TYPE(TYPE) \
   case TYPE::clsid :

#define CASE_TYPE_CAST(TYPE, CASTED_OBJ, OBJ) \
   case TYPE::clsid : if (TYPE * CASTED_OBJ = static_cast<TYPE *>(OBJ))

#endif /* _RTTI_H_ */
