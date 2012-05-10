#ifndef _ATOMIZED_PATH_H_
#define _ATOMIZED_PATH_H_

#include "common/sedna.h"

#include "tr/executor/base/xsd.h"
#include "tr/executor/xpath/XPathTypes.h"
#include "tr/executor/xpath/SchemaTests.h"

namespace pe {

struct PathAtom {
protected:
    int _cost;
public:
    int cost() const { return _cost; };

    PathAtom(int __cost) : _cost(__cost) {};
    virtual ~PathAtom() {};
    virtual PathAtom * clone() const  = 0;
    virtual std::ostream & __toString(std::ostream & stream) const = 0;
};

typedef std::vector<PathAtom *> AtomizedPathVector;

#define ATOMPATH_FOR_EACH_CONST(path, var) for (AtomizedPathVector::const_iterator var = (path).begin(); var != (path).end(); ++var)
#define ATOMPATH_FOR_EACH(path, var) for (AtomizedPathVector::iterator var = (path).begin(); var != (path).end(); ++var)

class AtomizedPath {
private:
    bool isMutable;
    counted_ptr<AtomizedPathVector> _list;
    
    AtomizedPathVector::size_type _sliceStart;
    AtomizedPathVector::size_type _sliceEnd;
public:
    inline AtomizedPath() : isMutable(true), _list(new AtomizedPathVector()), _sliceStart(0), _sliceEnd(0) {};

    inline AtomizedPath(const AtomizedPath & parent)
    : isMutable(false), _list(parent._list), _sliceStart(parent._sliceStart), _sliceEnd(parent._sliceEnd) {};

    explicit AtomizedPath(const pe::Path & parent);

    inline explicit AtomizedPath(const AtomizedPath & parent, AtomizedPathVector::size_type _start, AtomizedPathVector::size_type _end)
    : isMutable(false), _list(parent._list), _sliceStart(parent._sliceStart + _start), _sliceEnd(parent._sliceStart + _end) {};

    ~AtomizedPath();

    inline void push_back(PathAtom * pathAtom) {
        U_ASSERT(isMutable);

        _list->push_back(pathAtom);
        _sliceEnd++;
    };

    AtomizedPath operator+(AtomizedPathVector::size_type i) const { return AtomizedPath(*this, i, size()); }

    AtomizedPath reverse() const;

    bool empty() const { return _list.isnull() || _sliceEnd <= _sliceStart; };

    PathAtom * at(std::size_t i) const { return _list->at(_sliceStart + i); };

    inline AtomizedPathVector::size_type size() const { return _sliceEnd - _sliceStart; };
    
    inline AtomizedPathVector::iterator begin() { return _list->begin() + _sliceStart; };
    inline AtomizedPathVector::iterator end() { return _list->begin() + _sliceEnd; };
    
    inline AtomizedPathVector::const_iterator begin() const { return _list->begin() + _sliceStart; };
    inline AtomizedPathVector::const_iterator end() const { return _list->begin() + _sliceEnd; };
    
    inline void setImmutable() { isMutable = false; };
    
    inline int cost() const {
        int __result = 0;
        
        ATOMPATH_FOR_EACH_CONST(*this, _i) {
            __result += (*_i)->cost();
        };
        
        return __result;
    };
    
    std::string __toString() const;
};

#define CHILD_INITIAL_COST 10
#define CLOSURE_INITIAL_COST 10

struct AxisPathAtom : public PathAtom {
    axis_t axis;
    bool closure;
    bool orSelf;

    virtual PathAtom* clone() const ;

    AxisPathAtom(axis_t _axis, bool _closure, bool _orSelf)
    : PathAtom(CHILD_INITIAL_COST), axis(_axis), closure(_closure), orSelf(_orSelf) {
        if (_axis == axis_parent) { _cost = 1; };
        if (_closure) { _cost *= CLOSURE_INITIAL_COST; };
    };

    AxisPathAtom inverse() {
        AxisPathAtom result(*this);

        switch(result.axis) {
            case axis_child:
            case axis_child_or_attribute:
            case axis_attribute:
                result.axis = axis_parent;
                result._cost = 1;

                break;
            case axis_parent:
                result.axis = axis_child_or_attribute;
                result._cost = CHILD_INITIAL_COST;

                if (result.closure) {
                    result._cost *= CLOSURE_INITIAL_COST;
                }

                break;
            default:
                U_ASSERT(false);
                break;
        };

        return result;
    };

    virtual std::ostream & __toString(std::ostream& stream) const;
};

struct SchemaTestAtom : public PathAtom {
    t_item itemType;
    node_test_t nt;
    xsd::TemplateQName qname;

    explicit SchemaTestAtom(t_item _itemType)
        : PathAtom(1), itemType(_itemType), nt(nt_type_test), qname() {};

    explicit SchemaTestAtom(t_item _itemType, const xsd::TemplateQName & _qname, node_test_t _nt)
        : PathAtom(1), itemType(_itemType), nt(_nt), qname(_qname) {};

    SchemaTestData testData() const { return SchemaTestData(itemType, qname.getUri(), qname.getLocalName()); };

    bool test(schema_node_cptr snode) const { return schemaNodeTest(snode, nt, testData()); };

    virtual PathAtom* clone() const ;
    virtual std::ostream & __toString(std::ostream& stream) const;
};

struct CommonAxisAtom : public PathAtom {
    bool closure;
    bool orSelf;

protected:
    CommonAxisAtom(int _cost, bool _closure, bool _orSelf)
        : PathAtom(_cost), closure(_closure), orSelf(_orSelf) {};
};

struct ParentAtom : public CommonAxisAtom {
    ParentAtom(bool _closure, bool _orSelf)
        : CommonAxisAtom(1, _closure, _orSelf) {};

    virtual PathAtom* clone() const;
    virtual std::ostream & __toString(std::ostream& stream) const;
};

struct ChildAtom : public CommonAxisAtom {
    t_item childMask;

    ChildAtom(t_item _childMask, bool _closure, bool _orSelf) 
        : CommonAxisAtom(CHILD_INITIAL_COST, _closure, _orSelf), childMask(_childMask) {
            if (_closure) { _cost *= CLOSURE_INITIAL_COST; }
        };

    virtual PathAtom* clone() const;
    virtual std::ostream & __toString(std::ostream& stream) const;
};

}

#endif /* _ATOMIZED_PATH_H_ */
