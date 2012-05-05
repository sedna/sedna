/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef XPATHTYPES_H
#define XPATHTYPES_H

#include <string>
#include <vector>

#include "tr/executor/base/xsd.h"

/* Namespace pe stands for PathEvaluator. */

namespace pe {

enum axis_t {
    axis_error = 0, /* When not used, or implied */

    axis_child,
    axis_descendant,
    axis_attribute,
    axis_self,
    axis_descendant_or_self,
    axis_parent,

    axis_ancestor,
    axis_ancestor_or_self,
    axis_following,
    axis_following_sibling,
    axis_preceding,
    axis_preceding_sibling,

    axis_last,

    axis_child_or_attribute, /* Special axis */
};

enum node_test_t {
    nt_error = 0,

    nt_document,
    nt_element,
    nt_attribute,
    nt_schema_element,
    nt_schema_attribute,
    nt_pi,
    nt_comment,
    nt_text,
    nt_any_kind,

    nt_qname,
    nt_wildcard_star,
    nt_wildcard_prefix,
    nt_wildcard_name,

    nt_last,

    nt_type_test,
};

struct StepPredicate {
    uint32_t axisSet;
    StepPredicate(uint32_t _axisSet) : axisSet(_axisSet) {};
    inline static uint32_t axis(axis_t _axis) { return 1 << _axis; };
};

static const uint32_t childParentAxisTest =
  StepPredicate::axis(axis_child) | StepPredicate::axis(axis_parent);
  
static const uint32_t CDPAAxisTest =
  StepPredicate::axis(axis_child) | StepPredicate::axis(axis_parent) | StepPredicate::axis(axis_descendant) | StepPredicate::axis(axis_ancestor) | StepPredicate::axis(axis_descendant_or_self) | StepPredicate::axis(axis_ancestor_or_self);

static const uint32_t selfAxisTest = StepPredicate::axis(axis_self);

struct StepTest {
    node_test_t nodeTest;
    xsd::TemplateQName qname;

    StepTest(node_test_t _nodeTest, const xsd::TemplateQName &_qname)
     : nodeTest(_nodeTest), qname(_qname) {};
};

class Step {
private:
    axis_t axis;
public:
    StepTest test;
  
    Step() : axis(axis_error), test(nt_error, xsd::QNameAny) {};

    Step(const scheme_list * lst);

    Step(axis_t _axis, const StepTest& _test)
        : axis(_axis), test(_test) {};

    Step(axis_t _axis, node_test_t _nodeTest, const xsd::TemplateQName &_qname)
        : axis(_axis), test(_nodeTest, _qname) {};

    Step(axis_t _axis, node_test_t _nodeTest, const char * _prefix, const char * _name)
        : axis(_axis), test(_nodeTest, xsd::TemplateQName(_prefix, _name)) {};

    Step(const Step& x) : axis(x.axis), test(x.test) {};

    Step& operator=(const Step& x) {
        axis = x.axis;
        test = x.test;

        return *this;
    };

    bool satisfies(const StepPredicate & sp) const {
        return (StepPredicate::axis(axis) & sp.axisSet) > 0;
    };

    int compare(const xsd::QName &name, INamespaceMap * _context);

    xsd::QName getQName(INamespaceMap * _context) const;

    axis_t getAxis() const { return axis; };
    StepTest getTest() const { return test; }

    std::string toXPathString() const;
    std::string toLRString() const;
};

class PathAtom {
protected:
    int _cost;
public:
    PathAtom(int __cost) : _cost(__cost) {};
    virtual ~PathAtom() {};
    int cost() const { return _cost; };
    virtual PathAtom * clone() = 0;
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
      
    inline explicit AtomizedPath(const AtomizedPath & parent, AtomizedPathVector::size_type _start, AtomizedPathVector::size_type _end)
      : isMutable(false), _list(parent._list), _sliceStart(parent._sliceStart + _start), _sliceEnd(parent._sliceStart + _end) {};

    ~AtomizedPath();

    inline void push_back(PathAtom * pathAtom) {
        U_ASSERT(isMutable);

        _list->push_back(pathAtom);
        _sliceEnd++;
    };

    AtomizedPath operator+(AtomizedPathVector::size_type i) { return AtomizedPath(*this, i, size()); }

    AtomizedPath reverse() const;

    bool empty() const { return _list.isnull() || _sliceEnd <= _sliceStart; };

    PathAtom * at(AtomizedPathVector::size_type i) const { return _list[i]; };

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

typedef std::vector<Step> PathVector;
typedef counted_ptr<PathVector> PathVectorPtr;


class Path {
private:
    PathVectorPtr body;
    mutable size_t hash;
private:
    void modify();
public:
    struct EmptyPath {};

    Path() : body(NULL), hash(0) {};

    explicit Path(const EmptyPath &) : body(new PathVector()), hash(0) {};

    Path(const scheme_list * lst);
    Path(const Step & x);
    Path(const Path & x) : body(x.body), hash(x.hash) {};

    Path & operator = (const Path & x) {
        body = x.body;
        return *this;
    };

    bool forall(const StepPredicate & sp) const;
    bool exist(const StepPredicate & sp) const;

    bool valid() const;
    bool inversable() const;
    bool horizontal() const;

    Path inverse(const StepTest & baseTest) const;
    
    Path operator + (const Path & x);
    Path & append(const Step & _step);

    AtomizedPath atomize() const;

    PathVectorPtr getBody() const { return body; };

    std::string toXPathString() const;
    std::string toLRString() const;
};

#define CHILD_INITIAL_COST 10
#define CLOSURE_INITIAL_COST 10

class AxisPathAtom : public PathAtom { public:
    axis_t axis;
    bool closure;
    bool orSelf;

    virtual PathAtom* clone();
    
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

class TypeTestAtom : public PathAtom {
protected:
    TypeTestAtom(int __cost, t_item _itemType, node_test_t _nt) : PathAtom(__cost), itemType(_itemType), nt(_nt)  {};
public:
    t_item itemType;
    node_test_t nt;
    
    TypeTestAtom(t_item _itemType) : PathAtom(1), itemType(_itemType), nt(nt_type_test) {};

    virtual PathAtom* clone();
    virtual std::ostream & __toString(std::ostream& stream) const;
};

class NameTestAtom : public TypeTestAtom { public:
    xsd::TemplateQName qname;

    NameTestAtom(t_item _itemType, const xsd::TemplateQName & _qname, node_test_t _nt) : TypeTestAtom(1, _itemType, _nt), qname(_qname) {};
    virtual PathAtom* clone();
    virtual std::ostream & __toString(std::ostream& stream) const;
};

};

#endif /* XPATHLOOKUP_H */
