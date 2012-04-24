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

extern btree_blk_hdr a;
namespace sedna { class lr2rqp; }
class ASTAxisStep;

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
  
static const uint32_t nidComparableAxisTest = CDPAAxisTest | selfAxisTest;
  
class Step {
private:
    axis_t axis;
    node_test_t nodeTest;
public:
    xsd::NCName prefix;
    xsd::NCName name;

    Step() : axis(axis_error), nodeTest(nt_error) {};
    Step(const scheme_list * lst);
    Step(axis_t _axis, node_test_t _nodeTest, const xsd::NCName &_prefix, const xsd::NCName &_name)
        : axis(_axis), nodeTest(_nodeTest), prefix(_prefix), name(_name) {};

//    Step(const std::string & s);
//    Step(const ASTAxisStep * s, sedna::lr2rqp * context);

    Step(const Step& x)
        : axis(x.axis), nodeTest(x.nodeTest), prefix(x.prefix), name(x.name) {};

    Step& operator=(const Step& x) {
        axis = x.axis;
        nodeTest = x.nodeTest;
        name = x.name;
        prefix = x.prefix;

        return *this;
    };

    bool satisfies(const StepPredicate & sp) const {
        return (StepPredicate::axis(axis) & sp.axisSet) > 0;
    };

    int compare(const xsd::QName &name, INamespaceMap * _context);

    xsd::QName getQName(INamespaceMap * _context) const;

    axis_t getAxis() const { return axis; };
    node_test_t getTest() const { return nodeTest; }

    std::string toXPathString() const;
    std::string toLRString() const;
};

typedef std::vector<Step> PathVector;
typedef counted_ptr<PathVector> PathVectorPtr;

class PathAtom {
private:
    int _type;
public:
    virtual ~PathAtom() {};
    int type() const { return _type; };
};

enum atom_t {
    atom_axis = 1,
    atom_type,
    atom_qname,
    atom_name,
    atom_prefix,
    atom_union,
};

class AxisPathAtom : public PathAtom { public: 
    axis_t axis;
    bool closure;

    AxisPathAtom(axis_t _axis, bool _closure)
      : _type(atom_axis), axis(_axis), closure(_closure) {};
};

class TypeTestAtom : public PathAtom { public: 
    t_item itemType;

    TypeTestAtom(t_item _itemType)
      : _type(atom_type), itemType(_itemType) {};
};

class QNameTestAtom : public TypeTestAtom { public:
    xsd::QName qname;

    QNameTestAtom(t_item _itemType, const xsd::QName & _qname)
      : _type(atom_qname), itemType(_itemType), qname(_qname) {};
};

class NameTestAtom : public TypeTestAtom { public:
    std::string name;

    NameTestAtom(t_item _itemType, const std::string& _name)
      : _type(atom_name), itemType(_itemType), name(_name) {};
};

class PrefixTestAtom : public TypeTestAtom { public:
    std::string prefix;

    NameTestAtom(t_item _itemType, const std::string& _prefix)
      : _type(atom_prefix), itemType(_itemType), prefix(_prefix) {};
};

class UnionAtom : public PathAtom { public:
    UnionAtom() : _type(atom_union) {};
};

class AtomizedPath_int {
public:
    ~AtomizedPath_int();

    std::vector<PathAtom *> list;
};

typedef counted_ptr<AtomizedPath_int> AtomizedPath;

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

//    Path(const std::string & s);

    Path & operator = (const Path & x) {
        body = x.body;
        return *this;
    };

    Path operator + (const Path & x);
    Path & append(const Step & _step);

    Path inverse() const;
    Path squeeze() const;

    AtomizedPath atomize() const;

    bool inversable() const;

    bool forall(const StepPredicate & sp) const;
    bool exist(const StepPredicate & sp) const;

    PathVectorPtr getBody() const { return body; };
    std::string toXPathString() const;
    std::string toLRString() const;
};

};

#endif /* XPATHLOOKUP_H */
