/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef XPATHTYPES_H
#define XPATHTYPES_H

#include <string>
#include <vector>
#include <set>

#include "tr/executor/base/xsd.h"

/* Namespace pe stands for PathEvaluator. */

typedef std::set<schema_node_xptr> SchemaNodePtrSet;
typedef std::vector<schema_node_xptr> SchemaNodePtrList;
typedef std::vector<schema_node_cptr> SchemaNodeList;

static
SchemaNodeList toNodeSet(const SchemaNodePtrList & nptrset)
{
    SchemaNodeList result;

    result.reserve(nptrset.size());

    for (SchemaNodePtrList::const_iterator it = nptrset.begin(); it != nptrset.end(); ++it) {
        result.push_back(*it);
    };
    
    return result;
};

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

    PathVectorPtr getBody() const { return body; };

    std::string toXPathString() const;
    std::string toLRString() const;
};

};

#endif /* XPATHLOOKUP_H */
