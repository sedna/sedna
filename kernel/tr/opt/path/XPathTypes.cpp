/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/internstr.h"

#include "XPathTypes.h"
#include "tr/xqp/ast/ASTStep.h"

#include <iostream>
#include <sstream>
#include <stack>

using namespace pe;

struct axis_pair_t {
    pe::axis_t type;
    const char * str;
};

struct node_test_pair_t {
    pe::node_test_t type;
    const char * str;
};

static const axis_pair_t axisTypeMap[] = {
    { axis_error, "errornous" },
    { axis_child, "child" },
    { axis_descendant, "descendant" },
    { axis_attribute, "attribute" },
    { axis_self, "self" },
    { axis_descendant_or_self, "descendant-or-self" },
    { axis_parent, "parent" },
    { axis_ancestor, "ancestor" },
    { axis_ancestor_or_self, "ancestor-or-self" },
    { axis_following, "following" },
    { axis_following_sibling, "following-sibling" },
    { axis_preceding, "preceding" },
    { axis_preceding_sibling, "preceding-sibling" },
};


static const node_test_pair_t nodeTypeMap[] = {
    { nt_error, "errornous"},
    { nt_document, "document-node"},
    { nt_element, "element"},
    { nt_attribute, "attribute"},
    { nt_schema_element, "schema-element"},
    { nt_schema_attribute, "schema-attribute"},
    { nt_pi, "processing-instruction"},
    { nt_comment, "comment"},
    { nt_text, "text" },
    { nt_any_kind, "node" },
    { nt_qname, "qname" },
    { nt_wildcard_star, "star" },
    { nt_wildcard_prefix, "star_prefix" },
    { nt_wildcard_name, "star_name" }
};


#define axisTypeMapSize ((int) (sizeof(axisTypeMap) / sizeof(axis_pair_t)))
#define nodeTypeMapSize ((int) (sizeof(nodeTypeMap) / sizeof(node_test_pair_t)))


class __AxisTypeMap : private sedna::ConstStringHashMap {
public:
    __AxisTypeMap() : ConstStringHashMap() {
        for (int i = 0; i < axisTypeMapSize; ++i) {
            put(axisTypeMap[i].str, axisTypeMap + i);
        };
    };
    
    const axis_pair_t * get(const char * str) const {
        return (const axis_pair_t *) sedna::ConstStringHashMap::get(str);
    }
};

static const __AxisTypeMap axis_type_map;

class __NodeTypeMap : private sedna::ConstStringHashMap {
public:
    __NodeTypeMap() : ConstStringHashMap() {
        for (int i = 0; i < nodeTypeMapSize; ++i) {
            put(nodeTypeMap[i].str, nodeTypeMap + i);
        };
    };
    
    const node_test_pair_t * get(const char * str) const {
        return (const node_test_pair_t *) sedna::ConstStringHashMap::get(str);
    }
};

static const __NodeTypeMap node_type_map;

static inline
const char * axisTypeToStr(pe::axis_t axis) {
    if (axis >= axis_error && axis < axis_last) {
        return axisTypeMap[axis].str;
    } else {
        return NULL;
    }
}

static inline
const char * nodeTypeToStr(pe::node_test_t node) {
    if (node >= nt_error && node < nt_last) {
        return nodeTypeMap[node].str;
    } else {
        return NULL;
    }
}

const char * invalidLRStep = "Invalid step LR representation";

Step::Step(const scheme_list* lst) : axis(axis_error), test(nt_error, xsd::QNameAny)
{
    const axis_pair_t * _axis = axis_type_map.get(scmGetSymbol(lst, 0, invalidLRStep));
    const node_test_pair_t * _node_test = node_type_map.get(scmGetSymbol(lst, 1, invalidLRStep));

    axis = _axis == NULL ? axis_error : _axis->type;
    test.nodeTest = _node_test == NULL ? nt_error : _node_test->type;

    switch (test.nodeTest) {
        case nt_text:
        case nt_comment:
            break;
        case nt_attribute:
        case nt_pi:
        case nt_element:
            test.qname = xsd::TemplateQName(xsd::QNameWildcard, scmGetString(lst, 2, invalidLRStep));
            break;
        default:
            test.qname = xsd::TemplateQName(
              scmGetString(lst, 2, invalidLRStep),
              scmGetString(lst, 3, invalidLRStep));
    }
}

Path::Path(const scheme_list* lst)
{
    std::size_t len = lst->size();

    if (len < 1 || !CL_CHECK_SYMBOL(lst, 0, "path")) {
        throw USER_EXCEPTION2(SE1004, "Invalid path LR string");
    }

    modify();
    body->reserve(len - 1);

    for (std::size_t i = 1; i < len; ++i) {
        body->push_back(Step(scmGetList(lst, i, "Invalid path LR string")));
    }
}

xsd::QName Step::getQName(INamespaceMap* _context) const
{
    U_ASSERT(false);
    return xsd::QName();
//    return test.qname.toQName(_context);
}


std::string Step::toXPathString() const
{
    std::stringstream stream;

    stream << "/";

    if (axis != axis_error) {
        stream << axisTypeToStr(axis) << "::";
    }

    stream << nodeTypeToStr(test.nodeTest) << test.qname.getXPathName();

    return stream.str();
}

std::string Path::toXPathString() const
{
    if (body.isnull()) {
        return "";
    }

    std::stringstream stream;

    for (PathVector::const_iterator i = body->begin(); i != body->end(); ++i) {
        stream << i->toXPathString();
    }

    return stream.str();
}

std::string Step::toLRString() const
{
    std::stringstream stream;

    stream << "("
        << axisTypeToStr(axis) << " "
        << nodeTypeToStr(test.nodeTest) << " "
        << test.qname.getColonizedName();

    stream << ")";

    return stream.str();
}

std::string Path::toLRString() const
{
    std::stringstream stream;

    stream << "(path";

    for (PathVector::const_iterator i = body->begin(); i != body->end(); ++i) {
        stream << " " << i->toLRString();
    }

    stream << ")";

    return stream.str();
}



Path::Path(const pe::Step& x)
    : body(new PathVector())
{
    body->push_back(x);
}

void Path::modify()
{
    if (body.isnull()) {
        body = new PathVector();
    }

    if (!body.unique()) {
        body = new PathVector(*body);
    }
}


Path& Path::append(const pe::Step& _step)
{
    modify();
    body->push_back(_step);
    return *this;
}

Path Path::operator+(const pe::Path& x)
{
    Path result;

    result.modify();

    size_t lsize = (this->body.isnull() ? 0 : this->body->size());
    size_t rsize = (x.body.isnull() ? 0 : x.body->size());

    result.body->reserve(lsize + rsize);

    if (lsize > 0) {
        result.body->insert(result.body->end(), this->body->begin(), this->body->end());
    }

    if (rsize > 0) {
        result.body->insert(result.body->end(), x.body->begin(), x.body->end());
    }

    return result;
}


static const axis_t inverseAxis[] = {
  axis_error, //    axis_error = 0, /* When not used, or implied */

  axis_parent,  // axis_child,
  axis_ancestor,  // axis_descendant,
  axis_parent,  // axis_attribute,
  axis_self,  // axis_self,
  axis_ancestor_or_self, // axis_descendant_or_self,

  axis_error,
  axis_error,
  axis_error,
  axis_error,
  axis_error,
  axis_error,
  axis_error,
  axis_error,
  axis_error,
  axis_error,
  axis_error,
};


Path Path::inverse(const StepTest & baseTest) const
{
    U_ASSERT(inversable());
  
    Path result;

    if (body.isnull()) {
        return result;
    };

    std::stack<Step> stepStack;

    StepTest previousTest = baseTest;
    for (PathVector::const_iterator i = body->begin(); i != body->end(); ++i) {
        axis_t possibleAxis = inverseAxis[i->getAxis()];
        U_ASSERT(possibleAxis != axis_error);
        stepStack.push(Step(possibleAxis, previousTest));
        previousTest = i->getTest();
    }

    result.modify();
    result.body->reserve(this->body->size());

    while (!stepStack.empty()) {
        result.body->push_back(stepStack.top());
        stepStack.pop();
    };
    
    return result;
}

bool Path::inversable() const
{
    return forall(StepPredicate(ChildAttrAxisTest));
}

static const StepPredicate 
  siblingAxes(
    StepPredicate::axis(axis_following) |
    StepPredicate::axis(axis_following_sibling) |
    StepPredicate::axis(axis_preceding) |
    StepPredicate::axis(axis_preceding_sibling));

bool Path::horizontal() const
{
    return exist(siblingAxes);
}

bool Path::forall(const pe::StepPredicate& sp) const
{
    if (body.isnull()) {
        return false;
    };

    for (PathVector::const_iterator i = body->begin(); i != body->end(); ++i) {
        if (!i->satisfies(sp)) { return false; }
    }

    return true;
}

bool Path::exist(const pe::StepPredicate& sp) const
{
    if (body.isnull()) {
        return false;
    };

    for (PathVector::const_iterator i = body->begin(); i != body->end(); ++i) {
        if (i->satisfies(sp)) { return true; }
    }

    return false;
}

