#include "AtomizedPath.h"

#include <sstream>
#include <iomanip>

using namespace pe;

static const axis_t atomizeAxis[] = {
    axis_error, //    axis_error = 0, /* When not used, or implied */
    
    axis_child,  // axis_child,
    axis_child,  // axis_descendant,
    axis_attribute,  // axis_attribute,
    axis_self,  // axis_self,
    axis_child, // axis_descendant_or_self,
    
    axis_parent, //  axis_parent,
    axis_parent, //  axis_ancestor,
    axis_parent, //  axis_ancestor_or_self,
    
    axis_error, //  axis_following,
    axis_error, //  axis_following_sibling,
    axis_error, //  axis_preceding,
    axis_error, //  axis_preceding_sibling,
    
    axis_error, //  axis_last,
    
    axis_error, //  axis_child_or_attribute, /* Special axis */
};

AtomizedPath::AtomizedPath(PathVector::const_iterator begin, PathVector::const_iterator end)
{
    _list = new AtomizedPathVector;

    for (PathVector::const_iterator i = begin; i != end; ++i) {
        bool closure = false, orSelf = false;
        t_item pnk = ti_dmchildren;
        const pe::Step & step = *i;
        axis_t actual_axis = atomizeAxis[step.getAxis()];

        U_ASSERT(actual_axis != axis_error);

        switch (actual_axis) {
            case axis_attribute:
                pnk = attribute; break;
            case axis_self:
                pnk = ti_all_valid; break;
            default: break;
        };

        switch (step.getAxis()) {
            case axis_descendant_or_self :
            case axis_ancestor_or_self :
                orSelf = true;
            case axis_descendant :
            case axis_ancestor :
                closure = true;
            case axis_attribute :
            case axis_child :
            case axis_parent:
                push_back(new AxisPathAtom(actual_axis, closure, orSelf));
            case axis_self :
                break;
            default :
                U_ASSERT(false);
                break;
        };

        switch (step.getTest().nodeTest) {
            case nt_document :   pnk = document; break;
            case nt_element :    pnk = element; break;
            case nt_attribute :  pnk = attribute; break;
            case nt_pi :         pnk = pr_ins; break;
            case nt_comment :    pnk = comment; break;
            case nt_text :       pnk = text; break;
            default : break;
        };

        switch (step.getTest().nodeTest) {
            case nt_element :
            case nt_attribute :
                // FIXME : is there really no prefix test so???
                if (step.getTest().qname.getLocalName() == xsd::QNameWildcard) {
            case nt_document :
            case nt_comment :
            case nt_text :
            case nt_wildcard_star :
                push_back(new SchemaTestAtom(pnk));
                break;
                } else if (step.getTest().qname.getUri() == xsd::QNameWildcard) {
            case nt_pi :
            case nt_wildcard_name :
                // TRICKY : do not try to optimize!!!
                push_back(new SchemaTestAtom(pnk, step.getTest().qname, nt_wildcard_name));
                break;
                } else {
            case nt_qname :
                push_back(new SchemaTestAtom(pnk, step.getTest().qname, nt_qname));
                break;
                };
            case nt_wildcard_prefix :
                push_back(new SchemaTestAtom(pnk, step.getTest().qname, nt_wildcard_prefix));
                break;

            case nt_any_kind :
                break;
            case nt_schema_element :
            case nt_schema_attribute :
            default:
                U_ASSERT(false);
                break;
        };
    }
}

AtomizedPath AtomizedPath::reverse() const
{
    AtomizedPath result;
    
    result._list->reserve(size());
    
    for (AtomizedPathVector::const_iterator i = this->end()-1; i != this->begin()-1; --i) {
        AxisPathAtom * apa = dynamic_cast<AxisPathAtom *>(*i);
        
        if (apa != NULL) {
            result.push_back(new AxisPathAtom(apa->inverse()));
        } else {
            result.push_back((*i)->clone());
        };
    };
    
    result.setImmutable();
    
    return result;
}

AtomizedPath::~AtomizedPath()
{
    if (_list.unique()) {
        for (std::vector<PathAtom *>::const_iterator i = _list->begin(); i != _list->end(); ++i) {
            delete *i;
        }
    }
}

PathAtom* AxisPathAtom::clone() const
{
    return new AxisPathAtom(*this);
}

PathAtom* SchemaTestAtom::clone() const
{
    return new SchemaTestAtom(*this);
}

PathAtom* ChildAtom::clone() const
{
    return new ChildAtom(*this);
}

PathAtom* ParentAtom::clone() const
{
    return new ParentAtom(*this);
}


std::ostream & AxisPathAtom::__toString(std::ostream& stream) const
{
    switch(axis) {
        case axis_child:
            stream << "C";
            break;
        case axis_child_or_attribute:
            stream << "D";
            break;
        case axis_attribute:
            stream << "A";
            break;
        case axis_parent:
            stream << "P";
            break;
        default:
            U_ASSERT(false);
            break;
    };

    if (orSelf) {
        stream << "+";
    } else if (closure) {
        stream << "*";
    };
    
    return stream;
}

std::ostream& SchemaTestAtom::__toString(std::ostream& stream) const
{
    stream << "N{" << std::setbase(2) << itemType << " " << qname.getColonizedName() << "}";
    return stream;
}

std::string AtomizedPath::__toString() const
{
    std::stringstream ss;
    
    ATOMPATH_FOR_EACH_CONST(*this, it) {
        (*it)->__toString(ss);
    };
    
    return ss.str();
}

std::ostream& ChildAtom::__toString(std::ostream& stream) const
{
    stream << "D{" << std::setbase(2) << childMask << "}";

    if (orSelf) {
        stream << "+";
    } else if (closure) {
        stream << "*";
    };

    return stream;
}

std::ostream& ParentAtom::__toString(std::ostream& stream) const
{
    stream << "P";

    if (orSelf) {
        stream << "+";
    } else if (closure) {
        stream << "*";
    };

    return stream;
}
