#include "XmlConstructor.h"

#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/models/SCElementProducer.h"

tuple_cell IXMLSerializable::serialize() const
{
    XmlConstructor constructor(VirtualRootConstructor(0));
    return this->toXML(constructor).getLastChild();
}

void XmlConstructor::openElement(const xsd::QName& qname, xmlscm_type type)
{
    producers.push(producers.top()->addElement(qname, type));
}


void XmlConstructor::closeElement()
{
    lastElement = producers.top()->close();
    delete producers.top();
    producers.pop();
}


tuple_cell XmlConstructor::addAttributeValue(const xsd::QName& qname, const tuple_cell value, xmlscm_type type)
{
    tuple_cell atmoized_value = atomize(value); 
    tuple_cell str_value = cast(atmoized_value, xs_string);
    text_source_t str = text_source_tuple_cell(str_value);
    return producers.top()->addAttribute(qname, str, atmoized_value.get_atomic_type());
}

tuple_cell XmlConstructor::addAttributeValue(const xsd::QName& qname, const std::string& str, xmlscm_type type)
{
    return producers.top()->addAttribute(qname, text_source_cstr(str.c_str()), xs_string);
}


tuple_cell XmlConstructor::addElementValue(const xsd::QName& qname, const tuple_cell value, xmlscm_type type)
{
    tuple_cell atmoized_value = atomize(value);

    openElement(qname, atmoized_value.get_atomic_type());
    addText(atmoized_value);
    closeElement();

    return getLastChild();
}

tuple_cell XmlConstructor::addElementValue(const xsd::QName& qname, const std::string& str, xmlscm_type type)
{
    openElement(qname, xs_string);
    addText(str);
    closeElement();

    return getLastChild();
}

tuple_cell XmlConstructor::addText(const tuple_cell value)
{
    tuple_cell str_value = cast(atomize(value), xs_string);
    text_source_t str = text_source_tuple_cell(str_value);

    return producers.top()->addText(str);
}

tuple_cell XmlConstructor::addText(const std::string& str)
{
    return producers.top()->addText(text_source_cstr(str.c_str()));
}


XmlConstructor::XmlConstructor(const VirtualRootConstructor& )
{
    producers.push(SCElementProducer::getVirtualRoot(XNULL));
}
