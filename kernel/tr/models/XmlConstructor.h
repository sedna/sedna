#ifndef _XML_CONSTRUCTOR_H_
#define _XML_CONSTRUCTOR_H_

#include "IElementProducer.h"

#include <stack>

class XmlConstructor;

class VirtualRootConstructor {
public :
    VirtualRootConstructor(int x) {};
};

class XmlConstructor
{
    tuple_cell lastElement;
    std::stack<IElementProducer *> producers;
public:
    explicit XmlConstructor(const VirtualRootConstructor &);
    ~XmlConstructor() { while (!producers.empty()) { delete producers.top(); producers.pop(); }; };

    XmlConstructor(const XmlConstructor & _producer) { U_ASSERT(false); };
    XmlConstructor& operator= (const XmlConstructor & _producer) { U_ASSERT(false); return *this; };
   
    tuple_cell addElementValue(const xsd::QName &qname, const tuple_cell value, xmlscm_type type = xs_untypedAtomic);
    tuple_cell addElementValue(const xsd::QName &qname, const std::string & str, xmlscm_type type = xs_untypedAtomic);
    tuple_cell addAttributeValue(const xsd::QName &qname, const tuple_cell value, xmlscm_type type = xs_untypedAtomic);
    tuple_cell addAttributeValue(const xsd::QName &qname, const std::string & str, xmlscm_type type = xs_untypedAtomic);
    tuple_cell addText(const tuple_cell value);
    tuple_cell addText(const std::string & str);

    void openElement(const xsd::QName &qname, xmlscm_type type = xs_untypedAtomic);
    void closeElement();
    
    tuple_cell getLastChild() const { return lastElement; };
};

class IXMLSerializable {
public:
    virtual XmlConstructor& toXML(XmlConstructor& constructor) const = 0;
    tuple_cell serialize() const;
    void toStream(std::ostream & stream) const;
};

#endif /* _XML_CONSTRUCTOR_H_ */
