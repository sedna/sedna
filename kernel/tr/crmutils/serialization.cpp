#include "tr/crmutils/serialization.h"
#include "tr/crmutils/xmlserializer.h"

Serializer * Serializer::createSerializer(enum se_output_method method)
{
    switch (method) {
      case se_output_method_xml:
        return new XMLSerializer();
        break;
      case se_output_method_sxml:
        return new SXMLSerializer();
        break;
      default:
        throw USER_EXCEPTION(SE2301);
    }
}
