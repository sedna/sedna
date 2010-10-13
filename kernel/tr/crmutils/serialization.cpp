#include "tr/crmutils/serialization.h"
#include "tr/crmutils/xmlserializer.h"

Serializer * createSerializer(enum se_output_method method, GlobalSerializationOptions * options)
{
    switch (method) {
      case se_output_method_xml:
        return new XMLSerializer(options);
        break;
      case se_output_method_sxml:
        return new SXMLSerializer(options);
        break;
      default:
        throw USER_EXCEPTION(XXX);
    }
}
