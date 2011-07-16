set(base_SRC compare.cpp dm_accessors.cpp
            dynamic_context.cpp static_context.cpp
            PPBase.cpp PPUtils.cpp sequence.cpp
            SequenceType.cpp sorted_sequence.cpp tuple.cpp SortedSequence.cpp
            SCElementProducer.cpp
            namespaces.cpp inscns.cpp
            XMLDateTime.cpp XPath.cpp XPathOnSchema.cpp
            xptr_sequence.cpp xptrChanneledMerge.cpp
            xs_binary.cpp xs_decimal_t.cpp
            xs_fp_converter.cpp xs_helper.cpp xs_names.cpp
            xs_uri.cpp xsd.cpp)

ADD_SUBFOLDER_SRC(base_SRC decNumber)
ADD_SUBFOLDER_SRC(base_SRC lip)
ADD_SUBFOLDER_SRC(base_SRC visitor)
