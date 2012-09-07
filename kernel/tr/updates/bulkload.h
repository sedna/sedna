/*
 * File:  bulkload.h
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "common/sedna.h"

#include "tr/structures/nodeinterface.h"

struct BulkLoadOptions {
    bool stripBoundarySpaces;
    bool preserveCDataSection;
};

class BulkLoadFrontend {
  private:
    bool ownStream;
    std::istream * inputStream;
  public:
    BulkLoadOptions options;

    BulkLoadFrontend();
    ~BulkLoadFrontend();

    void setSourceStream(std::istream * _stream);

    Node loadDocument(const char * documentName);
    Node loadCollectionDocument(const char * collectionName, const char * documentName);
};
