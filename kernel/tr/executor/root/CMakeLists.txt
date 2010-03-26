set(root_SRC PPBulkLoad.cpp PPCreateIndex.cpp PPCreateMetadata.cpp PPDeleteDeep.cpp
       PPDeleteUndeep.cpp PPDropIndex.cpp PPDropMetadata.cpp PPDropModule.cpp
       PPInsertBefore.cpp PPInsertFollowing.cpp PPInsertTo.cpp PPLoadModule.cpp PPQueryRoot.cpp
       PPRename.cpp PPReplace.cpp PPRetrieveDS.cpp PPRetrieveMetadata.cpp PPSecurity.cpp)

if (ENABLE_FTSEARCH)
    list(APPEND root_SRC PPDropFtIndex.cpp PPCreateFtIndex.cpp)
endif (ENABLE_FTSEARCH)

if (ENABLE_TRIGGERS)
    list(APPEND root_SRC PPCreateTrigger.cpp PPDropTrigger.cpp)
endif (ENABLE_TRIGGERS)
