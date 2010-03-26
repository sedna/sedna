set(ft_SRC ft_index_data.cpp ft_index.cpp ft_cache.cpp)

if (ENABLE_DTSEARCH)
    list(APPEND ft_SRC FTsearch.cpp FTindex.cpp)
endif (ENABLE_DTSEARCH)
