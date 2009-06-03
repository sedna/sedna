#
# Makefile for block string trie
#

PP = ../../..

include $(PP)/Makefile.include

OBJS = st_frontend$(OBJ_EXT) st_pages$(OBJ_EXT) st_trie$(OBJ_EXT)

include $(PP)/Makefile.ordlib

TESTUNIFY=_debug_test_unification.c

#test: build build_test
#        ./unitest

#build_test: unitest
#
#unitest: bstrie.a _test_st.c $(TESTUNIFY)
#        $(COMPILER) $(CFLAGS) _test_st.c st.a $(TESTUNIFY) -o unitest
