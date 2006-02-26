
PP=../..
include $(PP)/Makefile.include



ifeq ("$(PLATFORM)", "WIN32")

#use EHa?
CPPFLAGS = /nologo /O2 /Ob2 /EHsc /D 'WIN32' /D '_WIN32' /GX /GR /MT /D _CONSOLE			\
           /DPCRE_STATIC /DSUPPORT_UTF8 /DSUPPORT_UCP /DPOSIX_MALLOC_THRESHOLD=10

else

CPPFLAGS = -O3 -Wall -Wno-deprecated  \
           -DPCRE_STATIC -DSUPPORT_UTF8 -DSUPPORT_UCP -DPOSIX_MALLOC_THRESHOLD=10

endif



libpcre$(LIB_EXT): maketables$(OBJ_EXT) study$(OBJ_EXT) pcre$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

clean:
	rm -f *.obj *.exe *.lib *.a *.o


