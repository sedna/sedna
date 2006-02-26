

CF=-I../../../../include -I/usr/include/sedna

OS := $(shell (uname | sed 's/^CYGWIN_NT.*$$/CYGWIN_NT/'))
ifeq ("$(OS)","CYGWIN_NT")

SO_EXT=.dll

sample$(SO_EXT): sample.cpp
	cl /LD $(CF) sample.cpp

else

SO_EXT=.so

sample$(SO_EXT): sample.cpp
	gcc -shared $(CF) sample.cpp -o sample$(SO_EXT)

endif


clean:
	-rm -rf *.o *.obj *.lib *.exp *$(SO_EXT)
