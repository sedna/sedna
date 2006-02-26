### makefile.vc - Makefile for building CHICKEN with VC++ 5/6 - felix

CC = cl
CFLAGS = /nologo /O2 /MT /DC_DEFAULT_TARGET_STACK_SIZE=300000 /DC_NO_PIC_NO_DLL /DHAVE_LOADLIBRARY /DHAVE_GETPROCADDRESS /DHAVE_WINDOWS_H
LFLAGS = /nologo /subsystem:console /incremental:no /machine:I386
CHICKEN = chicken
SFLAGS = -quiet -explicit-use -debug-level 0 -optimize-level 2
SXFLAGS = -quiet -no-warnings -debug-level 0 -optimize-level 2
#SFLAGS = -quiet -explicit-use -optimize-level 2
SUFLAGS = -quiet -explicit-use -debug-level 0 -optimize-level 2 -feature unsafe -unsafe
WINLIBS = ws2_32.lib
WINGUILIBS = $(WINLIBS) user32.lib

.SUFFIXES : .scm

# Targets:

all : libchicken.lib chicken.dll libchicken-static.lib \
	libchicken-gui.lib chicken-gui.dll libchicken-gui-static.lib \
	libstuffed-chicken.lib stuffed-chicken.dll libstuffed-chicken-static.lib \
	libstuffed-chicken-gui.lib stuffed-chicken-gui.dll \
	libuchicken.lib uchicken.dll libuchicken-static.lib \
	libustuffed-chicken.lib ustuffed-chicken.dll libustuffed-chicken-static.lib \
	libsrfi-chicken.lib srfi-chicken.dll libsrfi-chicken-static.lib \
	libsrfi-chicken-gui.lib srfi-chicken-gui.dll \
	libusrfi-chicken.lib usrfi-chicken.dll libusrfi-chicken-static.lib \
	chicken.exe chicken-static.exe \
	csi.exe csi-static.exe rcsi.exe rcsi-static.exe csc.exe chicken-profile.exe

clean :
	del *.obj *.obs library.c eval.c lolevel.c tinyclos.c srfi-14.c srfi-1.c srfi-13.c scheduler.c srfi-18.c srfi-25.c extras.c \
	  script-utils.c profiler.c match-support.c syntax-case.c pregexp.c format.c srfi-4.c srfi-37.c \
	  ulibrary.c ueval.c ulolevel.c utinyclos.c usrfi-14.c usrfi-1.c usrfi-13.c usrfi-18.c usrfi-25.c uextras.c \
	  uscript-utils.c umatch-support.c upregexp.c uformat.c usrfi-4.c usrfi-37.c tcp.c utcp.c \
	  chicken.c compiler.c support.c easyffi.c c-backend.c c-platform.c optimizer.c \
	  *.lib *.exe *.exp *.dll


# Compiling from sources:

.scm.c : 
	$(CHICKEN) $< -output-file $@ $(SFLAGS)

ulibrary.c : library.scm
	$(CHICKEN) library.scm -output-file $@ $(SUFLAGS)
ueval.c : eval.scm
	$(CHICKEN) eval.scm -output-file $@ $(SUFLAGS)
upregexp.c : pregexp.scm
	$(CHICKEN) pregexp.scm -output-file $@ $(SUFLAGS)
ulolevel.c : lolevel.scm
	$(CHICKEN) lolevel.scm -output-file $@ $(SUFLAGS)
utinyclos.c : tinyclos.scm
	$(CHICKEN) tinyclos.scm -output-file $@ $(SUFLAGS)
usrfi-1.c : srfi-1.scm
	$(CHICKEN) srfi-1.scm -output-file $@ $(SUFLAGS)
usrfi-4.c : srfi-4.scm
	$(CHICKEN) srfi-4.scm -output-file $@ $(SUFLAGS)
usrfi-13.c : srfi-13.scm
	$(CHICKEN) srfi-13.scm -output-file $@ $(SUFLAGS)
usrfi-14.c : srfi-14.scm
	$(CHICKEN) srfi-14.scm -output-file $@ $(SUFLAGS)
usrfi-25.c : srfi-25.scm
	$(CHICKEN) srfi-25.scm -output-file $@ $(SUFLAGS)
usrfi-18.c : srfi-18.scm
	$(CHICKEN) srfi-18.scm -output-file $@ $(SUFLAGS)
usrfi-37.c : srfi-37.scm
	$(CHICKEN) srfi-37.scm -output-file $@ $(SUFLAGS)
umatch-support.c : match-support.scm
	$(CHICKEN) match-support.scm -output-file $@ $(SUFLAGS)
uextras.c : extras.scm
	$(CHICKEN) extras.scm -output-file $@ $(SUFLAGS)
uformat.c : format.scm
	$(CHICKEN) format.scm -output-file $@ $(SUFLAGS)
uscript-utils.c : script-utils.scm
	$(CHICKEN) script-utils.scm -output-file $@ $(SUFLAGS)
utcp.c : tcp.scm
	$(CHICKEN) tcp.scm -output-file $@ $(SUFLAGS)


# The runtime library:

libchicken.lib chicken.dll: runtime.obj library.obj eval.obj syntax-case.obj profiler.obj scheduler.obj
	link /nologo /out:chicken.dll /implib:libchicken.lib \
		/dll $(WINLIBS) $**

libchicken-static.lib : runtime.obs library.obs eval.obs syntax-case.obs profiler.obs scheduler.obs
	lib /nologo /out:$@ $(WINLIBS) $**

libstuffed-chicken.lib stuffed-chicken.dll: extras.obj format.obj match-support.obj lolevel.obj tinyclos.obj pregexp.obj script-utils.obj tcp.obj
	link /nologo /out:stuffed-chicken.dll /implib:libstuffed-chicken.lib \
		/dll $(WINLIBS) $** libchicken.lib

libstuffed-chicken-gui.lib stuffed-chicken-gui.dll: extras.obj format.obj match-support.obj lolevel.obj tinyclos.obj pregexp.obj script-utils.obj tcp.obj
	link /nologo /out:stuffed-chicken-gui.dll /implib:libstuffed-chicken-gui.lib \
		/dll $(WINLIBS) $** libchicken-gui.lib

libstuffed-chicken-static.lib: extras.obs format.obs match-support.obs lolevel.obs tinyclos.obs pregexp.obs script-utils.obs tcp.obs
	lib /nologo /out:$@ $(WINLIBS) $**

libsrfi-chicken.lib srfi-chicken.dll: srfi-1.obj srfi-4.obj srfi-13.obj srfi-14.obj srfi-18.obj srfi-25.obj srfi-37.obj libchicken.lib
	link /nologo /out:srfi-chicken.dll /implib:libsrfi-chicken.lib \
		/dll $**

libsrfi-chicken-gui.lib srfi-chicken-gui.dll: srfi-1.obj srfi-4.obj srfi-13.obj srfi-14.obj srfi-18.obj srfi-25.obj srfi-37.obj libchicken-gui.lib
	link /nologo /out:srfi-chicken-gui.dll /implib:libsrfi-chicken-gui.lib \
		/dll $**

libsrfi-chicken-static.lib: srfi-1.obs srfi-4.obs srfi-13.obs srfi-14.obs srfi-18.obs srfi-25.obs srfi-37.obs
	lib /nologo /out:$@ $**

libuchicken.lib uchicken.dll: uruntime.obj ulibrary.obj ueval.obj syntax-case.obj profiler.obj scheduler.obj
	link /nologo /out:uchicken.dll /implib:libuchicken.lib \
		/dll $(WINLIBS) $**

libuchicken-static.lib : uruntime.obs ulibrary.obs ueval.obs syntax-case.obs profiler.obs scheduler.obs
	lib /nologo /out:$@ $(WINLIBS) $**

libustuffed-chicken.lib ustuffed-chicken.dll: uextras.obj uformat.obj umatch-support.obj ulolevel.obj utinyclos.obj upregexp.obj uscript-utils.obj utcp.obj libuchicken.lib
	link /nologo /out:ustuffed-chicken.dll /implib:libustuffed-chicken.lib \
		/dll $(WINLIBS) $**

libustuffed-chicken-static.lib : uextras.obs uformat.obs umatch-support.obs ulolevel.obs utinyclos.obs upregexp.obs uscript-utils.obs utcp.obs
	lib /nologo /out:$@ $(WINLIBS) $**

libusrfi-chicken.lib usrfi-chicken.dll: usrfi-1.obj usrfi-4.obj usrfi-13.obj usrfi-14.obj usrfi-18.obj usrfi-25.obj usrfi-37.obj libuchicken.lib
	link /nologo /out:usrfi-chicken.dll /implib:libusrfi-chicken.lib \
		/dll $**

libusrfi-chicken-static.lib : usrfi-1.obs usrfi-4.obs usrfi-13.obs usrfi-14.obs usrfi-18.obs usrfi-25.obs usrfi-37.obs
	lib /nologo /out:$@ $**

libchicken-gui.lib chicken-gui.dll: gui-runtime.obj gui-library.obj eval.obj syntax-case.obj profiler.obj scheduler.obj libchicken.lib
	link /nologo /out:chicken-gui.dll /implib:libchicken-gui.lib \
		/dll $(WINGUILIBS) $**

libchicken-gui-static.lib : gui-runtime.obs gui-library.obs eval.obs syntax-case.obs profiler.obs scheduler.obs
	lib /nologo /out:$@ $(WINGUILIBS) $**

.c.obj:
	$(CC) $(CFLAGS) /c $< /DPIC

.c.obs:
	$(CC) $(CFLAGS) /c $< /Fo$@

runtime.obj : runtime.c chicken.h
	$(CC) $(CFLAGS) /c runtime.c /DC_BUILDING_LIBCHICKEN /DPIC

runtime.obs : runtime.c chicken.h
	$(CC) $(CFLAGS) /c runtime.c /DC_BUILDING_LIBCHICKEN /Fo$@

uruntime.obj : runtime.c chicken.h
	$(CC) $(CFLAGS) /c runtime.c /DC_BUILDING_LIBCHICKEN /DC_UNSAFE_RUNTIME /DPIC /Fo$@

uruntime.obs : runtime.c chicken.h
	$(CC) $(CFLAGS) /c runtime.c /DC_BUILDING_LIBCHICKEN /DC_UNSAFE_RUNTIME /Fo$@

gui-runtime.obj : runtime.c
	$(CC) $(CFLAGS) /c /DC_WINDOWS_GUI runtime.c /Fo$@ /DC_BUILDING_LIBCHICKEN /DPIC

gui-runtime.obs : runtime.c
	$(CC) $(CFLAGS) /c /DC_WINDOWS_GUI runtime.c /Fo$@ /DC_BUILDING_LIBCHICKEN /Fo$@

eval.obj : eval.c chicken.h
	$(CC) $(CFLAGS) /c eval.c /DC_BUILDING_LIBCHICKEN /DPIC
eval.obs : eval.c chicken.h
	$(CC) $(CFLAGS) /c eval.c /DC_BUILDING_LIBCHICKEN /Fo$@
library.obj : library.c chicken.h
	$(CC) $(CFLAGS) /c library.c /DC_BUILDING_LIBCHICKEN /DPIC
library.obs : library.c chicken.h
	$(CC) $(CFLAGS) /c library.c /DC_BUILDING_LIBCHICKEN /Fo$@
profiler.obj : profiler.c chicken.h
	$(CC) $(CFLAGS) /c profiler.c /DC_BUILDING_LIBCHICKEN /DPIC
profiler.obs : profiler.c chicken.h
	$(CC) $(CFLAGS) /c profiler.c /DC_BUILDING_LIBCHICKEN /Fo$@
scheduler.obj : scheduler.c chicken.h
	$(CC) $(CFLAGS) /c scheduler.c /DC_BUILDING_LIBCHICKEN /DPIC
scheduler.obs : scheduler.c chicken.h
	$(CC) $(CFLAGS) /c scheduler.c /DC_BUILDING_LIBCHICKEN /Fo$@
syntax-case.obj : syntax-case.c chicken.h
	$(CC) $(CFLAGS) /c syntax-case.c /DC_BUILDING_LIBCHICKEN /DPIC
syntax-case.obs : syntax-case.c chicken.h
	$(CC) $(CFLAGS) /c syntax-case.c /DC_BUILDING_LIBCHICKEN /Fo$@

extras.obj : extras.c chicken.h
extras.obs : extras.c chicken.h
srfi-1.obj : srfi-1.c chicken.h
srfi-1.obs : srfi-1.c chicken.h
srfi-4.obj : srfi-4.c chicken.h
srfi-4.obs : srfi-4.c chicken.h
match-support.obj : match-support.c chicken.h
match-support.obs : match-support.c chicken.h
format.obj : format.c chicken.h
format.obs : format.c chicken.h
tinyclos.obj : tinyclos.c chicken.h
tinyclos.obs : tinyclos.c chicken.h
srfi-13.obj : srfi-13.c chicken.h
srfi-13.obs : srfi-13.c chicken.h
srfi-14.obj : srfi-14.c chicken.h
srfi-14.obs : srfi-14.c chicken.h
srfi-25.obj : srfi-25.c chicken.h
srfi-25.obs : srfi-25.c chicken.h
srfi-18.obj : srfi-18.c chicken.h
srfi-18.obs : srfi-18.c chicken.h
srfi-37.obj : srfi-37.c chicken.h
srfi-37.obs : srfi-37.c chicken.h
lolevel.obj : lolevel.c chicken.h
lolevel.obs : lolevel.c chicken.h
pregexp.obj : pregexp.c chicken.h
pregexp.obs : pregexp.c chicken.h
script-utils.obj : script-utils.c chicken.h
script-utils.obs : script-utils.c chicken.h
tcp.obj : tcp.c chicken.h
tcp.obs : tcp.c chicken.h

ueval.obj : ueval.c chicken.h
	$(CC) $(CFLAGS) /c ueval.c /DC_BUILDING_LIBCHICKEN /DPIC
ueval.obs : ueval.c chicken.h
	$(CC) $(CFLAGS) /c ueval.c /DC_BUILDING_LIBCHICKEN /Fo$@
ulibrary.obj : ulibrary.c chicken.h
	$(CC) $(CFLAGS) /c ulibrary.c /DC_BUILDING_LIBCHICKEN /DPIC
ulibrary.obs : ulibrary.c chicken.h
	$(CC) $(CFLAGS) /c ulibrary.c /DC_BUILDING_LIBCHICKEN /Fo$@
uextras.obj : uextras.c chicken.h
uextras.obs : uextras.c chicken.h
usrfi-1.obj : usrfi-1.c chicken.h
usrfi-1.obs : usrfi-1.c chicken.h
usrfi-4.obj : usrfi-4.c chicken.h
usrfi-4.obs : usrfi-4.c chicken.h
umatch-support.obj : umatch-support.c chicken.h
umatch-support.obs : umatch-support.c chicken.h
uformat.obj : uformat.c chicken.h
uformat.obs : uformat.c chicken.h
utinyclos.obj : utinyclos.c chicken.h
utinyclos.obs : utinyclos.c chicken.h
usrfi-13.obj : usrfi-13.c chicken.h
usrfi-13.obs : usrfi-13.c chicken.h
usrfi-14.obj : usrfi-14.c chicken.h
usrfi-14.obs : usrfi-14.c chicken.h
usrfi-25.obj : usrfi-25.c chicken.h
usrfi-25.obs : usrfi-25.c chicken.h
usrfi-18.obj : usrfi-18.c chicken.h
usrfi-18.obs : usrfi-18.c chicken.h
usrfi-37.obj : usrfi-37.c chicken.h
usrfi-37.obs : usrfi-37.c chicken.h
ulolevel.obj : ulolevel.c chicken.h
ulolevel.obs : ulolevel.c chicken.h
upregexp.obj : upregexp.c chicken.h
upregexp.obs : upregexp.c chicken.h
uscript-utils.obj : uscript-utils.c chicken.h
uscript-utils.obs : uscript-utils.c chicken.h
utcp.obj : utcp.c chicken.h
utcp.obs : utcp.c chicken.h

gui-library.obj : library.c chicken.h
	$(CC) $(CFLAGS) /c /DC_WINDOWS_GUI library.c /Fo$@ /DC_BUILDING_LIBCHICKEN /DPIC
gui-library.obs : library.c chicken.h
	$(CC) $(CFLAGS) /c /DC_WINDOWS_GUI library.c /Fo$@ /DC_BUILDING_LIBCHICKEN /Fo$@

chicken.res : chicken.rc
	rc /r chicken.rc

# The interpreter:

csi.c: csi.scm build.scm chicken-static.exe
	.\chicken-static.exe csi.scm -optimize-level 2 -debug-level 0 -quiet -output-file csi.c -prologue build.scm -uses tcp -feature fullcsi
csi.obj: csi.c chicken.h
csi.obs: csi.c chicken.h

csi.exe        : csi.obj libchicken.lib libsrfi-chicken.lib libstuffed-chicken.lib chicken.res
	link $(LFLAGS) $** /out:$@

csi-static.exe : csi.obs libchicken-static.lib libsrfi-chicken-static.lib libstuffed-chicken-static.lib chicken.res
	link $(LFLAGS) $** /out:$@

rcsi.c: csi.scm build.scm chicken-static.exe
	.\chicken-static.exe csi.scm -optimize-level 2 -debug-level 0 -quiet -output-file rcsi.c -prologue build.scm
rcsi.obj: rcsi.c chicken.h
rcsi.obs: rcsi.c chicken.h

rcsi.exe        : rcsi.obj libchicken.lib chicken.res
	link $(LFLAGS) $** /out:$@

rcsi-static.exe : rcsi.obs libchicken-static.lib chicken.res
	link $(LFLAGS) $** /out:$@

# csc:

csc.c: csc.scm chicken-static.exe
	.\chicken-static.exe csc.scm -optimize-level 2 -debug-level 0 -quiet -output-file csc.c
csc.obj: csc.c chicken.h

csc.exe        : csc.obj libchicken.lib libsrfi-chicken.lib libstuffed-chicken.lib chicken.res
	link $(LFLAGS) $** /out:$@

# chicken-profile

chicken-profile.c: chicken-profile.scm chicken-static.exe
	.\chicken-static.exe chicken-profile.scm -optimize-level 2 -debug-level 0 -quiet -output-file chicken-profile.c
chicken-profile.obj: chicken-profile.c chicken.h

chicken-profile.exe: chicken-profile.obj libchicken.lib libsrfi-chicken.lib libstuffed-chicken.lib chicken.res
	link $(LFLAGS) $** /out:$@


# The compiler:

chicken.exe : chicken.obj support.obj easyffi.obj compiler.obj optimizer.obj batch-driver.obj c-platform.obj c-backend.obj chicken.res \
	  libchicken.lib libsrfi-chicken.lib libstuffed-chicken.lib
	link $(LFLAGS) $** /out:$@

chicken-static.exe : chicken.obs support.obs easyffi.obs compiler.obs optimizer.obs batch-driver.obs c-platform.obs c-backend.obs chicken.res \
	  libchicken-static.lib libsrfi-chicken-static.lib libstuffed-chicken-static.lib
	link $(LFLAGS) $** /out:$@

chicken.c: chicken.scm tweaks.scm
	$(CHICKEN) chicken.scm $(SXFLAGS) -output-file chicken.c

chicken.obj : chicken.c chicken.h
chicken.obs : chicken.c chicken.h
support.obj : support.c chicken.h
support.obs : support.c chicken.h
easyffi.obj : easyffi.c chicken.h
easyffi.obs : easyffi.c chicken.h
c-platform.obj : c-platform.c chicken.h
c-platform.obs : c-platform.c chicken.h
c-backend.obj : c-backend.c chicken.h
c-backend.obs : c-backend.c chicken.h
batch-driver.obj : batch-driver.c chicken.h
batch-driver.obs : batch-driver.c chicken.h
compiler.obj : compiler.c chicken.h
compiler.obs : compiler.c chicken.h
optimizer.obj : optimizer.c chicken.h
optimizer.obs : optimizer.c chicken.h

easyffi.l.silex : easyffi.l
	csi -script silex.scm easyffi.l counters none
	move easyffi.l.scm easyffi.l.silex

psyntax-chicken.pp: psyntax.scm psyntax.pp psyntax-bootstrap.scm
	csi -script psyntax-bootstrap.scm

syntax-case.c: psyntax-chicken.pp
