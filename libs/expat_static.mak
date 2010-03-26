# Microsoft Developer Studio Generated NMAKE File, Based on expat_static.dsp
!IF "$(CFG)" == ""
CFG=expat_static - Win32 Release
!MESSAGE No configuration specified. Defaulting to expat_static - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "expat_static - Win32 Release" && "$(CFG)" != "expat_static - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "expat_static.mak" CFG="expat_static - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "expat_static - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "expat_static - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "expat_static - Win32 Release"

OUTDIR=..\lib
INTDIR=..\lib
# Begin Custom Macros
OutDir=..\lib
# End Custom Macros

ALL : "$(OUTDIR)\expat.lib"


CLEAN :
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\xmlparse.obj"
	-@erase "$(INTDIR)\xmlrole.obj"
	-@erase "$(INTDIR)\xmltok.obj"
	-@erase "$(OUTDIR)\expat.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /D "WIN32" /D "_WINDOWS" /D "NDEBUG" /D "_MBCS" /D "_LIB" /D "COMPILED_FROM_DSP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\expat_static.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\expat.lib"
LIB32_OBJS= \
	"$(INTDIR)\xmlparse.obj" \
	"$(INTDIR)\xmlrole.obj" \
	"$(INTDIR)\xmltok.obj"

"$(OUTDIR)\expat.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "expat_static - Win32 Debug"

OUTDIR=.\Debug_static
INTDIR=.\Debug_static
# Begin Custom Macros
OutDir=.\Debug_static
# End Custom Macros

ALL : "$(OUTDIR)\expat.lib" "$(OUTDIR)\expat_static.bsc"


CLEAN :
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\xmlparse.obj"
	-@erase "$(INTDIR)\xmlparse.sbr"
	-@erase "$(INTDIR)\xmlrole.obj"
	-@erase "$(INTDIR)\xmlrole.sbr"
	-@erase "$(INTDIR)\xmltok.obj"
	-@erase "$(INTDIR)\xmltok.sbr"
	-@erase "$(OUTDIR)\expat_static.bsc"
	-@erase "$(OUTDIR)\expat.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "COMPILED_FROM_DSP" /D "_MBCS" /D "_LIB" /FR"$(INTDIR)\\" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\expat_static.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\xmlparse.sbr" \
	"$(INTDIR)\xmlrole.sbr" \
	"$(INTDIR)\xmltok.sbr"

"$(OUTDIR)\expat_static.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\expat.lib"
LIB32_OBJS= \
	"$(INTDIR)\xmlparse.obj" \
	"$(INTDIR)\xmlrole.obj" \
	"$(INTDIR)\xmltok.obj"

"$(OUTDIR)\expat.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("expat_static.dep")
!INCLUDE "expat_static.dep"
!ELSE 
!MESSAGE Warning: cannot find "expat_static.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "expat_static - Win32 Release" || "$(CFG)" == "expat_static - Win32 Debug"
SOURCE=.\xmlparse.c

!IF  "$(CFG)" == "expat_static - Win32 Release"


"$(INTDIR)\xmlparse.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "expat_static - Win32 Debug"


"$(INTDIR)\xmlparse.obj"	"$(INTDIR)\xmlparse.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\xmlrole.c

!IF  "$(CFG)" == "expat_static - Win32 Release"


"$(INTDIR)\xmlrole.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "expat_static - Win32 Debug"


"$(INTDIR)\xmlrole.obj"	"$(INTDIR)\xmlrole.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\xmltok.c

!IF  "$(CFG)" == "expat_static - Win32 Release"


"$(INTDIR)\xmltok.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "expat_static - Win32 Debug"


"$(INTDIR)\xmltok.obj"	"$(INTDIR)\xmltok.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 

