// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__F04C790C_B35D_4748_8C3C_E6CD33C2DEF0__INCLUDED_)
#define AFX_STDAFX_H__F04C790C_B35D_4748_8C3C_E6CD33C2DEF0__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN	

#include <crtdbg.h>
#include "kbase.h"
namespace kb = kbase_2001;

#include <eh.h>

#pragma warning(push, 3) 
#include <iostream>
#include <sstream>
#include <string>
#include <exception>
#pragma warning(pop) 

#include <stdio.h>


// TODO: reference additional headers your program requires here

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__F04C790C_B35D_4748_8C3C_E6CD33C2DEF0__INCLUDED_)
