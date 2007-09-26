#define __WUDANG_SOURCES__

#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>
#include "wuaux.h"

static 
uint32_t BigEndianByteOrder(uint32_t arg)
{
	return (arg<<24) & UINT32_C(0xFF000000) | (arg<<8) & UINT32_C(0x00FF0000) | 
		(arg>>8) & UINT32_C(0x0000FF00) | (arg>>24) & UINT32_C(0x000000FF);
}

struct GuardMemoryVars
{
	uint32_t fill, m0, m1;
	uint32_t *begin, *end, *frag0, *frag1;
	void *vbegin, *vend;
	uint32_t dummy;
};

static 
void InitGuardMemoryVars(GuardMemoryVars *vars, void *ptr, ptrdiff_t dist, uint32_t fill)
{
	void *eptr=ptr;

	assert(vars && (ptr||!dist));	

	if (dist>0) eptr=OffsetPtr(ptr,dist);
	else ptr=OffsetPtr(eptr,dist);
	assert(ptr<=eptr); /* wraparound */ 

	vars->begin=(uint32_t*)AlignPtr(OffsetPtr(ptr,+4),4);
	vars->end=(uint32_t*)AlignPtr(OffsetPtr(eptr,-4),4);
	vars->vbegin=ptr;
	vars->vend=eptr;

	vars->dummy=vars->fill=BigEndianByteOrder(fill);
	vars->m0=BigEndianByteOrder(~UINT32_C(0)>>CalcPtrDistance(vars->begin,ptr)*8);
	vars->m1=BigEndianByteOrder(~UINT32_C(0)<<CalcPtrDistance(eptr,vars->end)*8);

	if (dist==0)
	{
		vars->m0=vars->m1=0;
		vars->begin=vars->end=NULL;
		vars->frag0=vars->frag1=&vars->dummy;
	}
	else if(vars->begin>vars->end)
	{
		vars->m0&=vars->m1;
		vars->m1=0;
		vars->frag0=vars->begin-1;
		vars->frag1=&vars->dummy;
		vars->begin=vars->end=NULL;
	}
	else
	{
		vars->frag0=vars->begin-1;
		vars->frag1=vars->end;
	}
}

void DbgInitGuardMemory(void *ptr, ptrdiff_t dist, uint32_t fill)
{
	uint32_t *i=0;
	GuardMemoryVars gmv;
	InitGuardMemoryVars(&gmv,ptr,dist,fill);
	*gmv.frag0=*gmv.frag0 & ~gmv.m0 | gmv.fill & gmv.m0;
	for(i=gmv.begin; i<gmv.end; ++i) *i=gmv.fill;
	*gmv.frag1=*gmv.frag1 & ~gmv.m1 | gmv.fill & gmv.m1;
}

static 
void ReportGuardMemOverrun(void *begin, void *end, uint32_t *loc, uint32_t content, uint32_t expect, 
						   const char *errmsg,
						   int *isHeaderOutput)
{
	if (*isHeaderOutput==0)
	{
		if (errmsg) fprintf(stderr,"%s\n",errmsg);
		else fprintf(stderr,"canary %08p-%08p\n", begin, end);
		*isHeaderOutput=1;
	}
	fprintf(stderr,"   %04x %08X %08X\n", 
		CalcPtrDistance(begin,loc), BigEndianByteOrder(content), BigEndianByteOrder(expect));
}

int DbgCheckGuardMemory(void *ptr, ptrdiff_t dist, uint32_t fill, int isReporting, const char *errmsg)
{
	uint32_t *i=0, expect=0, content=0;
	int isDamaged=0; int isHeaderOutput=0;
	GuardMemoryVars gmv;
	InitGuardMemoryVars(&gmv,ptr,dist,fill);

	expect=*gmv.frag0 & ~gmv.m0 | gmv.fill & gmv.m0;
	content=*gmv.frag0;
	if (expect!=content) 
	{
		++isDamaged; 
		if (isReporting) 
			ReportGuardMemOverrun(gmv.vbegin,gmv.vend,gmv.frag0,content,expect,errmsg,&isHeaderOutput);
	}

	for(i=gmv.begin; i<gmv.end; ++i)
	{
		expect=gmv.fill;
		content=*i;
		if (expect!=content)
		{
			++isDamaged; 
			if (isReporting) 
				ReportGuardMemOverrun(gmv.vbegin,gmv.vend,i,content,expect,errmsg,&isHeaderOutput);
		}
	}

	expect=*gmv.frag1 & ~gmv.m1 | gmv.fill & gmv.m1;
	content=*gmv.frag1;
	if (expect!=content)
	{
		++isDamaged;
		if (isReporting)
			ReportGuardMemOverrun(gmv.vbegin,gmv.vend,gmv.frag1,content,expect,errmsg,&isHeaderOutput);
	}

	if (isHeaderOutput) fputs("\n",stderr);
	return isDamaged==0;
}

#define HEXPATLINE(I) I "0" I "1" I "2" I "3" I "4" I "5" I "6" I "7" I "8" I "9" I "A" I "B" I "C" I "D" I "E" I "F"

static const char hexpat[513]=
HEXPATLINE("0") HEXPATLINE("1") HEXPATLINE("2") HEXPATLINE("3")
HEXPATLINE("4") HEXPATLINE("5") HEXPATLINE("6") HEXPATLINE("7")
HEXPATLINE("8") HEXPATLINE("9") HEXPATLINE("A") HEXPATLINE("B")
HEXPATLINE("C") HEXPATLINE("D") HEXPATLINE("E") HEXPATLINE("F");

#undef HEXPATLINE

void DbgDumpMemory(DbgDumpMemoryParams *dumpMemoryParams)
{
	DbgDumpMemoryParams params;
	void *defaultSections[1] = {NULL};
	size_t defaultSectionsSize[1] = {0};
	char dumpBuf[1][1024]={""};
	char markBuf[1][128]={""};
	uint32_t valid=0;
	char *p=NULL, *e=NULL, *o=NULL;
	int i=0, n=0, j=0, k=0, w=0;
	DbgDumpMemoryMarks *m=NULL;
	
	assert(dumpMemoryParams);
	params=*dumpMemoryParams;
	assert((params.base||!params.size) && params.stride>=0 && params.sectionsCount>=0);

	if (params.size>0)
	{
		/* normalise params */ 
		if (params.stride==0) params.stride=params.size;
		if (params.sectionsCount==0 || params.sections==NULL || params.sectionsSize==NULL)
		{
			params.sectionsCount=1;
			params.sectionsBase=NULL;
			params.sections=defaultSections;
			defaultSectionsSize[0]=params.stride;
			params.sectionsSize=defaultSectionsSize;			
		}
		/* number of rows */ 
		n=params.size/params.stride;
		/* estimate width to reserve for marks */ 
		for (m=params.marks;m;m=m->next)
		{
			assert(m->markBits);
			++w;
		}
		/* if have marks increase reserved width by 1 thus creating spacing before marks column */ 
		if (w>0) ++w;
		/* for each row */ 
		for(i=0;i<n;i+=32)
		{
			valid=~(params.skipBits?params.skipBits[i/32]:0);
			while(valid && (j=ResetLowestBitSet(&valid))+i<n)
			{
				/* fill mark buf with current row marks */ 
				o=markBuf[0];
				for (m=params.marks;m && o+1<markBuf[1];m=m->next)
				{
					if ((m->markBits[i/32]^m->xorMask)&(UINT32_C(1)<<j)) *o++=m->mark;
				}
				*o=0;
				/* fill dump buf with current row sections */ 
				o=dumpBuf[0];
				for(k=0; k<params.sectionsCount; ++k)
				{
					p=(char*)OffsetPtr(params.base,(i+j)*params.stride+CalcPtrDistance(params.sectionsBase,params.sections[k]));
					e=p+params.sectionsSize[k];
					/*	put sections separator, to skip redundant separator before
						1st section we later pass dumpBuf[0]+3 to printf */ 
					memcpy(o," | ",3); 
					o+=(AlignPtr(p,4)==p?2:3);
					for(;p<e && o+3<dumpBuf[1];++p,o+=2)
					{
						/*	put space after each group of 4 bytes */ 
						if (AlignPtr(p,4)==p) *o++=' ';
						memcpy(o,hexpat+2*(unsigned char)*p,2);
					}
					if (p<e) break; /* no space left in buffer */ 
				}
				*o=0;
				fprintf(stderr,"%4d%*s   %s\n",i+j,w,markBuf[0],dumpBuf[0]+3);
			}
		}
		fputs("\n",stderr);
	}
}
