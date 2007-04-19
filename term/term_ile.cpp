#ifndef LIBEDIT_POWERED_SETERM

int ile_init() { return 0; }
void ile_deinit() { ; }
char * ile_gets(char * buf, size_t sz) { return 0; }

#else

#include <cstdio>
#include <cctype>
#include <cstring>
#include <histedit.h>
#include <cassert>
#include "term_globals.h"

extern "C" unsigned ed_newline(EditLine *, int);
static char * se_prompt(EditLine *);
static unsigned se_smart_newline(EditLine *, int);

static EditLine *el=0;
static History *hist=0;

void ile_deinit()
{
	if (hist) history_end(hist);
  	if (el) el_end(el);
	hist=0;
	el=0;
}

int ile_init()
{
	HistEvent ev;
	if (el || hist) return 0;

	el = el_init("se_term", stdin, stdout, stderr);
	if (!el) return 0;

	hist = history_init();
	if (!hist) 
	{
		el_end(el);
		el=0;
		return 0;
	}
	history(hist, &ev, H_SETSIZE, 800);

	el_set(el,EL_PROMPT,&se_prompt);
	el_set(el,EL_EDITOR,"emacs");	
	el_set(el,EL_HIST,history,hist);
	el_set(el,EL_SIGNAL,1);

	el_set(el,EL_ADDFN,"se-smart-newline","",se_smart_newline);
	el_set(el,EL_BIND,"\n","se-smart-newline",NULL);
	el_set(el,EL_BIND,"\t","ed-insert",NULL);
	el_set(el,EL_BIND,"\033[A","ed-prev-line",NULL);
	el_set(el,EL_BIND,"\033[B","ed-next-line",NULL);
	el_set(el,EL_BIND,"\033\033[A","ed-prev-history",NULL);
	el_set(el,EL_BIND,"\033\033[B","ed-next-history",NULL);

	return 1;
}

char * ile_gets(char * buf, size_t sz)
{
	const char * line;
	int count;
	HistEvent ev;
	char dummy[4];
	if (!el||!hist||!buf||!sz) return 0;

	while(1)
	{
		line = el_gets(el, &count);

		if (count > 0) 
		{
			if(1==sscanf(line,"%1s",dummy)) /* true if the line consists any non-WS character */ 
			{	
      				history(hist, &ev, H_ENTER, line);
				break;
			}
		}
	}
	if (count+1<(int)sz) sz=count+1;
	memcpy(buf,line,sz);
	buf[sz-1]='\0';
	if (sz==count+1&&*dummy!='\\')
	{
		if(buf[sz-2]=='&') buf[sz-2]='\n';
		if(buf[sz-2]=='\n' && sz>2 && buf[sz-3]=='&') strcpy(&buf[sz-3],"\n");
	}
	return buf;
}

char * se_prompt(EditLine * e)
{
	const LineInfo * l;
	l=el_line(e);
	assert(l);
	/* we have different prompts for single line input and multiple line input */ 
	return (memchr(l->buffer,'\n',l->lastchar - l->buffer)) ? micro_prompt : prompt;
}

unsigned se_smart_newline(EditLine * e, int ch)
{
	char buf[256];
	const char * b=NULL, * i=NULL;
	const LineInfo * l;
	l=el_line(e);
	assert(l);
	
	/* should we accept input? */ 
	for (i=l->buffer;i<l->lastchar&&isspace(*i);++i);
	if (i==l->lastchar || *i=='\\')
	{
		/*	user hits Enter and entire input contains nothing but whitespace 
			or it is a special command */ 
		return ed_newline(e,ch);
	}	
	if(l->cursor==l->lastchar&&l->cursor!=l->buffer&&l->cursor[-1]=='&')
	{
		/*	input is & terminated and cursor stands at the input end */ 
		return ed_newline(e,ch);
	}

	/* insert another \n character and perform auto indent */ 
	for(i=l->cursor;i!=l->buffer&&i[-1]!='\n';--i);
	for(b=i;b!=l->cursor&&isspace(*b);++b);
	el_insertstr(e,"\n");
	if(b!=i && b-i+1<(int)(sizeof buf))
	{
		memcpy(buf,i,b-i);
		buf[b-i]='\0';
		el_insertstr(e,buf);
	}
	return CC_REDISPLAY;
}

#endif
