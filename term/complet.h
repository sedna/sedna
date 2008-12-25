
#ifndef __TERM_COMPLET_H__
#define __TERM_COMPLET_H__
// function is used to generate list of possible matches
// it is called from libedit
char **term_complet(const char *text, int start, int end);

#endif /* __TERM_COMPLET_H__ */