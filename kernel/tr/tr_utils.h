/*
 * File:  tr_utils.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TR_UTILS_H_
#define _TR_UTILS_H_

#include "common/sedna.h"
#include "auxiliary/utils.h"


//#define TIME_DEBUG_INFO_ON

#define DECLARE_TIME_VARS													\
	                      u_timeb t1_net, t2_net, t_total_net;				\
                          u_timeb t1_bqep, t2_bqep, t_total_bqep;			\
                          u_timeb t1_open, t2_open, t_total_open;			\
                          u_timeb t1_exec, t2_exec, t_total_exec;			\
                          u_timeb t1_parser, t2_parser,  t_total_parser;	\
                          u_timeb t1_scm, t2_scm, t_total_scm;



#define EXTERN_DECLARE_TIME_VARS					\
 extern u_timeb t1_net, t2_net, t_total_net;		\
 extern u_timeb t1_bqep, t2_bqep, t_total_bqep;		\
 extern u_timeb t1_open, t2_open, t_total_open;		\
 extern u_timeb t1_exec, t2_exec, t_total_exec;		\
 extern u_timeb t1_parser, t2_parser,  t_total_parser; \
 extern u_timeb t1_scm, t2_scm, t_total_scm;


#ifdef TIME_DEBUG_INFO_ON


#define INIT_TOTAL_TIME_VARS									\
	                              u_ftime(&t_total_net);		\
                                  u_ftime(&t_total_parser); 	\
                                  u_ftime(&t_total_scm);		\
                                  u_ftime(&t_total_bqep);		\
                                  u_ftime(&t_total_open);		\
                                  u_ftime(&t_total_exec);		\
                                  t_total_net.time =0;          \
                                  t_total_net.millitm =0;       \
                             	  t_total_parser.time =0;		\
                                  t_total_parser.millitm =0;	\
                                  t_total_scm.time =0;			\
                                  t_total_scm.millitm =0;		\
                                  t_total_bqep.time =0;			\
                                  t_total_bqep.millitm =0;		\
                                  t_total_open.time =0;			\
                                  t_total_open.millitm =0;		\
                                  t_total_exec.time =0;			\
                                  t_total_exec.millitm =0;


#define GET_TIME(t)	  u_ftime(t)


#define ADD_TIME(t1, t2, t3) t1 = t1 + (t3 - t2)



#define PRINT_DEBUG_TIME_RESULTS  																			\
	                             cerr << "time network: " << to_string(t_total_net).c_str() << endl;        \
                                 cerr << "time parser: " << to_string(t_total_parser).c_str() << endl;		\
                                 cerr << "time scheme: " << to_string(t_total_scm).c_str() << endl;			\
                                 cerr << "time QEP build: " << to_string(t_total_bqep).c_str() << endl;		\
                                 cerr << "time QEP open: " << to_string(t_total_open).c_str() << endl;		\
                                 cerr << "time pure exec: " << to_string(t_total_exec).c_str() << endl;

#else
#define INIT_TOTAL_TIME_VARS
#define GET_TIME(t)
#define ADD_TIME(t1, t2, t3)
#define PRINT_DEBUG_TIME_RESULTS
#endif

#endif /* _TR_UTILS_H_ */
