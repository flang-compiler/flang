/*
 * Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#ifndef	FORT_VARS_H
#define	FORT_VARS_H
#include <stdint.h>

typedef	struct	__fort_vars {
	int32_t	debug;		/*   0: 1 if debugging                        */
	int32_t	zmem;		/*   4: zero all memory allocations           */
	int32_t	debugn;		/*   8: which process or -1 for all           */
	int32_t	ioproc;		/*  12: id of i/o proccess                    */
	int32_t	lcpu;		/*  16:                                       */
	int32_t	np2;		/*  20: smallest power of 2 >= __fort_tcpus   */
	int32_t	pario;		/*  24:                                       */
	int32_t	quiet;		/*  28: runtime statistics                    */
	int32_t	tcpus;		/*  32: total number of processors            */
	int32_t	test;		/*  36:                                       */
	int64_t	heapz;		/*  40: global heap size                      */
	int64_t	heap_block;	/*  48: prcoessor heap size (not power of 2)  */
	int32_t	*tids;		/*  56: tid for each processor                */
	int32_t	dummy1[16];	/*  64:                                       */
	char	*red_what;	/* 128: !! NOT THREAD-SAFE !!                 */
	int32_t	dummy2[30];	/* 136:                                       */
} __fort_vars_t;

extern	__fort_vars_t	__fort_vars;

#define	__fort_debug		(__fort_vars.debug)
#define	__fort_zmem		(__fort_vars.zmem)
#define	__fort_debugn		(__fort_vars.debugn)
#define	__fort_ioproc		(__fort_vars.ioproc)
#define	__fort_lcpu		(__fort_vars.lcpu)
#define	__fort_np2		(__fort_vars.np2)
#define	__fort_pario		(__fort_vars.pario)
#define	__fort_quiet		(__fort_vars.quiet)
#define	__fort_tcpus		(__fort_vars.tcpus)
#define	__fort_test		(__fort_vars.test)
#define	__fort_heapz		(__fort_vars.heapz)
#define	__fort_heap_block	(__fort_vars.heap_block)
#define	__fort_tids		(__fort_vars.tids)
#define	__fort_red_what		(__fort_vars.red_what)

#endif	/* FORT_VARS_H */
