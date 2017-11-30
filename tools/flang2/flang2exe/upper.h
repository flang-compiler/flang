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

/** \file
 * \brief Header file for upper - import the lowered F90/HPF code
 */
/*
 * Compatibility History:
 * before 6.2  -- 1.9
 * 6.2         -- 1.10
 *                Includes all of 1.9 + PASSBYVAL & PASSBYREF
 * 7.0         -- 1.11
 *                Includes all of 1.10 + CFUNC for variables
 * 7.1         -- 1.12
 *                Includes all of 1.11 + DECORATE
 * 7.2         -- 1.13
 *                Includes all of 1.12 + CREF & NOMIXEDSTRLEN
 * 8.0         -- 1.14
 *                Includes all of 1.13 + FILE INDEX ENTRIES
 * 8.1         -- 1.15
 *                Includes all of 1.14 + new types + cuda flags
 * 9.0-3       -- 1.16
 *                Includes all of 1.15 + cudaemu value
 * 10.6        -- 1.17
 *                Includes all of 1.16 + sptr for Constant ID data init + denorm
 * 10.9        -- 1.18
 *                Includes all of 1.17 + reflected/mirrored/devcopy flags and
 * devcopy field
 * 11.0        -- 1.19
 *                Includes all of 1.18 + mscall & cref for vars & members
 * 11.4        -- 1.20
 *                Includes all of 1.19 + libm & libc for functions
 * 12.7        -- 1.21
 *                Includes all of 1.20 + TASK for variables
 * 12.7        -- 1.22
 *                Includes all of 1.21 + cuda texture flag
 * 12.7        -- 1.23
 *                Includes all of 1.21 + INTENTIN flag
 * 13.0        -- 1.24
 *                Includes all of 1.23 + DATACNST flag
 * 13.5        -- 1.25
 *                Includes all of 1.24 + MODCMN flag
 * 13.8        -- 1.26
 *                Includes all of 1.25 + DOBEGNZ & DOENDNZ
 * 13.9        -- 1.27
 *                Includes all of 1.26 + symbol ACCCREATE and ACCRESIDENT
 * 14.0        -- 1.28
 *                Includes all of 1.27 + ACCROUT
 * 14.4        -- 1.29
 *                Includes all of 1.28 + CUDAMODULE
 * 14.4        -- 1.30
 *                Includes all of 1.29 + MANAGED + additionsl ILM operand
 *                    for the call ILMs via a procedure ptr, e.g., CALLA,
 *                    CDFUNCA, etc.
 * 14.7        -- 1.31
 *                All of 1.30 + ACCCREATE + ACCRESIDENT for common blocks,
 *                    +ACCLINK +ACCCOPYIN symbol flags
 * 15.0        -- 1.32
 *                All of 1.31 + new FARGF ILM
 * 15.3        -- 1.33
 *                All of 1.32 + FWDREF flag + INTERNREF flag + AGOTO field
 * 15.4        -- 1.34
 *                All of 1.33 + ST_MEMBER IFACE field
 * 15.7        -- 1.35
 *                All of 1.34 + ST_ENTRY/ST_PROC ARET field
 * 15.9        -- 1.36
 *                All of 1.35 + PARREF, PARSYMS, & PARSYMSCT
 * 15.10       -- 1.37
 *                All of 1.36 + IM_BMPSCOPE/IM_EMPSCOPE
 * 16.1        -- 1.38
 *                All of 1.37 + IM_MPLOOP/IM_MPSCHED and
 *                    IM_MPBORDERED/IM_MPEORDERED + TPALLOC + IM_FLUSH flag
 * 16.4        -- 1.39
 *                All of 1.38 + IM_ETASK and IM_TASKFIRSPRIV
 * 16.5        -- 1.40
 *                All of 1.39 + ISOTYPE flag + update MP_SCHED and MPLOOP ilm
 * 16.6        -- 1.41
 *                All of 1.40 + IM_LSECTION
 * 16.6        -- 1.42
 *                All of 1.41 + VARARG
 * 16.8        -- 1.43
 *                All of 1.42 + ALLOCATTR + F90POINTER
 * 16.10       -- 1.44
 *                All of 1.43 +
 * IM_TASKGROUP/ETASKGROUP/TARGET/TARGETDATA/TARGETUPDATE/
 *                TARGETEXITDATA/TARGETENTERDATA/DISTRIBUTE/TEAMS and their
 * combinations
 *                constructs including TARGET/TEAMS/DISTRIBUTE/PARALLEL
 * DO/CANCEL/
 *                CANCELLATIONPOINT.
 * 17.0        -- 1.45
 *                All of 1.44 + INVOBJINC + PARREF for ST_PROC
 * 17.2        -- 1.46
 *                All of 1.45 + etls + tls, irrspective of target
 * 17.7        -- 1.47
 *                All of 1.46 + BPARA + PROC_BIND + MP_ATOMIC..
 * 17.10        -- 1.48 
 *                All of 1.47 + ETASKFIRSTPRIV, MP_[E]TASKLOOP, 
 *                MP_[E]TASKLOOPREG
 * 18.1         -- 1.49 
 *                All of 1.48 , MP_TASKLOOPVARS, [B/E]TASKDUP
 */
#define VersionMajor 1
#define VersionMinor 48

void upper(int);
void upper_assign_addresses(void);
void upper_save_syminfo(void);
void add_llvm_uplevel_symbol(int);
void fixup_llvm_uplevel_symbol(void);
int llvm_get_uplevel_newsptr(int oldsptr);
int F90_nme_conflict(int nme1, int nme2);
int IPA_func_almostpure(int sptr);
int IPA_func_pure(int sptr);
int IPA_pointer_safe(int nme);
int IPA_nme_conflict(int nme1, int nme2);
void stb_upper_init(void);
void upper_init(void);
void cuda_emu_start(void);
void cuda_emu_end(void);
