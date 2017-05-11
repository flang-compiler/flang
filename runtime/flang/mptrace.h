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
 * external declarations for libpgc routinens in mptrace.c
 */

extern void _mp_trace_parallel_enter(void);
extern void _mp_trace_parallel_exit(void);
extern void _mp_trace_parallel_begin(void);
extern void _mp_trace_parallel_end(void);
extern void _mp_trace_sections_enter(void);
extern void _mp_trace_sections_exit(void);
extern void _mp_trace_section_begin(void);
extern void _mp_trace_section_end(void);
extern void _mp_trace_single_enter(void);
extern void _mp_trace_single_exit(void);
extern void _mp_trace_master_enter(void);
extern void _mp_trace_master_exit(void);
extern void _mp_trace_loop_enter(void);
extern void _mp_trace_loop_exit(void);


