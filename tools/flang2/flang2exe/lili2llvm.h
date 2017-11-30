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

#ifndef __LILI2LLVM_H__
#define __LILI2LLVM_H__

#include <stdio.h>

struct lili2llvm_module_ *lili2llvm_createmodule();
void lili2llvm_destroymodule(struct lili2llvm_module_ *);

const struct LL_Type_ *llgetlldtype(int, int);

void lili2llvm_open_kernel_file(FILE *);
void lili2llvm_add_kernel(int v, struct lili2llvm_module_ *, const char *, int,
                          int, int);
void lili2llvm_output(int v, const char *, int, int, int, int);
void lili2llvm_empty_compile_unit(FILE *);
void lili2llvm_init();
void lili2llvm_finalize(FILE *);
void lili2llvm_reset();
void lili2llvm_decl_statics(char *, int, LOGICAL);
void lili2llvm_declare_device_static(char *, int);
void lili2llvm_declare_device_extern(char *, int);
void lili2llvm_declare_device_commonblock(char *, int, LOGICAL, ISZ_T);
void lili2llvm_declare_device_common(char *, int);

void lili2llvm_create_texture_sym(int, char *);

void lili2llvm_declare_device_extern_ftn(char *, int, LOGICAL, ISZ_T);

void lili2llvm_declare_device_static_array(char *, int, ISZ_T);
void lili2llvm_declare_device_common_array(char *, int, ISZ_T);
void lili2llvm_declare_device_extern_array(char *, int, ISZ_T);

void lili2llvm_put_dinit(int, int, LOGICAL);
void lili2llvm_decl_common(char *, int, int);
void lili2llvm_decl_const_param(FILE *, char *, int);

struct LL_DebugInfo;
struct LL_DebugInfo *get_current_l2l_debug_info(void);

struct lili2llvm_module_ *get_current_l2l_module(void);
void set_current_l2l_module(struct lili2llvm_module_ *);

void l2l_cleanup_missing_bounds(void);

#endif /* __LILI2LLVM_H__ */
