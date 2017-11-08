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

#ifndef LL_WRITE_H_
#define LL_WRITE_H_

#include <stdio.h>

#include "ll_structure.h"

void ll_write_module(FILE *, LLVMModuleRef);
void ll_write_module_header(FILE *out, LLVMModuleRef module);
void ll_write_function(FILE *, struct LL_Function_ *, LLVMModuleRef module);
void ll_write_basicblock(FILE *, struct LL_Function_ *,
                         struct LL_BasicBlock_ *, LLVMModuleRef module);
void ll_write_instruction(FILE *, struct LL_Instruction_ *, LLVMModuleRef module);
void ll_write_function_signature(FILE *, struct LL_Function_ *);
void ll_write_global_var_signature(FILE *, LL_Value *);
void ll_write_user_structs(FILE *out, LLVMModuleRef module);
void ll_write_llvm_used(FILE *out, LLVMModuleRef module);
void ll_write_global_objects(FILE *out, LLVMModuleRef module);
void ll_write_local_objects(FILE *out, struct LL_Function_ *function);
void ll_write_metadata(FILE *out, LLVMModuleRef module);
void ll_write_object_dbg_references(FILE *, LL_Module *, LL_ObjToDbgList *);

#endif
