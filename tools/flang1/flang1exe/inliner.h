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

/**
 *  \file
 *  \brief - publicly accessible functions for inliner
 */
extern void extractor_command_info(char *sDir, int ignore, char *sFunc);
extern void extractor_end(void);
extern void extractor(void);
extern int extractor_possible(void);
extern void inline_add_lib(char *sDir);
extern void inline_add_func(char *sFunc, int nSize);
extern void inliner(void);
