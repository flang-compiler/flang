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

#ifndef _PGHPF_SHMEM_REDUCE_H_
#define _PGHPF_SHMEM_REDUCE_H_

#define _SHMEM_REDUCE_SYNC_SIZE 36
#define _SHMEM_SYNC_VALUE (-1)
#define _SHMEM_REDUCE_MIN_WRKDATA_SIZE 8
void shmem_short_sum_to_all(int *target, int *source, int nreduce, int pe_start,
                            int logpe_stride, int pe_size, int *pwrk,
                            long *psync);
void shmem_float_sum_to_all(float *target, float *source, int nreduce,
                            int pe_start, int logpe_stride, int pe_size,
                            float *pwrk, long *psync);
void shmem_double_sum_to_all(double *target, double *source, int nreduce,
                             int pe_start, int logpe_stride, int pe_size,
                             double *pwrk, long *psync);

void __fort_shmem_reduce_setup(void **pwrk, long **psync);

#endif /*_PGHPF_SHMEM_REDUCE_H_*/
