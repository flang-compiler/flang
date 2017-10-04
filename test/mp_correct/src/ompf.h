!* Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* Unless required by applicable law or agreed to in writing, software
!* distributed under the License is distributed on an "AS IS" BASIS,
!* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!* See the License for the specific language governing permissions and
!* limitations under the License.

!	logical OpenMP functions
	logical omp_get_dynamic, omp_get_nested
	external omp_get_dynamic, omp_get_nested
	logical omp_in_parallel, omp_test_lock
	external omp_in_parallel, omp_test_lock
!	integer OpenMP functions
	integer omp_get_max_threads, omp_get_num_procs
	external omp_get_max_threads, omp_get_num_procs
	integer omp_get_num_threads, omp_get_thread_num
	external omp_get_num_threads, omp_get_thread_num
!	OpenMP subroutines
	external omp_destroy_lock, omp_init_lock
	external omp_set_dynamic, omp_set_lock
	external omp_set_nested, omp_set_num_threads
	external omp_unset_lock
