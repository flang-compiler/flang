!* Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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

!**    This test must be run with more than 1 thread.
       PROGRAM is_it_in_final
       
       use omp_lib

       integer n
       integer result(1)
       integer expect(1)
       parameter(n=1)
!$omp parallel
!$omp single
!$omp task
!$omp critical
       result(1) = omp_in_final()
!$omp end critical
!$omp end task
!$omp end single
!$omp end parallel

       call check(result,expect,n)

       END PROGRAM is_it_in_final
