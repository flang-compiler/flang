!* Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

subroutine testCase8_icv
    use omp_lib
    !$omp parallel
        !$omp task
            !$omp parallel
                PRINT *, omp_get_max_threads()
            !$omp end parallel
        !$omp end task
    !$omp end parallel
end subroutine
program fortran_omp_task
call testcase8_icv
print *, "PASS"
end program fortran_omp_task
