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

RECURSIVE INTEGER*8 FUNCTION parallel_sum(a)

    INTEGER, INTENT(IN) :: a(:)

    INTEGER*8 :: x

    x = 11

    !$omp task shared(x) firstprivate(a)
    x = 1
    !$omp end task

    !$omp taskwait

    parallel_sum = x
END FUNCTION

INTEGER*8 FUNCTION full_sum(a)

    interface
RECURSIVE INTEGER*8 FUNCTION parallel_sum(a)
    INTEGER, INTENT(IN) :: a(:)
END FUNCTION
    end interface

    INTEGER, INTENT(IN) :: a(:)

    INTEGER*8 :: s

    !$omp parallel
        s = parallel_sum(a)
    !$omp end parallel
    
    full_sum = s

    RETURN
END FUNCTION

PROGRAM fortran_omp_taskwait

    interface
INTEGER*8 FUNCTION full_sum(a)
    INTEGER, INTENT(IN) :: a(:)
END FUNCTION
    end interface

    INTEGER, ALLOCATABLE :: a(:)

    INTEGER*8 :: s, expected
    INTEGER :: i

    PRINT *, ''

    ALLOCATE(a(N))

    s = full_sum(a)

    PRINT *, "PASS"

END PROGRAM fortran_omp_taskwait
