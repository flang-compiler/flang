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
 
!       PRIVATE variables
!       ALLOCATABLE

      subroutine sub
      integer omp_get_thread_num
        integer, ALLOCATABLE :: A( : )
        INTEGER err
        ALLOCATE( A( 5 : 6 ), STAT = err )
        IF ( err .NE. 0 ) THEN
          PRINT *, 'Error occured while allocating when serial'
          STOP
	ENDIF
	a(5) = 5
	a(6) = 6
!$OMP   PARALLEL PRIVATE( A, ii, err )
!       since the outer A is allocated, a private copy is allocated using
!       the same bounds.
	  DEALLOCATE(A)  ! the private copy is deallocated
          ALLOCATE( A( 1 : 2 ), STAT = err )
          IF ( err .NE. 0 ) THEN
            PRINT *, 'Error occured while allocating when parallel'
            STOP
          END IF
          ii =  2*omp_get_thread_num()
          A(1) = ii+1
	  A(2) = ii+2
	  call stuff(a, ii+1)
          DEALLOCATE( A )
!$OMP   END PARALLEL
	call stuff(a,5)
        DEALLOCATE( A )
      END
      program test
      integer, dimension(6) :: expect = (/1,2,3,4,5,6/)
      integer result(6)
      common /result/result
      call omp_set_num_threads(2)
      call sub
      call check(result, expect, 6)
      end
      subroutine stuff(a,id)
      integer result(6)
      common /result/result
      integer a(2)
!$omp critical
      result(id) = a(1)
      result(id+1) = a(2)
!$omp endcritical
      end
