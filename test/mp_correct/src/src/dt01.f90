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

! Ensure that the temp created for the derived type constructor has the
! correct storage class

      MODULE dt01
      TYPE :: force_type
        integer  Xint    ! X direction internal force
        integer  Yint    ! Y direction internal force
        integer  Zint    ! Z direction internal force
        integer  Xext    ! X direction external force
        integer  Yext    ! Y direction external force
        integer  Zext    ! Z direction external force
      END TYPE
      TYPE (force_type), DIMENSION(:), ALLOCATABLE :: FORCE
      END MODULE

      subroutine solve(numrt)
      use dt01
      integer :: n, numrt
!$OMP PARALLEL DO
      DO N = 1,NUMRT
         FORCE(N) = force_type (numrt,0,1,0,0,n)
      ENDDO
      end

      use dt01
      integer results(3), expect(3)
      data expect/1000000, 1000, 500500/
      nn = 1000
      allocate(FORCE(nn))
      call omp_set_num_threads(4)
      call solve(nn)
      results(1) = sum(FORCE%Xint)
      results(2) = sum(FORCE%Zint)
      results(3) = sum(FORCE%Zext)
!!!      print *, results
      call check(results, expect, 3)
      end
