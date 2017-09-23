! Copyright (c) 1990-2017, NVIDIA CORPORATION.  All rights reserved.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

! User code gets execution seg fault in reshape intrinsic.
! Actually nothing to do with reshape.  The problem is in an array
! constructor in which an inner loop or array section depends on an
! outer loop variable like this: (/ (b(:j), j = 1, 3) /)
! The size of the space to be allocated for the section and for the
! constructed array depend on j, but space must be allocated before 
! the loops are executed.

! In this test case, the init and limit expressions of the outer loop are
! constants, so the size can be determined at compile time.

program unpack1
  implicit none

  integer, parameter :: m = 8, n = 30, k = 6
  integer, dimension(m), parameter :: b = (/ 3, 4, 2, 2, 6, 6, 5, 2 /)

  integer, dimension(n) ::  &
       a = (/1,4,7,2,6,9,8,5,2,5,1,6,3,6,4,7,8,4,0,4,3,2,7,8,9,4,3,2,1,5/)
  integer, dimension(k,m) :: d
  integer :: j, i, l
  integer, dimension(k*m) :: results
  integer, dimension(k*m) :: expect = (/ &
        1, 2, 5, 5, 6, 4, 8, 1, &
        4, 6, 2, 1, 3, 0, 9, 5, &
        7, 9, 0, 0, 6, 4, 4, 0, &
        0, 8, 0, 0, 4, 3, 3, 0, &
        0, 0, 0, 0, 7, 2, 2, 0, &
        0, 0, 0, 0, 8, 7, 0, 0 /)


  d = reshape((/ (a(sum(b(:j-1))+1:sum(b(:j))),  &
       (0, i = 1, k-b(j)), j = 1, m) /), (/ k, m /))

!  do l = 1, k
!     write (*, "(9i2)") d(l,:)
!  end do

  do l = 1, k
    do i = 1,m
      results((l-1)*m+i) = d(l, i)
    end do
  end do
!  print *, results
  call check(results, expect, m*k)
end program unpack1
