!
! Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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
!

  program p
   integer results(9), expect(9)
   data expect / 12,12,13,22,22,23,32,32,33 /
   type ttype
    integer, dimension(3) :: X
   end type
   type(ttype), dimension(3) :: var
   integer i,j,k
   do i = 1,3
    do j = 1,3
     var(i)%x(j) = i*10+j
    enddo
   enddo
   i = 1
   j = 2
   var%X(i) = var%X(j)
   do i = 1,3
    do j = 1,3
     k = (i-1)*3+j
     results(k) = var(i)%X(j)
    enddo
   enddo
  call check(results,expect,9)
  END
