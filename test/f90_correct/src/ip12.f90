!
! Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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
! check that loops in containing routines don't update counter in container

program p
 integer i,j,k
 integer result(4)
 integer expect(4)
 data expect/5,50,15,150/
 j = 0
 result(2) = 0
 do i = 1,5
  j = j + 1
  call sub(k)
  result(2) = result(2) + k
 enddo
 result(1) = j
 j = 0
 result(4) = 0
 do i = 1,15
  j = j + 1
  call sub(k)
  result(4) = result(4) + k
 enddo
 result(3) = j
!print *,result
 call check(result,expect,4)

contains
 subroutine sub(k)
  integer i,k
  k = 0
  do i = 1,10
   k = k + 1
  enddo
 end subroutine
end program
