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


program tst
implicit none
integer :: i,j,k
character(len=4), dimension(3) :: chres
integer, dimension(9) :: results
integer, dimension(9) :: expect = (/ 1414092101, 1414092869, 1414092869, &
 & 1414092101, 1414092101, 1414092869, 1414092101, 1414092101, 1414092101 /)
k=1
do i=1,3
   call sub(chres)
   do j=1,3
     results(k) = transfer(chres(j), i)
     k=k+1
   end do
end do
!print *,results
call check(results, expect, 9)

contains

subroutine sub(res)
implicit none
character(len=4), dimension(3) :: res
integer,save :: i=0
character , dimension(3) :: exitc*4=(/ ('EXIT',i=1,3) /)
!write(*,'(T2,"Exit = ",3A4)')exitc
i=i+1
exitc(i)(2:2)='U'
res=exitc
return
end subroutine sub

end program tst

