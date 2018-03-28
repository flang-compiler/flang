! Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

program omp_bug 
integer, parameter :: cnt = 100 
integer, pointer :: bnd(:,:) 
integer, pointer :: chamber(:) 
integer :: i, c 

allocate(bnd(1:2,cnt)) 
do i = 1, cnt 
bnd(1:2, i) = (/ i, i+1 /) 
end do 

!print *, "bnd:", bnd



allocate(chamber(1:cnt+1), source=0) 

!$omp parallel do private(i) 
do i = 1,cnt 
chamber(bnd(1:2,i)) = 1 
end do 
!$omp end parallel do 

do i = 1, cnt+1 
c = chamber(i) 
if (c <= 0) then 
print *, "FAIL"
call abort() 
end if 
end do 

print *, "PASS"

end 
