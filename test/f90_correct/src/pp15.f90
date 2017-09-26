! Copyright (c) 1999, NVIDIA CORPORATION.  All rights reserved.
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
! Test noncontiguous pointers.
! Bug when an outer-scope pointer is referenced in an internal procedure
!
integer, parameter:: N=10
integer, target :: a(10,N)
integer, pointer :: p(:)
integer :: expect(N) = (/1,2,3,4,5,6,7,8,9,10/) 
integer :: result(N)

p=>a(2,:)
call foo

!print *,  a(2,:)
result = a(2,1:N)
call check(result, expect, N)

contains
    subroutine foo
    do i = 1, N
        p(i) = i   !p's SDSCNS1 flag must be set in the backend
    enddo
    endsubroutine
end
