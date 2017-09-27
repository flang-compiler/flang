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
!   Internal procedures
!   don't delete assignments to outer-block variables

program p

integer i,j,k
integer result(3),expect(3)
data expect/99,199,299/

i = 100
j = 98
k = 299		! k not used, but must not be deleted
call sub
result(1)=i	! i must not be forward-substituted
result(2)=j	! j must not be forward-substituted
call check(result,expect,3)

contains

subroutine sub

i = i - 1	! i never used, should not be deleted
j = 199		! j never used, should not be deleted
result(3) = k	! k comes from outer block

end subroutine sub
end program p
