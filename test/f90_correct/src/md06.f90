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
! bug in 1.7
! host association caused an error
!
module aa
 integer f
end module
module bb
 use aa
 contains
  subroutine jj(f)
   integer f
   f = f + 1
  end subroutine
end module

program p
 use aa
 use bb
 integer g
 integer result(2),expect(2)
 data expect/11,10/
 f = 9
 g = 10
 call jj(g)
 call jj(f)
 result(1) = g
 result(2) = f
 call check(result,expect,2)
end
