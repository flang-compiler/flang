! Copyright (c) 2010, NVIDIA CORPORATION.  All rights reserved.
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

integer function foo(x)
  integer x
  integer, save :: i
  if (x .eq. 0) then
     i = 0
  else
     i = i + x
  endif
  foo = i
end function foo

program p
USE CHECK_MOD
  interface
     integer function foo(x)
       integer x
     end function foo
  end interface
  logical expect(4)
  logical rslt(4)
  integer i
  integer f
  expect = .true.
  rslt = .false.
  i = foo(0)
  associate ( f => foo(1), j=>i)
  rslt(1) = (f .eq. 1)
  rslt(2) = (f .eq. 1)
  rslt(3) = (j .eq. 0)
  i = i + 1
  rslt(4) = (j .eq. 1)
end associate

  call check(rslt,expect,4)

end program p	
