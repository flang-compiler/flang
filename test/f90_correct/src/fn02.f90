!
! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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
! If there is a RESULT clause in a function header, the result name
! means the result value, the function name means the function itself

module bug

contains

  recursive function foo (i) result (foo_x)
   integer foo_x
   integer i
   foo_x = i
   call bar(foo,foo_x)
  end function foo

  recursive subroutine bar (f,i)
   integer i
   interface
    recursive function f (i) result (f_x)
     integer f_x
     integer i
    end function
   end interface
   if( i .gt. 0 ) then
    j = f(i-1)
    i = i + j
   endif
  end subroutine

end module bug

use bug
integer expect(1),result(1)
result(1) = foo(3)
expect(1) = 6
call check(result,expect,1)
end
