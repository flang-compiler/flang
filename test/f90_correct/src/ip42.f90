!
! Copyright (c) 2005-2017, NVIDIA CORPORATION.  All rights reserved.
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
! array-valued internal function whose result specification
! depends on the value of a host dummy and the host is a module routine
!
module f21260
contains
  subroutine host(xxx,ub)
  integer :: xxx, ub
  integer :: m
  m = xxx
  call internal()
  contains
    subroutine internal()
      call zzz(xxx)
! for the invocation of foo(), a temporary is allocated for the array result.
! Its upper bound references a host dummy argument.  At the point of call,
! there are two symbol table entries for host (an ST_PROC & ST_ENTRY); there
! are also two instances of the host dummy, xxx.  
      ub = ubound(foo(), 1)
    end subroutine
    function foo()
      integer, dimension(xxx) :: foo
      foo(xxx) = 3
    end function
  end subroutine
end module
  use f21260
  common /result/ires(2)
  integer :: expect(2) = (/3,3/)
  call host(3, iub)
!  print *, 'expect 3 ',iub
  ires(2) = iub
!  print *, ires
  call check(ires, expect, 2)
end
subroutine zzz(iii)
  common /result/ires(2)
!  print *, 'zzz ', iii
  ires(1) = iii
end
