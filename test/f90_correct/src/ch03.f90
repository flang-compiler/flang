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
!  array constructors of character and assumed-length character
!
module stopwatch

 interface watch
  module procedure watch_a, watch_s
 end interface

 integer n
 integer result(100)
contains

 subroutine create()
  call watch(clock=(/"cpu ","user","sys ","wall"/))
 end subroutine

 subroutine watch_a(clock)
  character(len=*), intent(in), dimension(:) :: clock
  !print *,'in watch_a, character len is ', len(clock(1))
  n = n + 1
  result(n) = len(clock(1))
  do i = 1,ubound(clock,1)
   !print *, i, '->', clock(i)
   n = n + 1
   result(n) = iachar(clock(i)(1:1))
  enddo
 end subroutine

 subroutine watch_s(clock)
  character(len=*), optional, intent(in) :: clock
  if (present(clock)) then
   call watch_a((/clock/))
  else
   call watch_a((/"none"/))
  end if
 end subroutine
end module

 use stopwatch
 integer expect(15)
 data expect/4,99,117,115,119,4,110,10,117,4,100,98,97,102,-1/

 n = 0
 call create
 call watch_s()
 call watch_s('ugly clock')
 call watch_a( (/ 'dont','be','a','fool' /) )
 !print *,n
 !print *,result(1:n)
 n = n + 1
 result(n) = -1
 call check(result,expect,15)
end
