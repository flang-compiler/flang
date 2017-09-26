! Copyright (c) 2005, NVIDIA CORPORATION.  All rights reserved.
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
! Test array parameter in contained subprogram
! was failing with -Mipa
!
program p
 interface
  subroutine t1(n)
    integer :: n(*)
  end subroutine
 end interface

  integer :: rr(8)
  integer :: ex(8)
  data ex/97,98,99,100,97,98,99,100/
  call t1( rr )
  call check(rr,ex,8)
  !print *,rr
end

subroutine t1(n)
  integer, intent(inout) :: n(*)
  call t2( n(1), 0, 1 )
  call t2( n(2), 1, 1 )
  call t2( n(3), 2, 1 )
  call t2( n(4), 3, 1 )
  call t2( n(5), 0, 7 )
  call t2( n(6), 1, 7 )
  call t2( n(7), 2, 7 )
  call t2( n(8), 3, 7 )
contains
  subroutine t2( r, n, m )
    integer :: r, n, m
    character(len=8),parameter,dimension(0:3):: hm = &
      & (/'aaaaaaaa','bbbbbbbb','cccccccc','dddddddd'/)
    r = iachar( hm(n)(m:m) )
  end subroutine
end subroutine

