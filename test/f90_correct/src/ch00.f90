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
! This has failed from time to time:
!  assumed-shape, assumed-length character array
!
 program main
  interface
   subroutine chfunc( aslc )
   character*(*) aslc(:)
   end subroutine
  end interface
  integer r,e
 
  character*5 c(4)
 
  c(1) = 'abcde'
  c(2) = 'fghij'
  c(3) = 'klmno'
  c(4) = 'pqrst'
 
  call chfunc( c )     ! call subroutine
  r = 1
  e = 1
  call check(r,e,1)
 end program
 
 subroutine chfunc( aslc )
  character*(*) aslc(:)
  do i = 1,ubound(aslc,1)
   print *,aslc(i)     ! error appears when compiling this ref to aslc
  enddo
 end subroutine

