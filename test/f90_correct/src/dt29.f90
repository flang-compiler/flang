!* Copyright (c) 1999, NVIDIA CORPORATION.  All rights reserved.
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* Unless required by applicable law or agreed to in writing, software
!* distributed under the License is distributed on an "AS IS" BASIS,
!* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!* See the License for the specific language governing permissions and
!* limitations under the License.
!
!   character members
!
module abcd
   integer :: result
   integer :: expect=0
   integer, parameter :: ntests=1
   contains 
      subroutine pqrs(a,b)
         character(len=*),intent(in) :: a
         character(len=*),dimension(:), intent(in) :: b
         integer :: i
	 result = 0
!         write(*,*)a,(b(i),i=1,SIZE(b))
	 if (a .ne. 'asdf')            result = 1
	 if (b(1) .ne. 'lfjsa')        result =  result + 2
	 if (b(2) .ne. 'alkdjfsadlfj') result =  result + 4
!	 print *, result
      end subroutine pqrs
end module abcd
program test
   use abcd
   type uvwx
      character(len=20), dimension(2) :: ijkl
   end type
   type(uvwx) :: qrst   
   qrst%ijkl(1)='lfjsa'
   qrst%ijkl(2)='alkdjfsadlfj'
   call pqrs('asdf',(/qrst%ijkl(1),qrst%ijkl(2)/))
   call check(result, expect, ntests)
end program
