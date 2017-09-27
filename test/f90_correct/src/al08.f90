! Copyright (c) 1990-2012, NVIDIA CORPORATION.  All rights reserved.
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
!
!  Simple target associated test.
!  Watch out for -Mchkptr
module m
   type t
      complex, pointer, dimension(:) :: p => null()
      complex, allocatable, dimension(:)  :: q
   end type t
   contains
     subroutine init(x)
       type(t), intent(inout) :: x
       nullify(x%p)
     end subroutine init
     subroutine check_assoc(x)
       type(t), intent(inout), target :: x
       if( associated(x%p,x%q) ) then
	  call check(0,1,1)
          write(*,*) "Yes"
       else
          write(*,*) "No"
	  call check(1,1,1)
       end if
     end subroutine check_assoc
   end module m
program assoc_test
   use m
   type(t) :: x
   call init(x)
   call check_assoc(x)
end program assoc_test
