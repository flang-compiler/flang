! Copyright (c) 2011, NVIDIA CORPORATION.  All rights reserved.
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

module a
  implicit none
  type a_t
  logical :: result = .false.
   contains
     procedure :: say_hello
  end type a_t
  private
  public :: a_t
contains
  subroutine say_hello (this)
    class(a_t), intent(inout) :: this
    print *,'Hello from a'
    this%result = .true.
  end subroutine say_hello
end module a

module b
  use a
  implicit none
  type b_t
     type(a_t) :: a
   contains
     procedure :: say_hello
  end type b_t
contains
  subroutine say_hello (this)
    class(b_t), intent(inout) :: this
    call this%a%say_hello()
  end subroutine 
end module b

program p
USE CHECK_MOD
   use b
   logical results(1)
   logical expect(1)
   type(b_t) :: bt
   results = .false.
   expect = .true.
   call bt%say_hello()
   results(1) = bt%a%result
   call check(results,expect,1)
end
