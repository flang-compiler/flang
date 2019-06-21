! Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
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
! Issue 550: bind attribute is a concatenated expression
! 

program main
  implicit none
  interface
    subroutine go_print(f) bind(C,name="my_"//"printing")
      CHARACTER(len=1) :: f
    end subroutine
  end interface

  call go_print("Something "//"to print")
end program

