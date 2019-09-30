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


subroutine foo(arg)
  use iso_c_binding
  integer, pointer :: z
  type(c_ptr), value, intent(in)  :: arg
  call c_f_pointer(arg, z);
  if (z .eq. -99) then
    print *, 'PASS'
  else
    print *, 'FAIL'
  endif
end subroutine foo

program p
  use iso_c_binding
  
  interface
     subroutine foo(arg)
       use iso_c_binding
       type(c_ptr), value, intent(in)  :: arg
     end subroutine foo

     subroutine func(arg) bind(c)
       use iso_c_binding
       type(c_funptr), value, intent(in)  :: arg
     end subroutine func
  end interface
  
  abstract interface
     subroutine ifoo(arg)
       use iso_c_binding
       type(c_ptr), value, intent(in)  :: arg
     end subroutine ifoo
  end interface
  
  
  procedure(ifoo), bind(c), pointer :: proc
  type(c_funptr) :: tmp_cptr
  
  proc => foo
  
  tmp_cptr = c_funloc(proc)
  
  call func(tmp_cptr)
end program p
