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

module nested_types_module

  implicit none
!  private
  
  public type1, type2 

  type type1 
     procedure(used_function), pointer, nopass :: func1 => null()
  end type type1

  type type2 
     type(type1) :: member
  end type type2
  contains

     function unused_function() result(ret_val)
       integer :: ret_val
       ret_val = 2
     end function unused_function

     function used_function(thing1, int1) result(ret_val)
       type(type1), intent(inout) :: thing1
       integer, intent(in) :: int1
       real :: ret_val(int1) 
       ret_val = 2.0
     end function used_function

    
end module nested_types_module
