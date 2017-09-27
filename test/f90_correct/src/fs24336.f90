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

program nested_types

  use nested_types_module
  implicit none

  type(type2) :: obj
  integer :: int1 = 1
  real :: ret(1)

  obj%member%func1=>used_function
  ret = obj%member%func1(obj%member,int1)
  if (ret(1) .eq. 2.0) then
    print *, "PASS"
  else
    print *, "FAIL"
  endif

end program nested_types
