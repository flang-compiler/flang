!* Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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

!   Transfer intrinsic with derived type MOLD
module fred
  integer, parameter :: i1_kind = selected_int_kind(2)
  type, public :: byte_type
    private
    integer(i1_kind) :: data
  end type
contains
  subroutine put_char (data, data_pos, string)
    type(byte_type) :: data(*)
    integer :: data_pos
    character*(*) :: string
    data(data_pos+1:data_pos+len(string)) = transfer(string,data(1:0))
    data_pos = data_pos + len(string)
  end subroutine put_char
  subroutine init(data)
   type(byte_type)::data(:)
   data(:)%data = 0
  end subroutine
  subroutine copy(data,array,n)
   type(byte_type)::data(:)
   integer::array(:)
   do i = 1,n
    array(i) = data(i)%data
   enddo
  end subroutine
end module fred
program p
 use fred
 type(byte_type):: d1(20)
 integer p1
 integer result(21),expect(21)
 data expect/97,32,108,111,110,103,101,114,32,115,116,114,105,110,103,&
	0,0,0,0,0,15/
 call init(d1)
 p1 = 0
 call put_char(d1,p1,'a longer string')
 call copy(d1,result,20)
 result(21) = p1
 call check(result,expect,21)
end
