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
!
!   type constructors with pointer members including a recursive derived
!   type.
!

module aa
  TYPE value_list
     INTEGER :: typ
     integer, pointer, dimension(:) :: val
     TYPE(value_list), POINTER :: previous
  END TYPE value_list
  TYPE(value_list) :: crv
  integer, target, dimension(4) :: arr
contains
  subroutine init
      arr = (/1, 2, 3, 4/)
  endsubroutine
  subroutine create
      TYPE(value_list), pointer :: p
      integer, pointer, dimension(:) :: ap
      allocate(p)
      p%typ = 55
      nullify(p%previous)
      p%val => arr(2:4)
      ap => arr(1:3)
      crv = value_list(77, ap, p)
  endsubroutine
  subroutine get(ir, qq)
      integer, intent(out) :: ir(*)
      TYPE(value_list) :: qq
      ir(1) = qq%typ
      ir(2) = qq%val(1)
      ir(3) = qq%val(2)
      ir(4) = qq%val(3)
  endsubroutine
endmodule

program test
  use aa
  integer, parameter :: nt = 8
  integer, dimension(nt) :: result, expect

  call init

  call create
!  print *, crv%typ
!  print *, crv%val(1),crv%val(2),crv%val(3)
!  print *, crv%previous%typ
!  print *, crv%previous%val(1),crv%previous%val(2), crv%previous%val(3)

  call get(result, crv)
  call get(result(5), crv%previous)
!  print *, result

  data expect/77,1,2,3,55,2,3,4/
  call check(result, expect, nt);

end
