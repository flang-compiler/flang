! Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

! do concurrent variable declaration errors

subroutine s1 ! ok
  implicit none
  integer :: a(50) = 1
  do concurrent (integer(2)::i=1:50)
    a(i) = a(i) + 1
  end do
  print*, all(a.eq.2)
end

subroutine s2 ! ok
  integer :: a(50) = 1
  do concurrent (i=1:50) local(k)
    k = 1
    a(i) = a(i) + k
  end do
  print*, all(a.eq.2)
end

subroutine s3
  implicit none
  integer :: a(50) = 1
  !{error "PGF90-S-0038-Symbol, i, has not been explicitly declared"}
  do concurrent (i=1:50)
    a(i) = a(i) + 1
  end do
  print*, all(a.eq.2)
end

subroutine s4
  implicit none
  integer :: a(50) = 1
  do concurrent (integer::i=1:50)
    k = 1
    a(i) = a(i) + k
  !{error "PGF90-S-0038-Symbol, k, has not been explicitly declared"}
  end do
  print*, all(a.eq.2)
end

subroutine s5
  implicit none
  integer :: a(50) = 1
  !{error "PGF90-S-0038-Symbol, k, has not been explicitly declared"}
  do concurrent (integer(kind=8)::i=1:50) local(k)
    k = 1
    a(i) = a(i) + k
  end do
  print*, all(a.eq.2)
end

subroutine s6
  integer :: a(50) = 1
  !{error "PGF90-S-1062-LOCAL_INIT variable does not have an outside variable of the same name - k"}
  do concurrent (i=1:50) local_init(k)
    k = 1
    a(i) = a(i) + k
  end do
  print*, all(a.eq.2)
end

  call s1
  call s2
  call s3
  call s4
  call s5
  call s6
end
