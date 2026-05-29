! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! Test that MIN/MAX intrinsic have not the same kind type parameter.

program test
  real(kind = 16) :: r16 = 1.0
  integer(kind = 1) :: i1 = 1
  integer(kind = 2) :: i2 = 1
  integer(kind = 4) :: i4 = 1
  integer(kind = 8) :: i8 = 1
  character(len = 1) :: c = "a"
  real :: res
  !{warning "PGF90-W-0093-Type conversion of expression performed"}
  !{error "PGF90-S-0155-Arguments must have the same kind type parameter!"}
  res = max(r16, i1)
  !{warning "PGF90-W-0093-Type conversion of expression performed"}
  !{error "PGF90-S-0155-Arguments must have the same kind type parameter!"}
  res = min(r16, i1)
  !{warning "PGF90-W-0093-Type conversion of expression performed"}
  !{error "PGF90-S-0155-Arguments must have the same kind type parameter!"}
  res = max(r16, i2)
  !{warning "PGF90-W-0093-Type conversion of expression performed"}
  !{error "PGF90-S-0155-Arguments must have the same kind type parameter!"}
  res = min(r16, i2)
  !{warning "PGF90-W-0093-Type conversion of expression performed"}
  !{error "PGF90-S-0155-Arguments must have the same kind type parameter!"}
  res = max(r16, i4)
  !{warning "PGF90-W-0093-Type conversion of expression performed"}
  !{error "PGF90-S-0155-Arguments must have the same kind type parameter!"}
  res = min(r16, i4)
  !{warning "PGF90-W-0093-Type conversion of expression performed"}
  !{error "PGF90-S-0155-Arguments must have the same kind type parameter!"}
  res = max(r16, i8)
  !{warning "PGF90-W-0093-Type conversion of expression performed"}
  !{error "PGF90-S-0155-Arguments must have the same kind type parameter!"}
  res = min(r16, i8)
  !{error "PGF90-S-0074-Illegal number or type of arguments to max"}
  !{error "PGF90-S-0155-Arguments must have the same kind type parameter!"}
  res = max(r16, c)
  !{error "PGF90-S-0074-Illegal number or type of arguments to min"}
  !{error "PGF90-S-0155-Arguments must have the same kind type parameter!"}
  res = min(r16, c)
end
