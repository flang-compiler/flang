! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! Test that MIN/MAX intrinsic have not INTEGER, REAL, or CHARACTER arguments.

program test
  complex(kind = 4) :: c1 = 1.0
  complex(kind = 4) :: c2 = 1.0
  logical :: l1 = .true.
  logical :: l2 = .true.
  real :: res
  !{error "PGF90-S-0155-Arguments must be INTEGER, REAL, or CHARACTER!"}
  !{error "PGF90-S-0155-Arguments must be INTEGER, REAL, or CHARACTER!"}
  res = max(c1, c2)
  !{error "PGF90-S-0155-Arguments must be INTEGER, REAL, or CHARACTER!"}
  !{error "PGF90-S-0155-Arguments must be INTEGER, REAL, or CHARACTER!"}
  res = min(l1, l2)
end

