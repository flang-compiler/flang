!* Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
!* See https://llvm.org/LICENSE.txt for license information.
!* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! RUN: %flang -O0 -fopenmp -S -emit-llvm %s -o - | FileCheck %s
! RUN: %flang -i8 -O0 -fopenmp -S -emit-llvm %s -o - | FileCheck %s

module omp_atomic_load_logical
  use, intrinsic :: iso_fortran_env, &
                    only: logical64, logical32, logical16, logical8
  implicit none
  logical(logical8) :: l8 = .false.
  logical(logical16) :: l16 = .false.
  logical(logical32) :: l32 = .false.
  logical(logical64) :: l64 = .false.

contains

subroutine sub8
  implicit none
  logical(kind(l8)) :: v

  v = .false.
  do
!$OMP ATOMIC READ
    v = l8
! //CHECK: load atomic i8, i8*
    if (v) exit
  end do
end subroutine

subroutine sub16
  implicit none
  logical(kind(l16)) :: v

  v = .false.
  do
!$OMP ATOMIC READ
    v = l16
! //CHECK: load atomic i16, i16*
    if (v) exit
  end do
end subroutine

subroutine sub32
  implicit none
  logical(kind(l32)) :: v

  v = .false.
  do
!$OMP ATOMIC READ
    v = l32
! //CHECK: load atomic i32, i32*
    if (v) exit
  end do
end subroutine

subroutine sub64
  implicit none
  logical(kind(l64)) :: v

  v = .false.
  do
!$OMP ATOMIC READ
    v = l64
! //CHECK: load atomic i64, i64*
    if (v) exit
  end do
end subroutine

end module
