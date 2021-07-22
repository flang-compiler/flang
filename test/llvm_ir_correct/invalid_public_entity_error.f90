! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! RUN: %flang -S -emit-llvm %s

MODULE dbcsr_operations
 PUBLIC :: dbcsr_norm
 INTERFACE dbcsr_norm
  MODULE PROCEDURE dbcsr_norm_anytype
 END INTERFACE
 CONTAINS
 SUBROUTINE dbcsr_norm_anytype
   print *, "hello world"
 END SUBROUTINE
END MODULE dbcsr_operations

MODULE dbcsr_api
use dbcsr_operations
 use dbcsr_operations, only: &
 dbcsr_norm_prv => dbcsr_norm
 public::dbcsr_norm
private
 CONTAINS
 SUBROUTINE dbcsr_norm()
  print *, "hellow world from dbcsr_api "
 end subroutine
end module

program main
use dbcsr_api, only : dbcsr_norm
 call dbcsr_norm();
end program
