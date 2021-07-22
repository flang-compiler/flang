! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! RUN: %flang -S -emit-llvm %s

MODULE dbcsr_operations
  IMPLICIT NONE
  PUBLIC :: dbcsr_add
  PRIVATE
  INTERFACE dbcsr_add
    MODULE PROCEDURE dbcsr_add_d
  END INTERFACE
CONTAINS
  SUBROUTINE dbcsr_add_d()
    print *, "add_d and anytype."
  END SUBROUTINE dbcsr_add_d
END MODULE dbcsr_operations

MODULE dbcsr_api
  USE dbcsr_operations,ONLY: &
    dbcsr_add_prv=>dbcsr_add
  IMPLICIT NONE
  PUBLIC :: dbcsr_add
  PRIVATE
  INTERFACE dbcsr_add
    MODULE PROCEDURE dbcsr_add_dd
  END INTERFACE
  CONTAINS
    SUBROUTINE dbcsr_add_dd()
      print *, "add_dd in API."
    END SUBROUTINE
END MODULE dbcsr_api

PROGRAM dbcsr_test_csr_conversions
USE dbcsr_api,ONLY:  dbcsr_add
IMPLICIT NONE
  CONTAINS
  SUBROUTINE csr_conversion_test()
    CALL dbcsr_add()
  END SUBROUTINE csr_conversion_test
END PROGRAM dbcsr_test_csr_conversions

