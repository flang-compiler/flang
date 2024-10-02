! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! REQUIRES: llvm-19
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

! CHECK: #dbg_value(i32 1, [[ENM1:![0-9]+]], !DIExpression()
! CHECK: #dbg_value(i32 2, [[ENM2:![0-9]+]], !DIExpression()
! CHECK: #dbg_value(i32 5, [[ENM3:![0-9]+]], !DIExpression()
! CHECK: #dbg_value(i32 6, [[ENM4:![0-9]+]], !DIExpression()
! CHECK: [[ENM1]] = !DILocalVariable(name: "red"
! CHECK: [[ENM2]] = !DILocalVariable(name: "blue"
! CHECK: [[ENM3]] = !DILocalVariable(name: "black"
! CHECK: [[ENM4]] = !DILocalVariable(name: "pink"

program main
  enum, bind(c)
    enumerator :: red =1, blue, black =5
    enumerator :: pink
  endenum
  integer (kind=8) :: svar1, svar2, svar3, svar4
  svar1 = red
  svar2 = blue
  svar3 = black
  svar4 = pink

  print *, svar1, svar2, svar3, svar4

end program main
