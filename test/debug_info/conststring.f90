! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! REQUIRES: llvm-19
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

! Check if string constants are stored correctly for special characters.

! \0 = 0 , " = 34 = 0x22, \n = 10, \r = 13
! CHECK-LABEL: define void @MAIN_
! CHECK: #dbg_value([10 x i8] c"a\00d\22g     ", [[CONSTR1:![0-9a-f]+]], !DIExpression()
! CHECK: #dbg_value([10 x i8] c"h i~j     ",     [[CONSTR2:![0-9a-f]+]], !DIExpression()
! CHECK: #dbg_value([10 x i8] c"k\0Al\0Dm     ", [[CONSTR3:![0-9a-f]+]], !DIExpression()
! CHECK-LABEL: distinct !DISubprogram(name: "main"
! CHECK: [[CONSTR1]] = !DILocalVariable(name: "constr1",
! CHECK: [[CONSTR2]] = !DILocalVariable(name: "constr2",
! CHECK: [[CONSTR3]] = !DILocalVariable(name: "constr3",

program main
  character(10),parameter :: constr1 = "a"//achar(0)//"d"//achar(34)//"g"
  character(10),parameter :: constr2 = "h i~j"
  character(10),parameter :: constr3 = "k"//achar(10)//"l"//achar(13)//"m"

  print *,constr1
  print *,constr2
  print *,constr3
end
