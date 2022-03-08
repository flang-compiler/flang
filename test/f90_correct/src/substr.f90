!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!
! Test for implied do-loop contains substring in initialization.

program test
  implicit none
  integer :: i
  integer, parameter :: n = 8
  character(*), parameter :: a = "Hello world!"
  character(10), parameter :: z = ""
  character(*), parameter :: b1(2) = [(a(i:i+4),i=1,7,6)]
  character(5) :: b2(2) = [(a(i:i+4),i=1,7,6)]
  character(*), parameter :: c1 = transfer([(a(i:i),i=1,len(a))], a)
  character(12) :: c2 = transfer([(a(i:i),i=1,len(a))], a)
  character(*), parameter :: d1 = transfer([(a(i:i),i=len(a),1,-1)], a)
  character(12) :: d2 = transfer([(a(i:i),i=len(a),1,-1)], a)
  character(*), parameter :: e1 = transfer([(a(i:i+4),i=1,7,6)], z)
  character(10) :: e2 = transfer([(a(i:i+4),i=1,7,6)], z)
  character(5) :: cmp1(2)
  character(12) :: cmp2
  character(10) :: cmp3
  integer :: rslts(n), expect(n)

  DATA cmp1 /"Hello", "world"/
  DATA cmp2 /"!dlrow olleH"/
  DATA cmp3 /"Helloworld"/

  rslts = 0
  expect = 1

  if (len(b1) == 5 .and. all(b1 == cmp1)) rslts(1) = 1
  if (all(b2 == cmp1)) rslts(2) = 1
  if (len(c1) == 12 .and. c1 == a) rslts(3) = 1
  if (c2 == a) rslts(4) = 1
  if (len(d1) == 12 .and. d1 == cmp2) rslts(5) = 1
  if (d2 == cmp2) rslts(6) = 1
  if (len(e1) == 10 .and. e1 == cmp3) rslts(7) = 1
  if (e2 == cmp3) rslts(8) = 1

  call check(rslts, expect, n)
end program
