!* Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
!* See https://llvm.org/LICENSE.txt for license information.
!* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!
!	check correct creation of array parameter constructor
program main
  implicit none
  integer,parameter :: kind_param = 4
  integer(kind_param),parameter :: x = Z"FF"
  integer(kind_param) :: n
  integer(kind_param),parameter :: a(*) = [ Z"d76aa478", Z"e8c7b756"]
  enum, bind(c)
     enumerator :: param_foo = Z"2000" ! in C, 0x0200
     enumerator :: param_bar = Z"4000" ! in C, 0x4000
  end enum
  integer :: buffer = 5000, switch = 2

   n = O"034"
   print *, "n = ",n

   if (buffer > Z"3F") then
      print *, "Buffer is bigger"
   end if

   select case (switch)
      case (Z"01")
         print *, "bad select case branch"
         print *, "FAIL"
      case (Z"02")
         print *, "Good select case branch"
         print *, "PASS"
   end select

 end program main
