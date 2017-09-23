
!* Copyright (c) 2000, NVIDIA CORPORATION.  All rights reserved.
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* Unless required by applicable law or agreed to in writing, software
!* distributed under the License is distributed on an "AS IS" BASIS,
!* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!* See the License for the specific language governing permissions and
!* limitations under the License.
!
!  Test entity oriented variable initializations
!

module m
 INTEGER, parameter :: intParam1 = 2
 !INTEGER, parameter :: intParam2 = intParam1+3
 INTEGER, parameter :: intParamArr4(2)= (/3,4/)
 INTEGER, parameter :: intParamArr5(2)= intParamArr4

 INTEGER :: int1
 INTEGER :: intarr1(2)

 type t1
   integer :: i
   integer :: j
   integer :: k
 end type
 type (t1):: t1_inst
 type (t1), parameter :: t1_param1 = t1(1,2,3)
 type (t1), parameter :: t1_param2 = t1_param1
 type (t1), parameter :: t1_param_array(3) = (/t1(1,2,3), t1(2,3,4), t1(3,4,5)/)

 data int1,intarr1,t1_inst / 18, 81, 82, t1(1,2,3) /
end module m


SUBROUTINE module_test(strt_idx, result)
  use m

 INTEGER :: strt_idx 
 INTEGER, dimension(:) :: result 

 type t2
   integer :: i
   integer :: j
   integer :: k
 end type
 type (t2):: t2_inst = t2(6,7,8)
!
 result(strt_idx) = intParam1
 result(strt_idx+1:strt_idx+2) = intParamArr4
 result(strt_idx+3:strt_idx+4) = intParamArr5
 result(strt_idx+5) = int1
 result(strt_idx+6:strt_idx+7) = intarr1
 result(strt_idx+8) = t1_inst%i
 result(strt_idx+9) = t1_inst%j
 result(strt_idx+10) = t1_inst%k
 result(strt_idx+11) = t2_inst%i
 result(strt_idx+12) = t2_inst%j
 result(strt_idx+13) = t2_inst%k

END SUBROUTINE


PROGRAM wa04
!
 PARAMETER (N=241)

 INTEGER, dimension(N) :: result, expect

 INTERFACE
   SUBROUTINE module_test( strt_idx, result)
     INTEGER :: strt_idx
     INTEGER, dimension(:) :: result
   END SUBROUTINE
 END INTERFACE

 data expect / &
! intParam1
  2, &
! intParam2
  5, &
! int8Param1
   16, &
! intParamArr1
  6,6,6,6,6,6,6,6,6,6, &
! intParamArr2
  3,3,3,3,3,3,3,3,3,3, &
! intParamArr3
  3,4, &
! intParamArr4
  3,4, &
! intParamArr5
  6,8, &
! intParamArr6
  12,12,12,12,12,12,12,12,12,12, &
! int1
  2, &
! int2
  15, &
! int3
  3,4, &
! int4
  33, &
! intArr1
  5,6, &
! intArr2
  6,8, &
! intArr3
  3,5,7,9,11, &
! intArr4
  2,3,7,8,9, &
! intArr5
  6,6,6,6,6, &
! intArr6
  6,8, &
! intArr7
  2,2,2,2,2,2,2,2,2,2, &
! intArr8
  1,2,3,4,5,6,7,8,9,10, &
! intArr9
  20,20,20,20,20,20,20,20,20,20, &
! intArr10
  3,4, &
! intArr11
  -2,-2,-2,-2,-2,-2,-2,-2,-2,-2, &
! intArr12
  51,42,33,24,15, &
! t1_param1
  1,2,3, &
! t1_param2
  1,2,3, &
! t1_param_array
  1,2,3,2,3,4,3,4,5, &
! t1_inst1
  2,2,3, &
! t1_inst2
  1,2,3, &
! t1_array
  1,2,3,2,3,4,3,4,5, &
! t2param1
  4,51,42,33,24,15, &
! t2inst1
  1,1,2,3,4,5, &
! t2inst2
  2,3,4,5,6,7, &
! t2inst3
  2,51,42,33,24,15, &
! t3inst1
  2,4,2,0, &
! t3inst2
  2,1,2,3, &
! t4inst1
  2,4,2,0,8, &
! t4inst2
  2,1,2,3,7, &
! t4inst3
  2,1,2,3,5, &
! t5inst1
  4,2,0,2, &
! t5inst2
  1,2,3,2, &
! t6Param1
  4,51,42,33,24,15,2, &
! t3inst3
  1,1,2,3, &
! t3inst4
  6,1,2,3, &
! t7inst1
  1,2,3, &
! t6inst1
  4,51,42,33,24,15,1, &
! FROM module_test
! module m: intParam1
  2, &
! module m: intParamArr4
  3,4, &
! module m: intParamArr5
  3,4, &
! module m: int1
  18, &
! module m: int2
  81,82, &
! module m: t1_inst
  1,2,3, &
!module m:  t2_inst
  6,7,8, &
!t8_param
  3,4,5,6,7 /

 INTEGER, parameter :: intParam1 = 2
 INTEGER, parameter :: intParam2 = intParam1+3
 INTEGER*8, parameter :: int8Param1 = 16
 INTEGER, parameter :: intParamArr1(10) = 6
 INTEGER, parameter :: intParamArr2(10)= (/(3,i=1,10)/)
 INTEGER, parameter :: intParamArr3(2)= (/3,4/)
 INTEGER, parameter :: intParamArr4(2)= (/3.0,4.0/)
 INTEGER, parameter :: intParamArr5(2)= (/3.0,4.0/)*2
 INTEGER, parameter :: intParamArr6(10)= intParamArr1*2

 INTEGER :: int1 = intParam1
 INTEGER :: int2 = (intParam1+1)*intParam2
 INTEGER :: int3(2)=  intParamArr3

 INTEGER :: intArr1(2)= (/5,6/)
 INTEGER :: intArr2(2) = (/ intParamArr3*2 /)
 INTEGER :: intArr3(5)  = (/ (iii+2,iii=1,10,2) /)
 INTEGER :: intArr4(5)  = (/ intParam1, 3, 7, 8 ,9/)
 INTEGER :: intArr5(5)  = (/ intParamArr1(2:6) /)
 INTEGER :: intArr6(2) = ((/1,2/)+2)*intParam1
 INTEGER :: intArr7(10) = intParam1
 INTEGER :: intArr8(10)= (/(iii,iii=1,10)/)
 INTEGER :: intArr9(10) = 10*intparam1
 INTEGER :: intArr10(2) = (/1,2/)+2
 INTEGER :: intArr11(10) = -intparam1
!
 type t1
    integer :: i
    integer :: j
    integer :: k
 end type
 type (t1), parameter :: t1_param1 = t1(1,2,3)
 type (t1), parameter :: t1_param2 = t1_param1
 type (t1), parameter :: t1_param_array(3) = (/t1(1,2,3), t1(2,3,4), t1(3,4,5)/)
 
 type (t1) :: t1_inst1 = t1(intParam1,2,3)
 type (t1) :: t1_inst2 = t1_param1;
 type (t1) :: t1_array(3) = t1_param_array
!
 type t2
    integer :: i
    integer :: iary(1:5)
 end type
!
 type (t2), parameter :: t2param1 = t2(4, (/51,42,33,24,15/))
 type (t2) :: t2inst1 = t2(1, (/1,2,3,4,5/))
 type (t2) :: t2inst2 = t2(intParam1, (/(i+2,i=1,5)/) )
 type (t2) :: t2inst3 = t2(intParam1, t2param1%iary )
 INTEGER :: int4 = t2param1%iary(3)		
!
 type t3
    integer :: j
    type(t1) :: t1_inst
 end type
!
 type(t3) :: t3inst1 = t3(intParam1, t1(4,2,0) )
 type(t3) :: t3inst2 = t3(intParam1, t1_param1)
 type(t3) :: t3inst3 = t3(t1_param1%i, t1_param1)
 type(t3) :: t3inst4 = t3(intParamArr1(2:2), t1_param1)
!
 type t4
    integer :: j
    type(t1) :: t1_inst
    integer :: k
 end type
!
 type(t4) :: t4inst1 = t4(intParam1, t1(4,2,0), 8)
 type(t4) :: t4inst2 = t4(intParam1, t1_param1, 7)
 type(t4) :: t4inst3 = t4(intParam1, t1_param1, intParam2)
!
 type t5
    type(t1) :: t1_inst
    integer :: j
 end type
!
 type(t5) :: t5inst1 = t5(t1(4,2,0), intParam1)
 type(t5) :: t5inst2 = t5(t1_param1, intParam1)
!
 type t6
    type(t2) :: t2_inst
    integer :: j
 end type
!
 type(t6), parameter :: t6Param1 = t6(t2(4, (/51,42,33,24,15/)) ,2)
 type(t6) :: t6inst1 = t6(t6Param1%t2_inst,1)
 INTEGER :: intArr12(1:5) = t6Param1%t2_inst%iary
!
 type t7
    type(t1) :: t1_inst
 end type
 type(t7) :: t7inst1 = t7(t1_param1)

 type t8
   integer :: t8_intArr(5)
 end type
 type(t8), parameter :: t8_param = t8( (/1,2,3,4,5/) + 2 )



!
 result(1) = intParam1
 result(2) = intParam2
 result(3) = int8Param1
 result(4:13) = intParamArr1
 result(14:23) = intParamArr2
 result(24:25 ) = intParamArr3
 result(26:27) = intParamArr4
 result(28:29) = intParamArr5
 result(30:39) = intParamArr6
 result(40) = int1
 result(41) = int2
 result(42:43) = int3
 result(44) = int4
 result(45:46) = intArr1
 result(47:48) = intArr2
 result(49:53) = intArr3
 result(54:58) = intArr4
 result(59:63) = intArr5
 result(64:65) = intArr6
 result(66:75) = intArr7
 result(76:85) = intArr8
 result(86:95) = intArr9
 result(96:97) = intArr10
 result(98:107) = intArr11
 result(108:112) = intArr12
 result(113) = t1_param1%i
 result(114) = t1_param1%j
 result(115) = t1_param1%k
 result(116) = t1_param2%i
 result(117) = t1_param2%j
 result(118) = t1_param2%k
 result(119) = t1_param_array(1)%i
 result(120) = t1_param_array(1)%j
 result(121) = t1_param_array(1)%k
 result(122) = t1_param_array(2)%i
 result(123) = t1_param_array(2)%j
 result(124) = t1_param_array(2)%k
 result(125) = t1_param_array(3)%i
 result(126) = t1_param_array(3)%j
 result(127) = t1_param_array(3)%k
 result(128) = t1_inst1%i
 result(129) = t1_inst1%j
 result(130) = t1_inst1%k
 result(131) = t1_inst2%i
 result(132) = t1_inst2%j
 result(133) = t1_inst2%k
 result(134) = t1_array(1)%i
 result(135) = t1_array(1)%j
 result(136) = t1_array(1)%k
 result(137) = t1_array(2)%i
 result(138) = t1_array(2)%j
 result(139) = t1_array(2)%k
 result(140) = t1_array(3)%i
 result(141) = t1_array(3)%j
 result(142) = t1_array(3)%k
 result(143) = t2param1%i
 result(144) = t2param1%iary(1)
 result(145) = t2param1%iary(2)
 result(146) = t2param1%iary(3)
 result(147) = t2param1%iary(4)
 result(148) = t2param1%iary(5)
 result(149) = t2inst1%i
 result(150) = t2inst1%iary(1)
 result(151) = t2inst1%iary(2)
 result(152) = t2inst1%iary(3)
 result(153) = t2inst1%iary(4)
 result(154) = t2inst1%iary(5)
 result(155) = t2inst2%i
 result(156) = t2inst2%iary(1)
 result(157) = t2inst2%iary(2)
 result(158) = t2inst2%iary(3)
 result(159) = t2inst2%iary(4)
 result(160) = t2inst2%iary(5)
 result(161) = t2inst3%i
 result(162) = t2inst3%iary(1)
 result(163) = t2inst3%iary(2)
 result(164) = t2inst3%iary(3)
 result(165) = t2inst3%iary(4)
 result(166) = t2inst3%iary(5)
 result(167) = t3inst1%j
 result(168) = t3inst1%t1_inst%i
 result(169) = t3inst1%t1_inst%j
 result(170) = t3inst1%t1_inst%k
 result(171) = t3inst2%j
 result(172) = t3inst2%t1_inst%i
 result(173) = t3inst2%t1_inst%j
 result(174 ) = t3inst2%t1_inst%k
 result(175) = t4inst1%j
 result(176) = t4inst1%t1_inst%i
 result(177) = t4inst1%t1_inst%j
 result(178) = t4inst1%t1_inst%k
 result(179) = t4inst1%k
 result(180) = t4inst2%j
 result(181) = t4inst2%t1_inst%i
 result(182) = t4inst2%t1_inst%j
 result(183) = t4inst2%t1_inst%k
 result(184) = t4inst2%k
 result(185) = t4inst3%j
 result(186) = t4inst3%t1_inst%i
 result(187) = t4inst3%t1_inst%j
 result(188) = t4inst3%t1_inst%k
 result(189) = t4inst3%k
 result(190) = t5inst1%t1_inst%i
 result(191) = t5inst1%t1_inst%j
 result(192) = t5inst1%t1_inst%k
 result(193) = t5inst1%j
 result(194) = t5inst2%t1_inst%i
 result(195) = t5inst2%t1_inst%j
 result(196) = t5inst2%t1_inst%k
 result(197) = t5inst2%j
 result(198) = t6Param1%t2_inst%i
 result(199) = t6Param1%t2_inst%iary(1)
 result(200) = t6Param1%t2_inst%iary(2)
 result(201) = t6Param1%t2_inst%iary(3)
 result(202) = t6Param1%t2_inst%iary(4)
 result(203) = t6Param1%t2_inst%iary(5)
 result(204) = t6Param1%j
 result(205) = t3inst3%j
 result(206) = t3inst3%t1_inst%i
 result(207) = t3inst3%t1_inst%j
 result(208) = t3inst3%t1_inst%k
 result(209) = t3inst4%j
 result(210) = t3inst4%t1_inst%i
 result(211) = t3inst4%t1_inst%j
 result(212) = t3inst4%t1_inst%k
 result(213:) = t7inst1%t1_inst%i
 result(214:) = t7inst1%t1_inst%j
 result(215:) = t7inst1%t1_inst%k
 result(216) = t6inst1%t2_inst%i
 result(217) = t6inst1%t2_inst%iary(1)
 result(218) = t6inst1%t2_inst%iary(2)
 result(219) = t6inst1%t2_inst%iary(3)
 result(220) = t6inst1%t2_inst%iary(4)
 result(221) = t6inst1%t2_inst%iary(5)
 result(222) = t6inst1%j

 call module_test(223,result)
 result(237:241) = t8_param%t8_intArr;
!
 call check(result, expect, N);

end program
