!
! Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!

subroutine nlme(abc,def,l1,u1,u2)

! adjustable array in namelist
integer l1,u1,u2
integer abc(u1,u1)
integer def(u2,u2)

integer result(9)
integer expect(9)
character*20 output(10)
character*185 input
character*185 input2
character*185 output2

namelist/mygroup2/ def,abc

expect(1:2)=abc(:,1)
expect(3:4)=abc(:,2)
expect(5:6)=def(:,1)
expect(7:8)=def(:,2)
expect(9)=-1

!internal write
write(unit=output,  nml=mygroup2)
output2=' '
do i = 1, 10
    output2 = TRIM(output2)//TRIM(output(i))
end do

!internal read
input=" &MYGROUP2 DEF =            1,                  1,                  1,                  1, ABC =            1,                  2,                  3,                  4 /"
abc=0
def=0
read(unit=input,  nml=mygroup2)
result(1:2)=abc(:,1)
result(3:4)=abc(:,2)
result(5:6)=def(:,1)
result(7:8)=def(:,2)
result(9)=(output2 .eq. input)

call check(result,expect,9)

end 

program tt
integer abc(2,2)
integer def(2,2)

abc=1.2
abc(1,1)=1
abc(2,1)=2
abc(1,2)=3
abc(2,2)=4

def=1.3
call nlme(abc,def,1,2,2)

end
