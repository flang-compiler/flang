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


! test specifier encoding on open and inqurire

program encoding_in_openinquire
 character*10 charme,formatme
 parameter(N=9)
 integer result(N),expect(N)
 data expect /-1,0,0,0,-1,0,0,0,-1/

! This test assume the TRUE value is -1

! test 1 open file with formated, encoding should be ok
open(28, file='output.file', action='write', form='formatted' , encoding='UTF-8')
inquire(file='output.file', formatted=formatme, encoding=charme)
result(1) = (charme == 'UTF-8')
result(2) = (charme == 'DEFAULT')
result(3) = (charme == 'UNDEFINED')

! open file with no encoding specifier
open(30, file='output3.file', action='write', form='formatted' )
inquire(file='output3.file', formatted=formatme, encoding=charme)
close(30)
result(4) = (charme == 'UTF-8')
result(5) = (charme == 'DEFAULT')
result(6) = (charme == 'UNKNOWN')

! inquire without opening  should be unknown
inquire(file='output3.file', formatted=formatme, encoding=charme)
result(7) = (charme == 'UTF-8')
result(8) = (charme == 'DEFAULT')
result(9) = (charme == 'UNKNOWN')

call check(result, expect, 9)

end
