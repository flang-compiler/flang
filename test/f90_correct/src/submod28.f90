!
! Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

! related to C1411 submodule specification-part must not contain format-stmt
! however, specification-part of a module procedure in a submodule may contain a
! format-stmt

module prettyprint
    double precision A, B, C
  interface 
    module subroutine niceprint(e,f,g)
    double precision, intent(in) :: e,f,g
    end subroutine niceprint
  end interface
  contains
    
end module prettyprint

submodule (prettyprint) niceprint
!   400 FORMAT('|','|',3(F8.3,'|'),'|')
contains
  module procedure niceprint
   400 FORMAT('|','|',3(F8.3,'|'),'|')
   500 FORMAT(6H PASS )
   write(*,400)e,f,g
   write(*,500)
   
  end procedure  
end submodule niceprint

program foo
use prettyprint
200 FORMAT(' ',3F7.2)
300 FORMAT('|',3(F8.3,'|'))
    A = 3.141592
    B = -11.2
    C = 12.34567E-02
write(*,200)A,B,C
write(*,300)A,B,C
write(*,300)B,C,A
write(*,300)C,A,B
call niceprint(A,B,C)

end program foo

