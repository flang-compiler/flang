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


         function foo(i, writetome, numme)
           integer foo,i,numme
           integer omp_get_thread_num, omp_get_num_threads
           character*20 writetome
! it should overwrite the first position or value of 'a' that a caller has already wrote
           write(writetome, '(i1)') omp_get_thread_num()
           numme = omp_get_thread_num()
           foo=i+3
         end

         program mm
         use omp_lib
         integer a,b,foo,numme
         character*20 writetome
         a=1
         b=4
         call omp_set_num_threads(9)
!$omp parallel
         write(writetome, '(i1, i1, i1)') a,foo(2,writetome,numme),b
!$omp end parallel
!         print *, writetome,numme
! 'numme' should have the same value as the first value in writetome

         read(writetome, '(i1)') b      

         if (numme .eq. b) then
             print *, "PASS" 
         else
             print *, "FAIL"
         end if

         end
