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

! just do recursive io calls to check that it does not hangs.

         function foo2(i)
             integer foo2,i
             foo2 = i
             print *, i
         end

         function foo(i)
           integer foo,foo2,i
           integer omp_get_thread_num, omp_get_num_threads
           character*20 writetome
           i = omp_get_thread_num()
           print *, i, foo2(i)
           foo=i+3
         end

         program mm
         integer a,b,foo,numme
         character*20 writetome
         integer omp_get_thread_num, omp_get_num_threads
         integer i
         a=1
         b=4
         i=2
!$omp parallel
         print *, "thread num:",omp_get_thread_num(),"num_thread:",omp_get_num_threads()
         write(writetome, '(i2)') foo(i)
!$omp end parallel


! if it gets here, then it is OK.  No hang.
         print *, "PASS" 

         end
