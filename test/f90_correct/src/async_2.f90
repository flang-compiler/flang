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

!
      program prog

      implicit none

          character, dimension(8), asynchronous :: a1
          logical rslt(8), expect(8)
          integer :: i

          ! Open a file, This case is asynchronous
          open(7,FORM='unformatted',FILE='hello1.txt',ASYNCHRONOUS='yes',ACTION='read',ACCESS='stream')
          open(8,FORM='unformatted',FILE='hello2.txt',ASYNCHRONOUS='yes',ACTION='read',ACCESS='stream')

!----------------------------

! -- Read from the abcd file: sync, then async, then sync.

          read(7,ASYNCHRONOUS='no') a1

          do i = 1,8
            rslt(i) = 'a' .eq. a1(i)
            expect(i) = .true.
          enddo

          call check(rslt, expect, 8)

          read(7,ASYNCHRONOUS='yes') a1
          wait(7)

          do i = 1,8
            rslt(i) = 'b' .eq. a1(i)
          enddo

          call check(rslt, expect, 8)

          read(7,ASYNCHRONOUS='no') a1

          do i = 1,8
            rslt(i) = 'c' .eq. a1(i)
          enddo

          call check(rslt, expect, 8)

! -- Read from the 0123 file: async, then sync, then async.

          read(8,ASYNCHRONOUS='yes') a1
          wait(8)

          do i = 1,8
            rslt(i) = '0' .eq. a1(i)
          enddo

          call check(rslt, expect, 8)

          read(8,ASYNCHRONOUS='no') a1
          
          do i = 1,8
            rslt(i) = '1' .eq. a1(i)
          enddo

          call check(rslt, expect, 8)

          read(8,ASYNCHRONOUS='yes') a1
          wait(8)

          do i = 1,8
            rslt(i) = '2' .eq. a1(i)
          enddo

          call check(rslt, expect, 8)

      end program prog

