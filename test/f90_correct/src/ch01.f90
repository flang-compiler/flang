! Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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
!  need to allocate temp of dynamic size for the call when
!  concatenating two assumed-length character dummy variables
!
      subroutine something(a)
      character*(*) a
      integer result(0:10)
      integer expect(0:10)
      data expect/10,116,104,105,115,32,116,104,97,116,32/

      result(0) = len(a)
      do i = 1,min(10,len(a))
       result(i) = ichar(a(i:i))
      enddo
      !print *,result
      call check(result,expect,11)
      end
      subroutine concat(c1,c2)
      character(*) :: c1, c2
      call something(c1//c2)
      end

      call concat( 'this',' that ')
      end
