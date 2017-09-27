!* Copyright (c) 1999, NVIDIA CORPORATION.  All rights reserved.
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

!   passing nocontiguous section to subroutine

      program main

      implicit none

      integer xyz(2,4)
      integer result(4),expect(4)
      data expect/ 1,2,3,4/

      xyz(1,1) = 1
      xyz(1,2) = 2
      xyz(1,3) = 3
      xyz(1,4) = 4
      xyz(2,1) = 5
      xyz(2,2) = 6
      xyz(2,3) = 7
      xyz(2,4) = 8

      call junk(xyz(1,:), 4, result)

      call check(result,expect,4)

      end


      subroutine junk(abc, num, res)
      integer num,abc(num),res(4)
      res(:) = abc(1:4)
      end

