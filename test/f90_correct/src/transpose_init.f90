!** Licensed under the Apache License, Version 2.0 (the "License");
!** you may not use this file except in compliance with the License.
!** You may obtain a copy of the License at
!**
!**     http://www.apache.org/licenses/LICENSE-2.0
!**
!** Unless required by applicable law or agreed to in writing, software
!** distributed under the License is distributed on an "AS IS" BASIS,
!** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!** See the License for the specific language governing permissions and
!** limitations under the License.

!* Test checking tranpose during initialization
program test
      integer, parameter :: num = 1
      integer rslts(num), expect(num)
      data expect / 1 /
      integer, parameter :: arr(2, 3) = RESHAPE((/1, 2, 3, 4, 5, 6/), (/2, 3/))
      integer, parameter :: exp_transpose_arr(3, 2) = RESHAPE((/1, 3, 5, 2, 4, 6/), (/3, 2/))
      integer :: transpose_arr(3, 2) = TRANSPOSE(arr)

      if (all(transpose_arr .eq. exp_transpose_arr)) then
          rslts(1) = 1
      else
          rslts(1) = 0
          print *, 'tranpose_arr vs exp_transpose_arr mismatch'
      endif

      call check(rslts, expect, num)
end program
