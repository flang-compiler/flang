!
** Copyright (c) 1989, NVIDIA CORPORATION.  All rights reserved.
**
** Licensed under the Apache License, Version 2.0 (the "License");
** you may not use this file except in compliance with the License.
** You may obtain a copy of the License at
**
**     http://www.apache.org/licenses/LICENSE-2.0
**
** Unless required by applicable law or agreed to in writing, software
** distributed under the License is distributed on an "AS IS" BASIS,
** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
** See the License for the specific language governing permissions and
** limitations under the License.

*   VMS end-of-line comments and debug statements.

      program ! continue . . .
c . . .
     +        ab10   ! end of program statement

      integer rslts(3), expect(3)!
	! (tab)
      data expect / 1, 2, 3 /!/('
 !      x
  !     x
   !    x
    !   x
      ! x
      rslts(1) = 1	!" 
D     rslts(1) = 10
D    !
      rslts(2) = 2
D9999 rslts(2) =
D    + 22 ! x
      
      rslts(3) = 3
      call check(rslts, expect, 3)
      !END
      end!
!
D
D     subroutine ss
D     end
D
      subroutine ss
      character c
      c = '!'
      i = 4h!!!!
      end
!  last line of file is a comment ...
