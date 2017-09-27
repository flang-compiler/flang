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

*   Character string constants.

      program p
      parameter(n = 30)
      integer rslts(n), expect(n)

      character ch*14, ch2*10

c  ---- set up expected array:

      data expect / 10, 9, 8, 12, 13, 0, 39,
     +              34, 92, 97, 255, 34, 39, 65,
     +              10, 39, 39, 34, 34,
     +              65, 1, 56, 47, 44,
     +              203, 1, 1, 6, 8, 2           /

c  ---- tests 1 - 14:

      data ch /'\n\t\b\f\r\0\'\"\\\a\377"''A'/

      do 10 i = 1, 14
10        rslts(i) = ichar( ch(i:i) )

c  ---- tests 15 - 24:

      data ch2 / "\n''""""A\001\070/," /

      do 20 i = 1, 10
20        rslts(14 + i) = ichar( ch2(i:i) )

c  ---- tests 25 - 30:

      rslts(25) = LEN('456789 123456789 123456789 123456789  23456789 1
     +
     + 456789        89 123456789 123456789 123456789 123456789 12345
     +9 123456789 123456789 ')

      rslts(26) = LEN(' ')
      rslts(27) = LEN('''')
      rslts(28) = ichar( '\6' )
      rslts(29) = ichar('\10')
      rslts(30) = len( '\0123' )

c  ---- check results:

      call check(rslts, expect, n)
      end 
