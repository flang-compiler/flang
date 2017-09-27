
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

**--- BZ edit descriptor

      parameter (N=2)
      common ires(N), iexp(N)
      ires(1) = itest1()	! leading blanks
      ires(2) = itest2()	! leading and embedded blanks

      data iexp/-1, -1010/
      call check(ires, iexp, N)
      end
      integer function itest1()
      character *5 ch
      data ch /'  -1'/
      read (ch, 100) itest1
 100  format (BZ,I4)
      return
      end
      integer function itest2()
      character *6 ch
      data ch /' -1 1 '/
      read (ch, 100) itest2
 100  format (BZ,I6)
      return
      end
