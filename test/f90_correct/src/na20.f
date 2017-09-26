
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

**--- INQUIRE of preconnected units

      parameter (N=2)
      character*10 buf
      integer result(N), expect(N)
      logical is_open

      INQUIRE(unit=5, opened=is_open)
      result(1) = and(is_open, 1)
      INQUIRE(unit=6, opened=is_open)
      result(2) = and(is_open, 1)

      data expect/1, 1/
      call check(result, expect, N)
      end
