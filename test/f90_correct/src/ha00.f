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

*   STOP and PAUSE statements.

C    check that STOP and PAUSE statements can be compiled ok, but
C    it's not convenient to check that they actually work ok.

      program p
      data i /7/

      if (i .eq. 0) then
          pause
          PAUSE 0
          pause 00000
          pause 99999
          pause 'Hello'
          stop
      else if (i .lt. 7) then
          stop 1
      elseif  (i .gt. 7) then
          stop 'Fairly long message string'
      else
          call check(77, 77, 1)
      endif

      stop 'this message may or may not be printed'
      end
