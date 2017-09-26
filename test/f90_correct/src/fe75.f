C Copyright (c) 2005, NVIDIA CORPORATION.  All rights reserved.
C
C Licensed under the Apache License, Version 2.0 (the "License");
C you may not use this file except in compliance with the License.
C You may obtain a copy of the License at
C
C     http://www.apache.org/licenses/LICENSE-2.0
C
C Unless required by applicable law or agreed to in writing, software
C distributed under the License is distributed on an "AS IS" BASIS,
C WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
C See the License for the specific language governing permissions and
C limitations under the License.
C
C  PACK intrinsic with a scalar mask fails in 6.0 with -mcmodel=medium
C  Failure mode is a runtime error "0: PACK: invalid mask descriptor"
       integer*4 xo(5), xe(5)
       data xe/1, 2, 3, 4, 5/
       xo = pack(xe,.true.)
       call check(xe, xo, 5)
       end
