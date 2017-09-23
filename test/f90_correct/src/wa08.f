!* Copyright (c) 2000-2017, NVIDIA CORPORATION.  All rights reserved.
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
!
!	random_seed(size)
!	(Mantevo MiniGhost) - the write to the size argment may
!	write beyond the end of the argument when large arrays is enabled.
      program pp
      integer n
      common/nn/n,n0
      call random_seed(n)
!      print *, n
      call check(n0,97,1)
      end
      subroutine mycom
      common/nn/n,n0
      data n0/97/
      end
