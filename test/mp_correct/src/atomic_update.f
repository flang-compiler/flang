!* Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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

c	Simple OpenMP Parallel Region

	program p
	call t1
	end

	subroutine t1
	 integer atomic, res
	 integer act(2), expt(2)
	 atomic = 0
	 res = 0
c$omp	parallel shared(atomic, res)
c$omp	atomic update
	 atomic = atomic + 1

c$omp	end parallel

	 expt(1) = 2
	 act(1) = atomic
	 call check(act, expt, 1)

	end
