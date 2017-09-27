!* Copyright (c) 2004, NVIDIA CORPORATION.  All rights reserved.
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
!*
!* Tests that the random number generator does not return zeros
!* This occurs for all releases before 5.1-6 when using -r8

PROGRAM P
IMPLICIT NONE  

! - - - local variables - - -
!
integer, parameter :: N=3
integer, parameter :: NT=1
REAL*8 :: pool(N,N,N) ! random number pool
INTEGER :: i
INTEGER, dimension(NT) :: exp, res


CALL RANDOM_SEED()               ! set seed to random number based on time
CALL RANDOM_NUMBER(pool)        ! fill pool with random data ( 0. -> 1. )

exp(1) = 3
res(1) = 0
DO i = 1,N
   IF (pool(i,i,i) > 0) THEN
      res(1) = res(1) + 1
   ELSE
      WRITE (*,*) 'Random numbers should not be zeros'
      WRITE (*,*) '---- pool(1:3,1:3,i) i=', i
      WRITE (*,*) pool(1:3,1:3,i)
   ENDIF
END DO

call check(res, exp, NT)

END PROGRAM P


