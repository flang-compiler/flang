!
! Copyright (c) 2014, NVIDIA CORPORATION.  All rights reserved.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
C	This tests directives
!	And also comments
*	Even if comments or directives begin with an astrisk
c	And also macro replacement within directives
	program p
	logical :: res(1) = .false., expect(1) = .true.

#define FOO      critical
#define FN(c, d) c d
#define FIN      end
#define STR(_x) #_x
#define CPY(_x) _x

!$omp FOO
	print *, CPY("Making a critical block! WooHoo!")
	print *, STR(Make statements parallel! WooHoo!)
C$omp end FOO

*$omp FOO
	print *, "Many moar things!"
c$omp FN(FIN,FOO)
	res(1) = .true.
	call check(res, expect, 1)
	end
