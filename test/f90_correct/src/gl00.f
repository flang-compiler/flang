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

*   Logical operators (.NOT., .AND., .OR., .EQV., and .NEQV.)
*   used outside of IF conditions.

      implicit logical (r, e-f, t)
      parameter (N = 36, TT = .true., FF = .false.)
      common f
      dimension rslts(N), expect(N)
      data t, f / .true., .false. /

      data expect /
c tests 1 - 4:
     +			FF, TT, FF, TT,
c tests 5 - 12:
     +			FF, FF, FF, TT, FF, FF, FF, TT,
c tests 13 - 20:
     +			FF, TT, TT, TT, FF, TT, TT, TT,
c tests 21 - 24:
     +			TT, FF, FF, TT,
c tests 25 - 28:
     +			FF, TT, TT, FF,
c tests 29 - 33:
     +			TT, FF, TT, TT, FF,
c tests 34 - 36:
     +			TT, FF, FF          /

C   tests 1 - 4:   .NOT. operator:

	rslts(1) = .NOT. t
	rslts(2) = .not. f
	rslts(3) = .not. .TRUE.
	rslts(4) = .not..FALSE.

C   tests 5 - 12:   .AND. operator:

	rslts(5) = f .and. f
	rslts(6) = f .and. t
	rslts(7) = t .and. f
	rslts(8) = t .and. t

c       ... one constant operand:

	rslts(9) = f .and. .false.
	rslts(10) = f .and. .true.
	rslts(11) = .true. .and. f
	rslts(12) = .true. .and. t

C   tests 13 - 20:   .OR. operator:

	rslts(13) = f .or. f
	rslts(14) = f .or. t
	rslts(15) = t .or. f
	rslts(16) = t .or. t

c       ... one constant operand:

	rslts(17) = .false. .or. f
	rslts(18) = f .or. .true.
	rslts(19) = .true. .or. f
	rslts(20) = t .or. .true.

C   tests 21 - 24:   .EQV. operator:

	rslts(21) = f .eqv. f
	rslts(22) = f .eqv. t
	rslts(23) = t .eqv. .false.
	rslts(24) = .true. .eqv. t

C   tests 25 - 28:   .NEQV operator:

	rslts(25) = .false. .neqv. f
	rslts(26) = f .neqv. t
	rslts(27) = t .neqv. f
	rslts(28) = t .neqv. .true.

C   tests 29 - 33:   test precedence of logical operators:

	rslts(29) = .not. t .or. t
	rslts(30) = .not. f .and. f
	rslts(31) = t .or. t .and. f
	rslts(32) = f .eqv. f .and. f
	rslts(33) = t .neqv. f .or. t

C   tests 34 - 36:   test .not. of .not.:

        rslts(34) = .not. .not. t
        rslts(35) = .not. .not. f
        rslts(36) = .not. .not. .not. t

C   check results:

	call check(rslts, expect, N)
	end
