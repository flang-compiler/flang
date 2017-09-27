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

*   INTRINSIC statements.

C  Tests:
C   (1) generic in INTRINSIC stmt doesn't remove generic property.
C   (2) intrinsic in INTRINSIC stmt passed as argument.

	intrinsic iabs
	integer rslts(3), expect(3)
	intrinsic sngl, dprod, or, dfloat
	intrinsic int

        data expect / 2, 7, 7 /

	rslts(1) = int(2.9D0)

	call sub(iabs, ires)
	rslts(2) = ires

	call sub2(dprod, ires)
	rslts(3) = ires
	
	call check(rslts, expect, 3)
	end
CCCCC
	subroutine sub(ifunc, iout)
	iout = ifunc(-7)
	end
CCCCC
	subroutine sub2(dfunc, iout)
	double precision dfunc
	iout = dfunc(2.35E0, 3.0E0)
	end
