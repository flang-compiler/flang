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

*   Complex and real constants.

	complex expect(10), rslts(10), ccopy

	data expect / (1.0, 2.0), (0, 1), (-3.0, 4),
     +                (99999.1, -2.1), (2e+4, -1), (-1, 0),
     +                (2.0e-4, -2.5e-1), (-2e-1, -99999.1),
     +                2*(-2.5e1, 0.0)                      /

	rslts(1) = (1, 2)
	rslts(2) = - (-0.0, -1)
	rslts(3) = 1 * (-3, 4.e+0)
	rslts(4) = (999991e-1, -2.10000)
	rslts(5) = ((20000, -10e-1))

	rslts(6) = ccopy((-1, 00e00))
	rslts(7) = (.2e-003, -.25)
	rslts(8) = (-.02e+1, -99999.1)
	rslts(9) = (-00025, 0)
	rslts(10) = (-25., 0e0)

	call check(rslts, expect, 20)

	end

c -----
	complex function ccopy(c)
	complex c
	ccopy = c
	end
