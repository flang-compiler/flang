! 
! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

!          THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT
!   WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT
!   NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR
!   FITNESS FOR A PARTICULAR PURPOSE.
!


! iso_fortran_env.f90
! 32/64 bit  linux and windows.  Add further targets as required.

        module ISO_FORTRAN_ENV

	public

	integer CHARACTER_STORAGE_SIZE
	parameter (CHARACTER_STORAGE_SIZE = 8)
	integer ERROR_UNIT
	parameter (ERROR_UNIT = 0)
	integer FILE_STORAGE_SIZE
	parameter (FILE_STORAGE_SIZE = 8)
	integer INPUT_UNIT
	parameter (INPUT_UNIT = 5)
	integer IOSTAT_END
	parameter (IOSTAT_END = -1)
	integer IOSTAT_EOR
	parameter (IOSTAT_EOR = -2)
	integer NUMERIC_STORAGE_SIZE
	parameter (NUMERIC_STORAGE_SIZE = 32)
	integer OUTPUT_UNIT
	parameter (OUTPUT_UNIT = 6)

        integer IOSTAT_INQUIRE_INTERNAL_UNIT
        parameter (IOSTAT_INQUIRE_INTERNAL_UNIT=99)
    
	integer INT8
	parameter (INT8 = 1)
	integer INT16
	parameter (INT16 = 2)
	integer INT32
	parameter (INT32 = 4)
	integer INT64
	parameter (INT64 = 8)
	integer LOGICAL8
	parameter (LOGICAL8 = 1)
	integer LOGICAL16
	parameter (LOGICAL16 = 2)
	integer LOGICAL32
	parameter (LOGICAL32 = 4)
	integer LOGICAL64
	parameter (LOGICAL64 = 8)
	integer REAL32
	parameter (REAL32 = 4)
	integer REAL64
	parameter (REAL64 = 8)
	integer REAL128
	parameter (REAL128 = -1)

        integer INTEGER_KINDS(4)
        parameter (INTEGER_KINDS = (/INT8, INT16, INT32, INT64/))
        integer LOGICAL_KINDS(4)
        parameter (LOGICAL_KINDS = (/LOGICAL8, LOGICAL16, LOGICAL32, LOGICAL64/))
        integer REAL_KINDS(2)
        parameter (REAL_KINDS = (/REAL32, REAL64/))

        end module  ISO_FORTRAN_ENV

