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


 MODULE mo_input_request_list

 USE ISO_C_BINDING, ONLY: C_CHAR, C_SIGNED_CHAR, C_INT, C_LONG, C_DOUBLE, C_NULL_PTR, C_NULL_CHAR, C_ASSOCIATED

 IMPLICIT NONE

 INTEGER, PARAMETER :: SUCCESS = 0

 TYPE :: t_DomainData
 INTEGER :: domain
 TYPE(t_DomainData), POINTER :: next
 END TYPE t_DomainData

 TYPE :: t_ListEntry
 CHARACTER(KIND = C_CHAR), POINTER :: iconVarName(:)
 CHARACTER(KIND = C_CHAR), POINTER :: translatedVarName(:)
 TYPE(t_DomainData), POINTER :: domainData
 END TYPE t_ListEntry

 TYPE t_patch
 !
 ! domain ID of current domain
 INTEGER :: id
 END TYPE t_patch

 CHARACTER(*), PARAMETER :: modname = "mo_input_request_list"


 CONTAINS


 FUNCTION findDomainData(listEntry, domain, opt_lcreate) RESULT(RESULT)
 TYPE(t_ListEntry), POINTER, INTENT(INOUT) :: listEntry
 INTEGER, VALUE :: domain
 LOGICAL, OPTIONAL, VALUE :: opt_lcreate
 TYPE(t_DomainData), POINTER :: RESULT

! CHARACTER(*), PARAMETER :: routine = modname//":findDomainData"
 INTEGER :: error

 IF(PRESENT(opt_lcreate)) THEN
   if (opt_lcreate .eq. .FALSE.) then
     print *, "FAIL"
     return
   end if

   if (domain .ne. 3) then
     print *, "FAIL"
     return 
   end if
   print *, "PASS"
   return
 ENDIF
 IF(.NOT.ASSOCIATED(listEntry)) print *, "assertion failed, listEntry IS NOT ASSOCIATED"

 ! Try to find a preexisting DomainData object.
 RESULT => listEntry%domainData
 DO
 IF(.NOT.ASSOCIATED(RESULT)) EXIT
 IF(RESULT%domain == domain) RETURN
 RESULT => RESULT%next
 END DO

 ! Nothing preexisting found, should we create a new one?
 IF(PRESENT(opt_lcreate)) THEN
 IF(opt_lcreate) THEN
 ALLOCATE(RESULT, STAT = error)
 IF(error /= SUCCESS) print *, "error allocating memory"
 RESULT%domain = domain
 RESULT%next => listEntry%domainData
 listEntry%domainData => RESULT
 END IF
 END IF
 END FUNCTION findDomainData


 SUBROUTINE some_foo(p_patch)

 TYPE(t_patch), INTENT(IN) :: p_patch

! CHARACTER(*), PARAMETER :: routine = modname//":InputRequestList_readFile"
 TYPE(t_ListEntry), POINTER :: listEntry
 TYPE(t_DomainData), POINTER :: domainData

 domainData => findDomainData(listEntry, p_patch%id, opt_lcreate = .TRUE.)

 END SUBROUTINE some_foo


 END MODULE mo_input_request_list



  use mo_input_request_list
  TYPE(t_patch) :: p_patch
  p_patch%id = 3
  call some_foo(p_patch)
  end
