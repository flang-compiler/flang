! Copyright (c) 2013, NVIDIA CORPORATION.  All rights reserved.
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
! Tests F2003 defined I/O (recursive read)

module person_module
 logical rslt(10), expect(10)
 integer :: cnt
 type :: person
  character(len=20) :: name
  integer :: age
 contains
  procedure :: my_read => rf
  procedure :: my_write => wf
  generic :: READ(FORMATTED) => my_read
  generic :: WRITE(FORMATTED) => my_write
 end type
 type, extends(person) :: employee
   integer id
   real salary
   contains
   procedure :: my_read => rf2
   procedure :: my_write => wf2
 end type
 contains

 recursive subroutine rf(dtv, unit, iotype, vlist, iostat, iomsg)
 class(person), intent(inout) :: dtv
 integer, intent(in) :: unit
 character(len=*),intent(in) :: iotype
 integer, intent(in) :: vlist(:)
 integer, intent(out) :: iostat
 character (len=*), intent(inout) :: iomsg
 
 character(len=9) :: pfmt
 
 read (unit, *, iostat=iostat) dtv%name, dtv%age
 if (iostat .eq. 0) then
   cnt = cnt + 1
   rslt(cnt) = dtv%age .eq. 40+(cnt-1) 
   read(unit, *) dtv
 endif
 
 end subroutine

subroutine wf(dtv, unit, iotype, vlist, iostat, iomsg)
 class(person), intent(inout) :: dtv
 integer, intent(in) :: unit
 character(len=*),intent(in) :: iotype
 integer, intent(in) :: vlist(:)
 integer, intent(out) :: iostat
 character (len=*), intent(inout) :: iomsg

 character(len=9) :: pfmt

 write (unit, *) dtv%name, dtv%age
 end subroutine

 subroutine wf2(dtv, unit, iotype, vlist, iostat, iomsg)
 class(employee), intent(inout) :: dtv
 integer, intent(in) :: unit
 character(len=*),intent(in) :: iotype
 integer, intent(in) :: vlist(:)
 integer, intent(out) :: iostat
 character (len=*), intent(inout) :: iomsg

 character(len=9) :: pfmt

 write (unit, *) dtv%name, dtv%age, dtv%id, dtv%salary
 end subroutine

 recursive subroutine rf2(dtv, unit, iotype, vlist, iostat, iomsg)
 class(employee), intent(inout) :: dtv
 integer, intent(in) :: unit
 character(len=*),intent(in) :: iotype
 integer, intent(in) :: vlist(:)
 integer, intent(out) :: iostat
 character (len=*), intent(inout) :: iomsg

 character(len=9) :: pfmt

 read (unit, *, iostat=iostat) dtv%name, dtv%age, dtv%id, dtv%salary
 if (iostat .eq. 0) then
   cnt = cnt + 1
   rslt(cnt) = dtv%id .eq. 100+(cnt-1)
   read(unit, *) dtv
 endif

 end subroutine




end module

 use person_module
 integer id, members
 type(employee) :: chairman

 chairman%name='myname'
 chairman%age=40
 chairman%id = 100
 chairman%salary = 0

 rslt = .false.
 expect = .true.

 open(11, file='io16.output', status='replace')
 do i=1,10
   write(11, *) chairman
   chairman%id = chairman%id + 1
 enddo

 cnt = 0
 open(11, file='io16.output', position='rewind')
 read(11, *)  chairman
 
 close(11)

 call check(rslt, expect, 10)

 end


