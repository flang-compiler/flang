!*** Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
!***
!*** Licensed under the Apache License, Version 2.0 (the "License");
!*** you may not use this file except in compliance with the License.
!*** You may obtain a copy of the License at
!***
!***     http://www.apache.org/licenses/LICENSE-2.0
!***
!*** Unless required by applicable law or agreed to in writing, software
!*** distributed under the License is distributed on an "AS IS" BASIS,
!*** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!*** See the License for the specific language governing permissions and
!*** limitations under the License.



! Write a sequence of values with DTIO calls.
! Then read them with DTIO calls, checking the resulting values.

module dtio
implicit none

type mytype
  integer val
contains
  procedure read_fmt
  procedure write_fmt
  generic::read(formatted)=>read_fmt
  generic::write(formatted)=>write_fmt
end type mytype

contains
  subroutine read_fmt(dtv,iunit,iotype,v_list,iostat,iomsg)
    class(mytype),intent(inout)::dtv
    integer,intent(in)::iunit,v_list(:)
    character(*),intent(in)::iotype
    integer,intent(out)::iostat
    character(*),intent(inout)::iomsg
    character(8)::fmt

    if(iotype(1:2).ne.'DT') then
      read(iunit,*,iostat=iostat,iomsg=iomsg) dtv%val
    else if(iotype.eq.'DT') then
      if(size(v_list).eq.0) then ! no integer argument supplied
        read(iunit,*,iostat=iostat,iomsg=iomsg) dtv%val
      else if(v_list(1).gt.0) then ! generate a format string
        write(fmt,"('(I',i0,')')") v_list(1)
        read(iunit,fmt,iostat=iostat,iomsg=iomsg) dtv%val
      else ! return an error indication
        iostat=10
        iomsg='first numeric argument to DT must be positive'
      endif
    endif
  end subroutine read_fmt

  subroutine write_fmt(dtv,iunit,iotype,v_list,iostat,iomsg)
    class(mytype),intent(in)::dtv
    integer,intent(in)::iunit,v_list(:)
    character(*),intent(in)::iotype
    integer,intent(out)::iostat
    character(*),intent(inout)::iomsg
    character(8)::fmt

    if(iotype(1:2).ne.'DT') then
      write(iunit,*,iostat=iostat,iomsg=iomsg) dtv%val
    else if(size(v_list).eq.0) then ! no integer argument supplied
      write(iunit,*,iostat=iostat,iomsg=iomsg) dtv%val
    else if(v_list(1).gt.0) then ! generate a format string
      write(fmt,"('(I',i0,')')") v_list(1)
      write(iunit,fmt,iostat=iostat,iomsg=iomsg) dtv%val
    else ! return an error indication
      iostat=10
      iomsg='first numeric argument to DT must be positive'
    endif
  end subroutine write_fmt
end module dtio

  use dtio
  type(mytype)::a,b
  integer::istat,count
  character(80)::rmsg1=' ',rmsg2=' ',wmsg1=' ',wmsg2=' '

  100 format(dt(10))
  200 format(2dt(10))
  300 format(dt(-1))

  count=0

  ! write tests --------------------------------------------------------

  open(8,file='io26.dat',action='write')

  ! Test #1: Is a single dt(10) specifier processed correctly?

  a%val=10
  write(8,'(dt(10))') a

  b%val=11
  write(8,100) b

  ! Test #2: Does format processing implicitly repeat?

  a%val=12
  b%val=13
  write(8,'(dt(10))') a,b

  a%val=14
  b%val=15
  write(8,100) a,b

  ! Test #3: Is an explicit repeat count processed correctly?

  a%val=16
  b%val=17
  write(8,'(2dt(10))') a,b

  a%val=18
  b%val=19
  write(8,200) a,b

  ! Test #4: Does the runtime handle an error indication?

  a%val=20
  b%val=21
  write(8,'(dt(-1))',iostat=istat,iomsg=wmsg1) a,b
  if(istat.ne.0) write(8,'(a)') wmsg1

  a%val=22
  b%val=23
  write(8,300,iostat=istat,iomsg=wmsg2) a,b
  if(istat.ne.0) write(8,'(a)') wmsg2

  close(8)

  ! read tests ---------------------------------------------------------

  open(8,file='io26.dat',action='read')

  ! Test #1: Is a single dt(10) specifier processed correctly?

  read(8,'(dt(10))') a
  if (a%val.ne.10) count=count+1

  read(8,100) b
  if (b%val.ne.11) count=count+1

  ! Test #2: Does format processing implicitly repeat?

  read(8,'(dt(10))') a,b
  if (a%val.ne.12) count=count+1
  if (b%val.ne.13) count=count+1

  read(8,100) a,b
  if (a%val.ne.14) count=count+1
  if (b%val.ne.15) count=count+1

  ! Test #3: Is an explicit repeat count processed correctly?

  read(8,'(2dt(10))') a,b
  if (a%val.ne.16) count=count+1
  if (b%val.ne.17) count=count+1

  read(8,200) a,b
  if (a%val.ne.18) count=count+1
  if (b%val.ne.19) count=count+1

  read(8,'(a)') msg1

  ! Test #4: Does the runtime handle an error indication?

  read(8,'(dt(-1))',iostat=istat,iomsg=rmsg1) a,b
  if (istat.ne.10)           count=count+1
  if (len_trim(rmsg1).ne.45) count=count+1
  if (rmsg1.ne.wmsg1)        count=count+1

  read(8,300,iostat=istat,iomsg=rmsg2) a,b
  if (istat.ne.10)           count=count+1
  if (len_trim(rmsg2).ne.45) count=count+1
  if (rmsg2.ne.wmsg2)        count=count+1

  if (count.eq.0) print*,'PASS'
  if (count.ne.0) print*,'fail',count

  close(8)
end
