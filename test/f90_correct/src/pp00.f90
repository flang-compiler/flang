!*** Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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
!
! Test functions that return functions, pointer lists,
! pointers are formal arguments, etc.

module mod1
 type list
  integer:: value
  type(list),pointer::next
 end type
 type(list),pointer::head
contains

function pushlist(v,x)
 integer v
 type(list),pointer:: pushlist,x

 if( v .eq. 0 ) then
  pushlist => x
 else
  allocate(pushlist)
  pushlist%value = v
  pushlist%next => x
 endif
end function

function addlist( v, x )
 integer v
 type(list),pointer:: addlist,x,p
 if( v .eq. 0 )then
  nullify(addlist)
 else if( .not.associated(x) )then
  allocate(addlist)
  addlist%value = v
  nullify(addlist%next)
  x => addlist
 else
  p => x
  do while( associated(p%next) )
   p => p%next
  enddo
  allocate(addlist)
  addlist%value = v
  nullify(addlist%next)
  p%next => addlist
 endif
end function

subroutine printlist(x)
 type(list),pointer:: x,p
 print *,'------'
 p => x
 do while( associated(p) )
  print *,'list ',p%value
  p => p%next
 enddo
 print *,'==end='
end subroutine

subroutine getlist(x,result,i)
 type(list),pointer:: x,p
 integer result(:)
 integer i
 p => x
 do while( associated(p) )
  i = i + 1
  if( i.gt.ubound(result,1) ) return
  result(i) = p%value
  p => p%next
 enddo
end subroutine
end module

program p
 use mod1
 type(list),pointer ::x, y
 parameter(n=11)
 integer expect(n),result(n)
 data expect/ 40,30,20,10,-4,-3,-2,-1,10,20,30 /
 integer i
 i = 0
 nullify(head)
 head => pushlist(10,head)
 head => pushlist(20,head)
 head => pushlist(0,head)
 head => pushlist(30,head)
 head => pushlist(40,head)
 call getlist(head,result,i)
 !call printlist(head)
 nullify(x)
 x => pushlist(-1,x)
 x => pushlist(-2,x)
 x => pushlist(0,x)
 x => pushlist(-3,x)
 x => pushlist(-4,x)
 call getlist(x,result,i)
 !call printlist(x)

 nullify(x)
 y => addlist( 10, x )
 y => addlist( 20, x )
 y => addlist(  0, x )
 y => addlist( 30, x )
 call getlist(x,result,i)
 !call printlist(x)

 call check(result,expect,n)
end
