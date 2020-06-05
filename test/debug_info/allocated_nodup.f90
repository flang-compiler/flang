!RUN: %flang -gdwarf-4 -S -emit-llvm %s -o - | FileCheck %s

!CHECK: distinct !DIGlobalVariable(name: "arr",
!CHECK-SAME: type: [[TYPE:![0-9]+]]
!CHECK: [[TYPE]] = !DICompositeType(tag: DW_TAG_array_type, baseType: [[DTYPE:![0-9]+]]
!CHECK: [[DTYPE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "dtype"
!CHECK-SAME: elements: [[MEMBERS:![0-9]+]]
!CHECK: [[MEMBERS]] = !{[[MEM1:![0-9]+]]
!CHECK: [[MEM1]] = !DIDerivedType(tag: DW_TAG_member, name: "memfunptr",
!CHECK-SAME: baseType: [[FUNPTRTYPE:![0-9]+]]
!CHECK: [[FUNPTRTYPE]] = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: [[FUNTYPE:![0-9]+]]
!CHECK: [[FUNTYPE]] = !DISubroutineType(types: [[FUNSIGNATURE:![0-9]+]])
!CHECK: [[FUNSIGNATURE]] = !{[[DTYPE]]}

module pdt
  type dtype
    procedure (func), pointer, nopass :: memfunptr
    integer, allocatable :: memalcarr(:)
  end type dtype
contains
  function func()
    class (dtype), allocatable :: func
  end function func
end module pdt

program main
  use pdt
  type (dtype) arr(3)
  allocate(arr(1)%memalcarr(10))
  arr(1)%memalcarr=9
  print *, arr(1)%memalcarr
end program main
