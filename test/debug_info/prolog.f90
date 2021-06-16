!RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

! check non debug instructions should not have debug location
!CHECK: define void @show_
!CHECK: call void @llvm.dbg.declare
!CHECK-SAME: , !dbg {{![0-9]+}}
!CHECK-NOT: bitcast i64* %"array$sd" to i8*, !dbg
!CHECK: store i64 {{%[0-9]+}}, i64* %z_b_3_{{[0-9]+}}, align 8
!CHECK: br label
!CHECK: ret void, !dbg {{![0-9]+}}
subroutine show (message, array)
  character (len=*) :: message
  integer :: array(:)

  print *, message
  print *, array

end subroutine show

!CHECK: define void @MAIN_
!CHECK-NOT: bitcast void (...)* @fort_init to void (i8*, ...)*, !dbg {{![0-9]+}}
!CHECK: call void @llvm.dbg.declare
!CHECK-SAME: , !dbg {{![0-9]+}}
!CHECK: ret void, !dbg
program prolog

  interface
     subroutine show (message, array)
       character (len=*) :: message
       integer :: array(:)
     end subroutine show
  end interface

  integer :: array(10) = (/1,2,3,4,5,6,7,8,9,10/)

  call show ("array", array)
end program prolog
