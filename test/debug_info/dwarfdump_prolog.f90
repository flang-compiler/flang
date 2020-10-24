!RUN: %flang -g %s -o %t
!RUN: llvm-dwarfdump  --debug-line %t -o - | FileCheck %s

!CHECK: name: "dwarfdump_prolog.f90"
!CHECK: Address            Line   Column File   ISA Discriminator Flags
!CHECK: {{0x[0-9a-f]+}}     13      1      1   0             0  is_stmt prologue_end
!CHECK: {{0x[0-9a-f]+}}     29      1      1   0             0  is_stmt prologue_end

subroutine show (message, array)
  character (len=*) :: message
  integer :: array(:)

  print *, message
  print *, array

end subroutine show

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
