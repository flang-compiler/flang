!RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

!CHECK: !DILocalVariable(name: "cvar", scope: {{![0-9]+}}, file: {{![0-9]+}}, type: [[TYPE:![0-9]+]])
!CHECK: [[TYPE]] = !DIBasicType(tag: DW_TAG_string_type, name: "character",


program main
  character :: cvar
  cvar = 'a'
end program main
