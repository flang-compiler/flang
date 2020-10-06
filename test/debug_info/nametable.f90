!RUN: %flang %s -gdwarf-5 -S -emit-llvm -o - | FileCheck --check-prefix=NOTABLE %s
!RUN: %flang %s -g -S -emit-llvm -o - | FileCheck --check-prefix=NOTABLE %s

!RUN: %flang %s -gdwarf-5 -gpubnames -S -emit-llvm -o - | FileCheck --check-prefix=TABLE %s
!RUN: %flang %s -g -gpubnames -S -emit-llvm -o - | FileCheck --check-prefix=TABLE %s

!Ensure that "nameTableKind: None" field is present in DICompileUnit.
!NOTABLE: !DICompileUnit({{.*}}, nameTableKind: None

!Ensure that "nameTableKind: None" field is NOT present in DICompileUnit.
!TABLE-NOT: !DICompileUnit({{.*}}, nameTableKind: None

PROGRAM main
END PROGRAM main

