!RUN: %flang -gdwarf-4 -S -emit-llvm %s -o - | FileCheck %s

!CHECK: @blk_ = global {{.*}} align 64, !dbg !{{[0-9]+}}, !dbg !{{[0-9]+}}, !dbg !{{[0-9]+}}, !dbg !{{[0-9]+}}, !dbg !{{[0-9]+}}, !dbg !{{[0-9]+}}

PROGRAM bdata
   integer b3i, b3j, adr
   COMMON/BLK/b3i, b3j
   b3i = 111
   b3j = 222
   CALL sub_block_data      ! BP_BEFORE_SUB
   print *,"End of program"
END
! BLOCK DATA
BLOCK DATA
integer b3i, b3j
COMMON/BLK/b3i, b3j
DATA b3i, b3j/11, 22/
END
! SUBROUTINE
SUBROUTINE sub_block_data
   integer b3i, b3j
   COMMON/BLK/b3i, b3j
   b3i = 1111; ! BP_SUB
   b3j = 2222; ! BP_SUB
END
