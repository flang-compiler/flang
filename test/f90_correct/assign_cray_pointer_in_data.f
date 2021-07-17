       ! RUN: %flang -c %s -fsyntax-only
      PROGRAM MINIMAL
      IMPLICIT NONE
      REAL OLD(1)
      POINTER(IOLD, OLD)
      DATA IOLD/-1/
      END PROGRAM MINIMAL
