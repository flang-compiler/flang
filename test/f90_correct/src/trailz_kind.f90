      program test
      integer, parameter :: num = 1
      integer results(num), expect(num)
      data expect /1/
      integer (kind =8)  :: arr_kind8(5)
      integer (kind =2)  :: arr_kind2(5)
      integer (kind =1)  :: arr_kind1(5)
      integer results_kind1(5), results_kind2(5), results_kind4(5), results_kind8(5)
      integer , parameter :: arr(5)=(/-108,-1,64,-64,1/) !32 bits kind=4

         arr_kind8=arr
         arr_kind2=arr
         arr_kind1=arr

     do i=1,5
          results_kind1(i)=trailz(arr_kind1(i))
          results_kind2(i)=trailz(arr_kind2(i))
          results_kind4(i)=trailz(arr(i))
          results_kind8(i)=trailz(arr_kind8(i))
      end do

      if (all( results_kind8 .eq. results_kind4)) then
        if (all(results_kind2 .eq. results_kind8)) then
          if(all(results_kind2 .eq. results_kind1)) then
              results(1)=1
              print *, 'expect  vs results match'
          else
              results(1)=0
              print *, 'resulst_kind2  vs results_kind1  mismatch'
          endif
        else
          results(1)=0
          print *, 'resulst_kind2  vs results_kind8  mismatch'
        endif
      else
        results(1)=0
        print *, 'resulst_kind8  vs results_kind4  mismatch'
      endif
      call check(results, expect, num)
      end program
