!
! Copyright (c) 2014, NVIDIA CORPORATION.  All rights reserved.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
! This tests stringification.  The result should be: 
!   Match:rain
!   Match:snow
!

#define doCompare(NAME) if(cmp(a%NAME,b%NAME,sizeof(a%NAME))) print *, "Match:", #NAME

module weather
    type WeatherType
        real rain
        real snow
    end type
   
    contains

    ! Dummy routine, just for syntax and macro testing purposes
    function cmp(a, b, sz)
        real :: a, b
        integer :: sz ! Ignored
        cmp = a .eq. b

        if (cmp .eq. .true.) then
            call check(.true., .true., 1)
        else
            call check(.false., .true., 1)
        endif
    end function cmp

    subroutine compareWeather(a,b)
        type (WeatherType) :: a,b
        doCompare(rain)
        doCompare(snow)
    end subroutine compareWeather
end module weather

program p
    use weather
    logical :: res(1) = .false., expect(1) = .true.
    type(WeatherType) :: foo = WeatherType(1.0, 2.0)
    type(WeatherType) :: bar = WeatherType(1.0, 2.0)
    call compareWeather(foo, bar)
end program
