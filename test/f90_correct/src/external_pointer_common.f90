! Copyright (c) 1989, NVIDIA CORPORATION.  All rights reserved.
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

! Test different combinations of common + external + pointer
program comm
 common /com/ Q1
 external Q1
 pointer Q1

 common /com/ Q2
 pointer Q2
 external Q2

 ! This is a bug, uncomment when fixed
 !external Q3
 !common /com/ Q3
 !pointer Q3

 external Q4
 pointer Q4
 common /com/ Q4

 pointer Q5
 common /com/ Q5
 external Q5

 pointer Q6
 external Q6
 common /com/ Q6
end
