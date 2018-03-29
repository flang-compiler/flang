
/*
 * Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

  VRS_T rarg1;
  VRS_T rout;
  float reltol = TOL;

  rarg1 = vrs_set_arg(FMIN, FCONST1);

  rout = CONCAT7(__,FRP,PREC,_,FUNC,_,VL)(rarg1);

  if (checkfltol(rout,expd_res,VL,reltol) != 0) exit(-1);
