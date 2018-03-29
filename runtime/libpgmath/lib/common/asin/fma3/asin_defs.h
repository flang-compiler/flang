/* 
 * Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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
 *
 */

int   const ABS_MASK_I  = 0x7fffffff;
int   const SGN_MASK_I  = 0x80000000;
float const ONE_F       = 1.0f;
float const THRESHOLD_F = 0.5705f;
float const PIO2_F      = 1.570796327f;

// p0 coefficients
float const A0_F        =  5.175137819e-02f;
float const B0_F        =  1.816697683e-02f;
float const C0_F        =  4.675685871e-02f;
float const D0_F        =  7.484657646e-02f;
float const E0_F        =  1.666701424e-01f;

// p1 coefficients
float const A1_F        = -7.437243476e-04f;
float const B1_F        =  5.207145121e-03f;
float const C1_F        = -1.764218137e-02f;
float const D1_F        =  4.125141352e-02f;
float const E1_F        = -8.533414453e-02f;
float const F1_F        =  2.137603760e-01f;
float const G1_F        = -1.570712566e-00f;

