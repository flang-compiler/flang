
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

long long const ABS_MASK_LL   = 0x7fffffffffffffffLL;
long long const SGN_MASK_LL   = 0x8000000000000000LL;
long long const THRESHOLD_LL  = 0x3fe2666600000000LL;
double    const PIO2_HI_D     = 1.5707963267948966e+00;
double    const PIO2_LO_D     = 6.1232339957367660e-17;

// p0 coefficients
double const A0_D =  6.259798167646803e-02;
double const B0_D = -7.620591484676952e-02;
double const C0_D =  6.686894879337643e-02;
double const D0_D = -1.787828218369301e-02;
double const E0_D =  1.745227928732326e-02;
double const F0_D =  1.000422754245580e-02;
double const G0_D =  1.418108777515123e-02;
double const H0_D =  1.733194598980628e-02;
double const I0_D =  2.237350511593569e-02;
double const J0_D =  3.038188875134962e-02;
double const K0_D =  4.464285849810986e-02;
double const L0_D =  7.499999998342270e-02;
double const M0_D =  1.666666666667375e-01;

// p1 coefficients
double const A1_D =  1.721210849597979257994719015645301851691328920423985e-05;
double const B1_D = -1.842317629775989658389573344621226169692818075418472e-04;
double const C1_D =  9.344810842282320548224383571778162149712443351745605e-04;
double const D1_D = -3.018570623999869938225426579947452410124242305755615e-03;
double const E1_D =  7.090796449446847699027429712259618099778890609741211e-03;
double const F1_D = -1.325851090927590049395323745784480706788599491119385e-02;
double const G1_D =  2.140283993664857456473704644395184004679322242736816e-02;
double const H1_D = -3.243581768138021487191124947457865346223115921020508e-02;
double const I1_D =  5.041038820371314399526596616851747967302799224853516e-02;
double const J1_D = -8.896780217400947210482087257332750596106052398681641e-02;
double const K1_D =  0.214591350956731541366195870068622753024101257324219e-00;
double const L1_D = -1.570795697960716275076720194192603230476379394531250e-00;

