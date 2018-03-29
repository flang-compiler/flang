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

MTHINTRIN(asin , ss   , any        ,  __mth_i_asin         ,  __mth_i_asin        , __mth_i_asin          ,__math_dispatch_error)
MTHINTRIN(asin , ds   , any        ,  __mth_i_dasin        ,  __mth_i_dasin       , __mth_i_dasin         ,__math_dispatch_error)
MTHINTRIN(asin , sv4  , any        ,  __gs_asin_4          ,  __gs_asin_4         , __gs_asin_4           ,__math_dispatch_error)
MTHINTRIN(asin , dv2  , any        ,  __gd_asin_2          ,  __gd_asin_2         , __gd_asin_2           ,__math_dispatch_error)
MTHINTRIN(asin , sv4m , any        , __fs_asin_4_mn        , __rs_asin_4_mn        , __ps_asin_4_mn        ,__math_dispatch_error)
MTHINTRIN(asin , dv2m , any        , __fd_asin_2_mn        , __rd_asin_2_mn        , __pd_asin_2_mn        ,__math_dispatch_error)
