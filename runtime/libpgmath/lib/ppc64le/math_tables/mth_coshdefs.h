/*
 * Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

MTHINTRIN(cosh , ss   , any        , __mth_i_cosh          , __mth_i_cosh          , __mth_i_cosh          ,__math_dispatch_error)
MTHINTRIN(cosh , ds   , any        , __mth_i_dcosh         , __mth_i_dcosh         , __mth_i_dcosh         ,__math_dispatch_error)
MTHINTRIN(cosh , sv4  , any        , __gs_cosh_4           , __gs_cosh_4           , __gs_cosh_4           ,__math_dispatch_error)
MTHINTRIN(cosh , dv2  , any        , __gd_cosh_2           , __gd_cosh_2           , __gd_cosh_2           ,__math_dispatch_error)
MTHINTRIN(cosh , sv4m , any        , __fs_cosh_4_mn        , __rs_cosh_4_mn        , __ps_cosh_4_mn        ,__math_dispatch_error)
MTHINTRIN(cosh , dv2m , any        , __fd_cosh_2_mn        , __rd_cosh_2_mn        , __pd_cosh_2_mn        ,__math_dispatch_error)
