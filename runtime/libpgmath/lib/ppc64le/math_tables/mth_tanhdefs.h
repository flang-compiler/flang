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

MTHINTRIN(tanh , ss   , any        ,  __mth_i_tanh        ,  __mth_i_tanh        , __mth_i_tanh          ,__math_dispatch_error)
MTHINTRIN(tanh , ds   , any        ,  __mth_i_dtanh       ,  __mth_i_dtanh       , __mth_i_dtanh         ,__math_dispatch_error)
MTHINTRIN(tanh , sv4  , any        ,  __gs_tanh_4         ,  __gs_tanh_4         , __gs_tanh_4           ,__math_dispatch_error)
MTHINTRIN(tanh , dv2  , any        ,  __gd_tanh_2         ,  __gd_tanh_2         , __gd_tanh_2           ,__math_dispatch_error)
MTHINTRIN(tanh , sv4m , any        , __fs_tanh_4_mn        , __rs_tanh_4_mn        , __ps_tanh_4_mn        ,__math_dispatch_error)
MTHINTRIN(tanh , dv2m , any        , __fd_tanh_2_mn        , __rd_tanh_2_mn        , __pd_tanh_2_mn        ,__math_dispatch_error)
