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

MTHINTRIN(acos , ss   , any        ,  __mth_i_acos        ,  __mth_i_acos        , __mth_i_acos          ,__math_dispatch_error)
MTHINTRIN(acos , ds   , any        ,  __mth_i_dacos       ,  __mth_i_dacos       , __mth_i_dacos         ,__math_dispatch_error)
MTHINTRIN(acos , sv4  , any        ,  __gs_acos_4         ,  __gs_acos_4         , __gs_acos_4           ,__math_dispatch_error)
MTHINTRIN(acos , dv2  , any        ,  __gd_acos_2         ,  __gd_acos_2         , __gd_acos_2           ,__math_dispatch_error)
MTHINTRIN(acos , sv4m , any        , __fs_acos_4_mn        , __rs_acos_4_mn        , __ps_acos_4_mn        ,__math_dispatch_error)
MTHINTRIN(acos , dv2m , any        , __fd_acos_2_mn        , __rd_acos_2_mn        , __pd_acos_2_mn        ,__math_dispatch_error)
