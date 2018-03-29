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

MTHINTRIN(log  , ss   , any        ,  __mth_i_log         ,  __mth_i_log         , __mth_i_log           ,__math_dispatch_error)
MTHINTRIN(log  , ds   , any        ,  __mth_i_dlog        ,  __mth_i_dlog        , __mth_i_dlog          ,__math_dispatch_error)
MTHINTRIN(log  , sv4  , any        ,  __gs_log_4          ,  __gs_log_4          , __gs_log_4            ,__math_dispatch_error)
MTHINTRIN(log  , dv2  , any        ,  __gd_log_2          ,  __gd_log_2          , __gd_log_2            ,__math_dispatch_error)
MTHINTRIN(log  , sv4m , any        , __fs_log_4_mn         , __rs_log_4_mn         , __ps_log_4_mn         ,__math_dispatch_error)
MTHINTRIN(log  , dv2m , any        , __fd_log_2_mn         , __rd_log_2_mn         , __pd_log_2_mn         ,__math_dispatch_error)
