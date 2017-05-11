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

/* update start asynch receive message stats */

void
__fort_trac_arecv(int cpu, long len, int reqn)
{
}

/* update done asynch receive message stats */

void
__fort_trac_arecv_done(int cpu)
{
}

/* update start asynch send message stats */

void
__fort_trac_asend(int cpu, long len, int reqn)
{
}

/* update done asynch send message stats */

void
__fort_trac_asend_done(int cpu)
{
}

/* update start await receive message stats */

void
__fort_trac_await(int reqn)
{
}

/* update done await receive message stats */

void
__fort_trac_await_done(int reqn)
{
}
