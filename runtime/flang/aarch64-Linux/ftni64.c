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

#include "ftni64.h"

#ifdef TM_I8

_ULONGLONG_T
ftn_i_kishft(_ULONGLONG_T op, int count)
{
  /*
          logical shift:
              cnt < 0 ==> shift op1 left by |cnt|
              cnt > 0 ==> shift op1 right by cnt
              |cnt| >= 64 ==> result is 0
  */

  if (count >= 0) {
    if (count >= 64)
      return 0;
    return op << count;
  }
  if (count <= -64)
    return 0;
  return op >> -count;
}

#else

__I8RET_T
ftn_i_kishft(_LONGLONG_T op, int count)
{
  /*
          logical shift:
              cnt < 0 ==> shift op1 left by |cnt|
              cnt > 0 ==> shift op1 right by cnt
              |cnt| >= 64 ==> result is 0
  */
  UINT64 u_arg;
  INT64 arg, res;
  int msh;
  INT64D u;

  if (count >= 64 || count <= -64) {
    res[0] = res[1] = 0;
    goto return_it;
  }

  u.lv = op;
  msh = u_arg[0] = arg[0] = I64_MSH(u.i);
  u_arg[1] = arg[1] = I64_LSH(u.i);

  if (count == 0) {
    res[0] = arg[0];
    res[1] = arg[1];
    goto return_it;
  }
  if (count > 0) {
    if (count < 32) {
      res[0] = (u_arg[0] << count) | (u_arg[1] >> (32 - count));
      res[1] = u_arg[1] << count;
    } else {
      res[0] = u_arg[1] << (count - 32);
      res[1] = 0;
    }
  } else if (count > -32) {
    res[0] = u_arg[0] >> -count;
    res[1] = (u_arg[1] >> -count) | (u_arg[0] << (count + 32));
  } else {
    res[0] = 0;
    res[1] = u_arg[0] >> -(count - 32);
  }
return_it:
  UTL_I_I64RET(res[0], res[1]);
}

#endif

__I8RET_T
ftn_i_xori64(int op1, int op2, int op3, int op4)
{
  INT64 u1;
  INT64 u2;

#ifdef PGI_BIG_ENDIAN
  u1[0] = op1;
  u1[1] = op2;
  u2[0] = op3;
  u2[1] = op4;
#else
  u1[0] = op2;
  u1[1] = op1;
  u2[0] = op4;
  u2[1] = op3;
#endif
  u1[0] ^= u2[0];
  u1[1] ^= u2[1];
  UTL_I_I64RET(u1[0], u1[1]);
}

__I8RET_T
ftn_i_xnori64(int op1, int op2, int op3, int op4)
{
  INT64 u1;
  INT64 u2;

#ifdef PGI_BIG_ENDIAN
  u1[0] = op1;
  u1[1] = op2;
  u2[0] = op3;
  u2[1] = op4;
#else
  u1[0] = op2;
  u1[1] = op1;
  u2[0] = op4;
  u2[1] = op3;
#endif
  u1[0] = ~(u1[0] ^ u2[0]);
  u1[1] = ~(u1[1] ^ u2[1]);
  UTL_I_I64RET(u1[0], u1[1]);
}

int
ftn_i_kr2ir(int op1, int op2)
{
  INT64 u1;
  /*
      result is first element of int[2] which is union u'd with dp if little
      endian; if big endian, result is second element.
  */
  u1[0] = op1;
  u1[1] = op2;
  return I64_LSH(u1);
}

float
ftn_i_kr2sp(int op1, int op2)
{
  INT64 u1;
  int i;

  u1[0] = op1;
  u1[1] = op2;
  i = I64_LSH(u1);
  return (float)i;
}

double
ftn_i_kr2dp(int op1, int op2)
{
  INT64D u1;

  u1.i[0] = op1;
  u1.i[1] = op2;
  return u1.d;
}
