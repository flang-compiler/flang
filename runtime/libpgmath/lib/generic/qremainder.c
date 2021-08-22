/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

extern long double remainderl(long double, long double);

long double __mth_i_qremainder(long double x, long double y)
{
  return remainderl(x, y);
}

