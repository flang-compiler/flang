
/*
 * Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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


#include <math.h>
#include <common_sin.h>

vdouble static INLINE
__reduction_slowpath(vdouble const a, vmask *h)
{
    union {
        vdouble vd;
        vmask vm;
        double sd[sizeof(vdouble) / sizeof(double)];
        uint64_t sm[sizeof(vmask) / sizeof(uint64_t)];
    } rv, av, hv;
    av.vd = a;

    for (int i = 0; i < sizeof(vdouble) / sizeof(double); i++) {
        rv.sd[i] = reduction_slowpath(av.sd[i], hv.sm + i);
    }
    *h = hv.vm;

    return rv.vd;
}

#define fma(a, b, c) vfma_vd_vd_vd_vd((a), (b), (c))

vdouble static INLINE
__sin_d_kernel(vdouble const a, vint2 const h)
{
    vdouble A = vcast_vd_d(A_D);
    vdouble B = vcast_vd_d(B_D);
    vdouble C = vcast_vd_d(C_D);
    vdouble D = vcast_vd_d(D_D);
    vdouble E = vcast_vd_d(E_D);
    vdouble F = vcast_vd_d(F_D);
    vdouble G = vcast_vd_d(G_D);

#ifdef FASTER // ???
    vdouble x = (vdouble)vxor_vi2_vi2_vi2((vint2)a, h);
    vdouble x2 = vmul_vd_vd_vd(a, a);
    vdouble x3 = vfma_vd_vd_vd_vd(x2, x, vcast_vd_d(0.0));
    vdouble x4 = vmul_vd_vd_vd(x2, x2);
    vdouble x7 = vmul_vd_vd_vd(x3, x4);
    return fma(fma(fma(fma(A, x4, C), x2, fma(B, x4, D)), x2, E), x7, fma(fma(F, x2, G), x3, x));*/
#else
    vdouble s, r, f, t;
    s = vmul_vd_vd_vd(a, a);
    r = vfma_vd_vd_vd_vd(A, s, B);
    r = vfma_vd_vd_vd_vd(r, s, C);
    r = vfma_vd_vd_vd_vd(r, s, D);
    r = vfma_vd_vd_vd_vd(r, s, E);
    r = vfma_vd_vd_vd_vd(r, s, F);
    r = vfma_vd_vd_vd_vd(r, s, G);
    f = (vdouble)vxor_vi2_vi2_vi2((vint2)a, h);
    t = vfma_vd_vd_vd_vd(s, f, vcast_vd_d(0.0));
    r = vfma_vd_vd_vd_vd(r, t, f);
    return r;
#endif
}

vdouble static INLINE
__sin_d_vec(vdouble const x)
{

    vdouble a, k, r;
    vint2 p, h;

    k = vfma_vd_vd_vd_vd(x, vcast_vd_d(_1_OVER_PI), vcast_vd_d(6755399441055744.0));
    h = vsll64_vi2_vi2_i((vint2)k, 63);
    k = vsub_vd_vd_vd(k, vcast_vd_d(6755399441055744.0));

    a = vfma_vd_vd_vd_vd(k, vcast_vd_d(-PI_HI), x);
    a = vfma_vd_vd_vd_vd(k, vcast_vd_d(-PI_MI), a);
    a = vfma_vd_vd_vd_vd(k, vcast_vd_d(-PI_LO), a);

    r = __sin_d_kernel(a, h);

    p = vand_vi2_vi2_vi2((vint2)x, (vint2)vcast_vm_i_i(0x7fffffff, 0xffffffff));
    vopmask m = vgt64_vo_vi2_vi2(p, (vint2)vcast_vd_d(THRESHOLD));

    if (__builtin_expect(!vtestz_i_vo(m), 0)) {
    //if (!vtestz_i_vo(m)) {
        vdouble res;
        vopmask ninf;
        vmask half;

        res = __reduction_slowpath(x, &half);
        res = __sin_d_kernel(res, half);
        ninf = vgt64_vo_vi2_vi2((vint2)vcast_vm_i_i(0x7ff00000, 0), p);

        res = vsel_vd_vo_vd_vd(ninf, res, vmul_vd_vd_vd(x, vcast_vd_d(0.0)));

        r = vsel_vd_vo_vd_vd(m, res, r);
    }

    //print(r);
    return r;
}

