
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
#include <common_cosf.h>

//static vmask i1opi_vec[] = {
//    vcast_vm_i_i(0, i1opi_f[0]),
//    vcast_vm_i_i(0, i1opi_f[1]),
//    vcast_vm_i_i(0, i1opi_f[2]),
//    vcast_vm_i_i(0, i1opi_f[3]),
//    vcast_vm_i_i(0, i1opi_f[4]),
//    vcast_vm_i_i(0, i1opi_f[5]),
//};

vfloat static INLINE
__reduction_slowpath(vfloat const a, vmask *h)
{
    vint2 ia, e, idx, q, p;
    vint2 ia_a, ia_b, p_a, p_b, hi_a, hi_b;
    vint2 hi, lo, ll, prev, prev2;

    vmask i1opi_vec[] = {
        vcast_vm_i_i(0, i1opi_f[0]),
        vcast_vm_i_i(0, i1opi_f[1]),
        vcast_vm_i_i(0, i1opi_f[2]),
        vcast_vm_i_i(0, i1opi_f[3]),
        vcast_vm_i_i(0, i1opi_f[4]),
        vcast_vm_i_i(0, i1opi_f[5]),
    };

    ia = (vint2)a;
    /* e = ((ia >> 23) & 0xff) - 127; */
    e = vsrl_vi2_vi2_i(ia, 23);
    e = vand_vi2_vi2_vi2(e, vcast_vi2_i(0xff));
    e = vsub_vi2_vi2_vi2(e, vcast_vi2_i(127));
    /* ia = (ia << 8) | 0x80000000; */
    ia = vsll_vi2_vi2_i(ia, 8);
    ia = vor_vi2_vi2_vi2(ia, vcast_vi2_i(0x80000000));

    /* compute x * 1/pi */
    /* idx = 6 - ((e >> 5) & 3); */
    idx = vsrl_vi2_vi2_i(e, 5);
    idx = vand_vi2_vi2_vi2(idx, vcast_vi2_i(3));
    idx = vsub_vi2_vi2_vi2(vcast_vi2_i(6), idx);

    ia_a = vsrl64_vi2_vi2_i(ia, 32);
    ia_b = ia;
    hi_a = vcast_vi2_i(0);
    hi_b = vcast_vi2_i(0);

    q = vcast_vi2_i(0);
    for (int i = 0; i < 6; i++) {
        p_a = vmulu_vi2_vi2_vi2((vint2)i1opi_vec[i], ia_a);
        p_b = vmulu_vi2_vi2_vi2((vint2)i1opi_vec[i], ia_b);
        p_a = vadd64_vi2_vi2_vi2(p_a, hi_a);
        p_b = vadd64_vi2_vi2_vi2(p_b, hi_b);

        hi_a = vsrl64_vi2_vi2_i(p_a, 32);
        hi_b = vsrl64_vi2_vi2_i(p_b, 32);

        p_a = vsll64_vi2_vi2_i(p_a, 32);
        p_b = vand_vi2_vi2_vi2(p_b, vcast_vm_i_i(0, 0xffffffff));

        p = vor_vi2_vi2_vi2(p_a, p_b);

        vopmask m = veq_vo_vi2_vi2(idx, q);
        hi = vsel_vi2_vo_vi2_vi2(m, p, hi);
        lo = vsel_vi2_vo_vi2_vi2(m, prev, lo);
        ll = vsel_vi2_vo_vi2_vi2(m, prev2, ll);

        prev2 = prev;
        prev = p;

        q = vadd_vi2_vi2_vi2(q, vcast_vi2_i(1));
    }
    p = vor_vi2_vi2_vi2(vsll64_vi2_vi2_i(hi_a, 32), hi_b);

    vopmask m = veq_vo_vi2_vi2(idx, q);
    hi = vsel_vi2_vo_vi2_vi2(m, p, hi);
    lo = vsel_vi2_vo_vi2_vi2(m, prev, lo);
    ll = vsel_vi2_vo_vi2_vi2(m, prev2, ll);

    e = vand_vi2_vi2_vi2(e, vcast_vi2_i(31));

    union {
        vint2 v;
        uint32_t t[sizeof(vint2) / sizeof(uint32_t)];
    } ue, uhi, ulo, ull, us, uh, ur;
    ue.v = e; uhi.v = hi; ulo.v = lo; ull.v = ll;
    for (unsigned i = 0; i < sizeof(vint2) / sizeof(uint32_t); i++) {
        uint32_t e = ue.t[i], q;
        uint64_t p = ((uint64_t)uhi.t[i] << 32) | ulo.t[i];

        if (e) {
            q = 32 - e;
            p = (p << e) | (ull.t[i] >> q);
        }

        uh.t[i] = (uhi.t[i] << e) & 0x80000000;
        p &= 0x7fffffffffffffffULL;
        p = (int64_t)p - 0x4000000000000000LL;

        double d = (double)(int64_t)p;
        d *= PI_2_M63;
        float r = (float)d;
        ur.t[i] = float_as_int(r);
    }
    vstore_v_p_vf((float*)h, (vfloat)uh.v);
    return (vfloat)ur.v;
}

vfloat static INLINE
__sin_kernel(vfloat const a, vint2 const h)
{
    vfloat s, r, f, t;

    s = vmul_vf_vf_vf(a, a);
    r = vcast_vf_f(A_F);
    r = vfma_vf_vf_vf_vf(r, s, vcast_vf_f(B_F));
    r = vfma_vf_vf_vf_vf(r, s, vcast_vf_f(C_F));
    r = vfma_vf_vf_vf_vf(r, s, vcast_vf_f(D_F));
    f = (vfloat)vxor_vi2_vi2_vi2((vint2)a, h);
    t = vmul_vf_vf_vf(s, f);
    r = vfmapn_vf_vf_vf_vf(r, t, f);

    return r;
}

vfloat static INLINE
__cos_f_vec(vfloat const x)
{

    vfloat p, a, k, r;
    vopmask m0, m1;
    vint2 h;

    p = (vfloat)vand_vi2_vi2_vi2((vint2)x, vcast_vi2_i(0x7fffffff));

    k = vfma_vf_vf_vf_vf(p, vcast_vf_f(_1_OVER_PI_F), vcast_vf_f(-0.5f));
    k = vadd_vf_vf_vf(k, vcast_vf_f(12582912.0f));
    h = vsll_vi2_vi2_i((vint2)k, 31);
    k = vsub_vf_vf_vf(k, vcast_vf_f(12582912.0f));
    k = vfma_vf_vf_vf_vf(vcast_vf_f(2.0f), k, vcast_vf_f(1.0f));

    a = vfma_vf_vf_vf_vf(k, vcast_vf_f(-PI_2_HI_F), p);
    a = vfma_vf_vf_vf_vf(k, vcast_vf_f(-PI_2_MI_F), a);
    a = vfma_vf_vf_vf_vf(k, vcast_vf_f(-PI_2_LO_F), a);

    r = __sin_kernel(a, h);

    m0 = vgt_vo_vi2_vi2((vint2)p, vcast_vi2_i(0x39800000));
    r = vsel_vf_vo_vf_vf((vopmask)m0, r, vcast_vf_f(1.0f));

    m1 = vgt_vo_vi2_vi2((vint2)p, (vint2)vcast_vf_f(THRESHOLD_F));
    if (__builtin_expect(!vtestz_i_vo(m1), 0)) {
        vfloat res;
        vopmask ninf;
        vmask half;

        res = __reduction_slowpath(x, &half);
        res = __sin_kernel(res, half);
        ninf = vgt_vo_vi2_vi2(vcast_vi2_i(0x7f800000), (vint2)p);
        res = vsel_vf_vo_vf_vf(ninf, res, vmul_vf_vf_vf(x, vcast_vf_f(0.0f)));

        r = vsel_vf_vo_vf_vf(m1, res, r);
    }

    return r;
}
