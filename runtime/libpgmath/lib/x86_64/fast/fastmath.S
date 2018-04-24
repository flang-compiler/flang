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
 */

/*
 *	Global to all include passes.
 */
#include "tmp.mthdecls.h"

#ifdef PGFLANG
#include "directives.h"
#else
#include "directives.h"
#endif

#define	FMATH_EXCEPTIONS

/*
 *	First pass.
 */
#define	TABLE_TARGET
#define	LNUM	001
#include "fastmath.h"
#undef	TABLE_TARGET
#undef	LNUM


/*
 *	Second pass.
 */
#define	LNUM	002
#include "fastmath.h"
#undef	LNUM


/*
 *	Third pass.
 */
#define	GH_TARGET
#define	LNUM	003
#include "fastmath.h"
#undef	GH_TARGET
#undef	LNUM


/*
 *	Fourth pass.
 */
#define	VEX_TARGET
#define	TARGET_VEX_OR_FMA	vex
#define	LNUM	004
#include "fastmath.h"
#undef	VEX_TARGET
#undef	TARGET_VEX_OR_FMA
#undef	LNUM


/*
 *	Fifth pass.
 */
#define	VEX_TARGET
#define	TARGET_FMA
#define	TARGET_VEX_OR_FMA	fma4
#define	VFMA_IS_FMA3_OR_FMA4	FMA4
#define	LNUM	005
#include "fastmath.h"
#undef	VEX_TARGET
#undef	TARGET_FMA
#undef	TARGET_VEX_OR_FMA
#undef	VFMA_IS_FMA3_OR_FMA4
#undef	LNUM


/*
 *	Sixth pass.
 */
#define	HELPER_TARGET
#define	LNUM	006
#include "fastmath.h"
#undef	HELPER_TARGET
#undef	LNUM

/*
 *	Seventh pass.
 */
#define	VEX_TARGET
#define	TARGET_FMA
#define	TARGET_VEX_OR_FMA	avx2
#define	VFMA_IS_FMA3_OR_FMA4	FMA3
#define	LNUM	007
#include "fastmath.h"
#undef	VEX_TARGET
#undef	TARGET_FMA
#undef	TARGET_VEX_OR_FMA
#undef	VFMA_IS_FMA3_OR_FMA4
#undef	LNUM
