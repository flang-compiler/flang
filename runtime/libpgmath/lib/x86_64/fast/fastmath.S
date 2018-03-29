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
#define	LNUM	004
#include "fastmath.h"
#undef	VEX_TARGET
#undef	LNUM


/*
 *	Fifth pass.
 */
#define	VEX_TARGET
#define	FMA4_TARGET
#define	LNUM	005
#include "fastmath.h"
#undef	VEX_TARGET
#undef	FMA4_TARGET
#undef	LNUM


/*
 *	Sixth pass.
 */
#define	HELPER_TARGET
#define	LNUM	006
#include "fastmath.h"
#undef	HELPER_TARGET
#undef	LNUM
