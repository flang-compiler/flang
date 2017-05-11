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


/** \file
 *  \brief extern declarations for ftnbitsup.c
 */

/** \brief
 *  performs circular bit shift
 *  sc > 0 => circular left shift.
 *  sc < 0 => circular right shift.
 */
int
Ftn_ishftc(int val, int sc, int rc);

/** \brief
 *  Ftn_i_iishftc, 16-bit integer
 *  sc > 0 => circular left shift.
 *  sc < 0 => circular right shift.
 */
int
Ftn_i_iishftc(int val, int sc, int rc);

/** 
 *  \brief
 *  performs circular bit shift.
 *  sc > 0 => circular left shift.
 *  sc < 0 => circular right shift.
 */
int Ftn_i_i1shftc(int val, int sc, int rc);

/**
 * \brief
 * moves len bits from pos in src to posd in dest
 */
void Ftn_jmvbits(int src, int pos, int len, int *dest, int posd);

/**
 * \brief
 * moves len bits from pos in src to posd in dest-- dest is 16-bit integer
 */
void Ftn_imvbits(int src, int pos, int len, short int *dest, int posd);

/**
 * \brief
 * zero extends value to 32 bits
 */
int Ftn_jzext(int val, int dt);

/*C* function: Ftn_izext
 * \brief
 * zero extends value to 16 bits
 */
int Ftn_izext(int val, int dt);

