/*
 * Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \brief Utility routines for namelist IO.
 */

#include "global.h"
#include "nml.h"

/** \brief
 * Return the size of a name list item
 */
__BIGINT_T
I8(siz_of)(NML_DESC *descp)
{
  int sz;

  switch (descp->type) {
  case __DERIVED:
    sz = descp->len;
    break;
  case __STR:
    sz = descp->len;
    F90_Desc *sd = get_descriptor(descp);
    if (sd != NULL) {
      descp->len = sz = F90_LEN_G(sd);
    }
    break;
  case __NCHAR:
    sz = descp->len * FIO_TYPE_SIZE(descp->type);
    break;
  default:
    sz = FIO_TYPE_SIZE(descp->type);
    break;
  }
  return sz;
}

/** \brief
 * Return the number of elements in a name list item.
 */
int
nelems_of(NML_DESC *descp)
{
  int nelems;
  int k;
  __POINT_T *desc_dims;
  __POINT_T actual_ndims;

  /* count up number of constants ( == 1 unless array):  */
  if (descp->ndims == 0)
    return 1;

  /* MAX_DIM (30) is the number number we add to actual ndims in semfin.c */
  if (descp->ndims == MAX_DIM)
    return 1;

  /* write one at a time */
  if (descp->ndims == -1 || descp->ndims == -2) { /* for pointer/allocatable */
    return 1;
  }

  desc_dims = (__POINT_T *)((char *)descp + sizeof(NML_DESC));
  nelems = desc_dims[1] - desc_dims[0] + 1;
  ACTUAL_NDIMS(actual_ndims);
  for (k = 1; k < actual_ndims; k++)
    nelems *= (desc_dims[2 * k + 1] - desc_dims[2 * k] + 1);

  return nelems;
}

/** \brief
 * If a descriptor exists for a data item return a pointer to it.  Otherwise 
 * return NULL.
 */
F90_Desc *
get_descriptor(NML_DESC *descp)
{
  if (descp->ndims == -1 || descp->ndims == -2 || descp->ndims >= MAX_DIM)
    return (F90_Desc *)*(char **)((char *)descp + sizeof(NML_DESC) +
                                  sizeof(__POINT_T));
  else
    return NULL;
}
