/*
 * Copyright (c) 2010-2018, NVIDIA CORPORATION.  All rights reserved.
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

/* clang-format off */

/** \file
 * \brief F2003 polymorphic/OOP support
 */

#ifndef _TYPE_H_
#define _TYPE_H_

#include "fioMacros.h"

#define FUNC(x) void (*x)()
/* VTABLE is a pointer to a NULL terminated array of function pointers. */
#define VTABLE(x) void (**x)()

/* FINAL_TABLE is a pointer to an array of 8 funtion pointers represening
 * final subroutines. Each element represents the recpective rank of the
 * dummy argument.
 */
#if defined(TARGET_WIN_X8632)
#define FINAL_TABLE(x) void __stdcall (**x)(char *, char *)
#else
#define FINAL_TABLE(x) void (**x)(char *, char *)
#endif

#define MAX_TYPE_NAME 32

typedef struct object_desc OBJECT_DESC;
typedef struct type_desc TYPE_DESC;
typedef struct layout_desc LAYOUT_DESC;
typedef struct proc_desc PROC_DESC;

/** \brief Layout descriptor
 *
 * A layout descriptor will have a list of the following integers representing
 * information on components in the object. For now, we only specify a
 * layout for pointer/allocatable/finalizable components. The layout
 * descriptor is used with sourced allocation (to allocate/clone nested
 * components in an object) and with final subroutines.
 *
 * There are five tags (used in the tag field) to indicate the type of
 * component we're dealing with:
 *
 * - 'T' => an "allocated" pointer (has allocatable attribute)
 * - 'D' => a regular pointer to a derived type (non-recursive)
 * - 'R' => a "recursive" pointer to a derived type
 * - 'P' => a general purpose pointer. Can be a pointer to an array, a
 *        scalar, etc.
 * - 'F' => a non-pointer/non-allocatable finalizable component
 * - 'S' => procedure pointer
 */
struct layout_desc {
  /**< the tag -- one of T,D,R,P,F,S -- see description above */
  __INT_T tag;
  /**< runtime type of component, base type if pointer (not yet used)*/
  __INT_T type;
  /**< byte offset of component */
  __INT_T offset;
  /**< length in bytes of component. 0 if unknown */
  __INT_T length;
  /**< byte offset of component's descriptor. -1 if N/A */
  __INT_T desc_offset;
  __INT_T padding;
  /**< ptr to declared type or 0 if N/A  */
  POINT(TYPE_DESC, declType);
};

/* The object_desc structure describes an ``OOP'' F2003 object */
struct object_desc {
  /* The n fields between the "Begin overloaded F90_Desc" and
   * "End overloaded F90_Desc" must be consistent in type and
   * length with the first n fields in F90_Desc.
   *
   * Begin overloaded F90_Desc: */
  __INT_T tag;     /* usually __POLY */
  __INT_T baseTag; /* base tag of variable, usually __DERIVED */
  __INT_T level;   /* hierarchical position in the inheritance graph */
  __INT_T size;    /* size of object */
  __INT_T reserved1;
  __INT_T reserved2;
  __INT_T reserved3;
  __INT_T reserved4;
  POINT(__INT_T, prototype); /* address of initialized instance */
  POINT(TYPE_DESC, type); /* address of type of object */
  /* End overloaded F90_Desc */
};

/* The type_desc structure describes the type of an ``OOP'' F2003 object */
struct type_desc /* extends(OBJECT_DESC) */ {
  OBJECT_DESC obj;
  VTABLE(func_table);           /* pointer to virtual function table */
  VTABLE(constructor);          /* pointer to constructor/initializer (TBD) */
  FINAL_TABLE(finals);          /* pointer to VTABLE for final procedures */
  POINT(LAYOUT_DESC, layout);   /* pointer to layout descriptor */
  char name[MAX_TYPE_NAME + 1]; /* null terminated user defined name of type */
};

/* Procedure Pointer/Argument Descriptor */
struct proc_desc {
  __INT_T tag;     /**< usually __PROCPTR */
  __INT_T reserved1;
  __INT_T reserved2; 
  __INT_T reserved3;   
  __INT_T reserved4; 
  __INT_T reserved5;
  __INT_T reserved6;
  __INT_T reserved7;
  POINT(__INT_T, reserved8);
  POINT(void, closure); /**< closure/context pointer */
};

extern void I8(__fort_dump_type)(TYPE_DESC *d); /* type.c */

extern __INT_T ENTF90(GET_OBJECT_SIZE, get_object_size)(F90_Desc *d);
extern __INT8_T ENTF90(KGET_OBJECT_SIZE, kget_object_size)(F90_Desc *d);
void ENTF90(SET_TYPE, set_type)(F90_Desc *dd, OBJECT_DESC *td);

#endif
