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

/*
 * STG_DECLARE(name, datatype) - declare structure
 * STG_ALLOC(name, datatype, size) - allocate
 * STG_CLEAR(name) - clear all fields up to stg_avail
 * STG_DELETE(name) - deallocate
 * i = STG_NEXT(name, datatype) - return next available index (no free list)
 * i = STG_NEXT_SIZE(name, size) - return next available index, allocate size
 * i = STG_NEXT_FREELIST(name, datatype) - return index from free list
 * STG_NEED(name, datatype) - test avail vs size, realloc if needed
 * STG_ADD_FREELIST(name, i) - add to free list
 * STG_ALLOC_SIDECAR(basename, name, datatype)
 *   allocate name the same size as basename
 *   register name on the sidecar list of basename
 * STG_DELETE_SIDECAR(basename, name)
 *   remove name from the sidecar list of basename
 *   deallocate name
 */

/* declare:
 *  struct{
 *     dt* stg_base;
 *     unsigned int stg_size, stg_avail, stg_free, stg_cleared,
 * stg_dtsize;
 *     void* stg_sidecar; *   }name; */

/* declare the stg_ members; useful in a struct that also has other members */
#define STG_MEMBERS(dt)                                                        \
    dt *stg_base;                                                              \
    unsigned int stg_size, stg_avail, stg_free, stg_cleared,                   \
        stg_dtsize;                                                            \
    char *stg_name;                                                            \
    void *stg_sidecar

/* to statically initialize STG_MEMBERS */
#define STG_INIT  NULL, 0, 0, 0, 0, 0, NULL, NULL

/* declare a struct with the stg_members */
#define STG_DECLARE(name, dt)                                                  \
  struct {                                                                     \
    STG_MEMBERS(dt);                                                           \
  } name

typedef STG_DECLARE(STG, void);

/* allocate the above structure
 * clear all fields
 * allocate stg_base
 * set stg_size, stg_avail
 * clear element zero */
void stg_alloc(STG* stg, int dtsize, int size, char* name);
#define STG_ALLOC(name, dt, size)                                              \
  stg_alloc((STG*)&name, sizeof(dt), size, #name)

/* clear a single field */
void stg_clear(STG* stg, int r, int n);
#define STG_CLEAR(name, r)                                                     \
  stg_clear((STG*)&name, r, 1)

/* clear a number of fields */
#define STG_CLEAR_N(name, r, n)                                                \
  stg_clear((STG*)&name, r, n)

/* clear all allocated fields */
void stg_clear_all(STG* stg);
#define STG_CLEAR_ALL(name)                                                    \
  stg_clear_all((STG*)&name);

/* delete the data structure */
void stg_delete(STG* stg);
#define STG_DELETE(name)                                                       \
  stg_delete((STG*)&name);

/* allocate one element at stg_avail */
int stg_next(STG* stg, int n);
#define STG_NEXT(name)                                                         \
  stg_next((STG*)&name, 1)

/* allocate 'size' elements at stg_avail */
#define STG_NEXT_SIZE(name, size)                                              \
  stg_next((STG*)&name, size)

/* check that stg_avail does not overflow stg_size */
void stg_need(STG* stg);
#define STG_NEED(name)                                                         \
  stg_need((STG*)&name)

/* get the next element from free list, if any, otherwise, from stg_avail */
int stg_next_freelist(STG* stg);
#define STG_NEXT_FREELIST(name)                                                \
  stg_next_freelist((STG*)&name)

/* put this element on the free list */
void stg_add_freelist(STG* stg, int r);
#define STG_ADD_FREELIST(name, index)                                          \
  stg_add_freelist((STG*)&name, index)

/* allocate sidecar the same size as name */
void stg_alloc_sidecar(STG* basestg, STG* stg, int dtsize, char* name);
#define STG_ALLOC_SIDECAR(basename, name, dt)                                  \
  stg_alloc_sidecar((STG*)&basename, (STG*)&name, sizeof(dt), #name)

/* deallocate sidecar */
void stg_delete_sidecar(STG* basestg, STG* stg);
#define STG_DELETE_SIDEVAR(basename, name)                                     \
  stg_delete_sidecar((STG*)&basename, (STG*)&name);
