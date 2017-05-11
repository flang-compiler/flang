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
 * these defines allow this module to be compiled twice.  once for
 * 32-bit int indexes and once for 64-bit indexes.
 */

#if   defined(UNSIGNED)
#define SCHED _mp_schedsu
#define STATIC_INIT _mp_scheds_static_initu
#define DYN_INIT _mp_scheds_dyn_initu
#define DYN_RESET _mp_scheds_dyn_resetu
#define GUID_INIT _mp_scheds_guid_initu
#define AUTO_INIT _mp_scheds_auto_initu
#define RUN_INIT _mp_scheds_run_initu
#define INT unsigned int
#define SINT int
#elif defined(REAL8)
#define SCHED _mp_scheds8
#define STATIC_INIT _mp_scheds_static_init8
#define DYN_INIT _mp_scheds_dyn_init8
#define DYN_RESET _mp_scheds_dyn_reset8
#define GUID_INIT _mp_scheds_guid_init8
#define AUTO_INIT _mp_scheds_auto_init8
#define RUN_INIT _mp_scheds_run_init8
#define INT long long
#define SINT long long
#else
#define SCHED _mp_scheds
#define STATIC_INIT _mp_scheds_static_init
#define DYN_INIT _mp_scheds_dyn_init
#define DYN_RESET _mp_scheds_dyn_reset
#define GUID_INIT _mp_scheds_guid_init
#define AUTO_INIT _mp_scheds_auto_init
#define RUN_INIT _mp_scheds_run_init
#define INT int
#define SINT int
#endif

/* the stack structure per thread */

struct it {
  int (*func)();
  int thin;  /* current instance */
  int lcpu;  /* cpu number */
  int ncpus; /* number of cpus */
  INT to;    /* to index */
  INT chunk; /* chunk size */
  INT skip;  /* skip size */
  INT index; /* current index */
  int unused;
  int unused1;
};

/* the static structure per loop */

struct its {
  int sem;           /* semaphore */
  int flags;         /* flags */
  INT index;         /* next chunk index */
  struct its *link;  /* link to next structure */
  struct ods *ods;   /* pointer to ordered section data */
  INT chunk;         /* current chunk size (guided) */
  int spare[1];      /* future use */
  int curin;         /* current instance */
  int thin[MAXCPUS]; /* instance per thread */
};

#define IT_ACTIVE 1 /* loop is active */
#define IT_LINKED 2 /* its is linked */

/* the malloc'ed structure per loop with ordered section */

struct ods {
  int sem;
  int maxin;
  struct in {
    int curin;
    volatile INT from;
    INT to;
    INT incr;
  } in[MAXCPUS];
  struct in *myin[MAXCPUS];
  volatile INT myfrom[MAXCPUS];
};

/* runtime schedule types */

#define SCHED_NONE 0
#define SCHED_STATIC 1
#define SCHED_DYN 2
#define SCHED_GUID 3
#define SCHED_AUTO 4

/* ==== Function to support parallel loops in nested parallel regions ==== */
/* Initialize the storage, not thread safe */
void init_nested_its();
/* Clear ITS storage, not thread safe */
void reset_nested_its();
/* Remove ITS data for a particular team */
void delete_nested_its(struct team *);
/** Get a copy of ITS structure from the storage to swap with existing struct
 * Thread safe. */
struct its *get_nested_its(struct its *);
