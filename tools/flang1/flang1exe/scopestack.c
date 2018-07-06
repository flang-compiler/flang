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

/**
    \file
    \brief Manage the scope stack.
*/

#include "gbldefs.h"
#include "global.h"
#include "error.h"
#include "symtab.h"
#include "symutl.h"
#include "ccffinfo.h"
#include "semant.h"

static SCOPESTACK *push_scope(void);
static void pop_scope(void);
static const char *kind_to_string(SCOPEKIND kind);

/** \brief Initialize the scope stack.

    It starts with one frame representing the outer scope,
    with no associated symbol.
 */
void
scopestack_init()
{
  SCOPESTACK *scope;
  if (sem.scope_stack == NULL) {
    sem.scope_size = 10;
    NEW(sem.scope_stack, SCOPESTACK, sem.scope_size);
  }
  sem.scope_level = 0;
  sem.scope_extra = 5;
  scope = curr_scope();
  BZERO(scope, SCOPESTACK, 1);
  scope->kind = SCOPE_OUTER;
  scope->symavl = stb.stg_avail;
  scope->sym = 0;
}

/** \brief Return the scope at the top of the scope stack. */
SCOPESTACK *
curr_scope()
{
  return get_scope(0);
}

/** \brief Get an entry in the scope stack.
    \param level Level of the entry to return: 0 means top and negative is
           relative to the top.
 */
SCOPESTACK *
get_scope(int level)
{
  if (level <= 0) {
    level += sem.scope_level;
  }
  if (level < 0 || level >= sem.scope_size) {
#if DEBUG
    dumpscope(gbl.dbgfil);
#endif
    interr("bad scope stack level", level, ERR_Fatal);
  }
  return &sem.scope_stack[level];
}

/** \brief Return the level of this entry in the scope stack
           or -1 if \a scope is null.
 */
int
get_scope_level(SCOPESTACK *scope)
{
  if (scope == 0) {
    return -1;
  } else {
    int level = scope - sem.scope_stack;
    assert(level >= 0 && level <= sem.scope_level, "bad scope stack level",
           level, ERR_Fatal);
    return level;
  }
}

/** \brief Return the next entry below this one in the scope stack; 0 if none.
    If scope is 0, return the top of the stack.
 */
SCOPESTACK *
next_scope(SCOPESTACK *scope)
{
  int sl = get_scope_level(scope);
  if (sl < 0) {
    return curr_scope();
  } else if (sl == 0) {
    return 0;
  } else {
    return scope - 1;
  }
}

/** \brief Return the next entry below scope that has this sptr assocated with
   it.
           If scope is 0, search from the top of the stack.
 */
SCOPESTACK *
next_scope_sptr(SCOPESTACK *scope, int sptr)
{
  while ((scope = next_scope(scope)) != 0) {
    if (scope->sptr == sptr) {
      return scope;
    }
  }
  return 0;
}

/** \brief Return the next entry below scope that has this kind.
           If scope is 0, search from the top of the stack.
 */
SCOPESTACK *
next_scope_kind(SCOPESTACK *scope, SCOPEKIND kind)
{
  while ((scope = next_scope(scope)) != 0) {
    if (scope->kind == kind) {
      return scope;
    }
  }
  return 0;
}

/** \brief Return the next entry below scope that has this kind and sptr.
           If scope is 0, search from the top of the stack.
 */
SCOPESTACK *
next_scope_kind_sptr(SCOPESTACK *scope, SCOPEKIND kind, int sptr)
{
  while ((scope = next_scope_kind(scope, kind)) != 0) {
    if (scope->sptr == sptr) {
      return scope;
    }
  }
  return 0;
}

/** \brief Return the next entry below scope that has this kind and symbol name.
           If scope is 0, search from the top of the stack.
 */
SCOPESTACK *
next_scope_kind_symname(SCOPESTACK *scope, SCOPEKIND kind, const char *symname)
{
  while ((scope = next_scope_kind(scope, kind)) != 0) {
    if (strcmp(symname, SYMNAME(scope->sptr)) == 0) {
      return scope;
    }
  }
  return 0;
}

/** \brief Return the USE module scope for the module associated with this
           symbol, or -1 if none.
 */
int
have_use_scope(int sptr)
{
  SCOPESTACK *scope = 0;
  if (sem.scope_stack == NULL) {
    return -1;
  }
  while ((scope = next_scope(scope)) != 0) {
    if (scope->kind == SCOPE_USE && scope->sptr == sptr) {
      return get_scope_level(scope);
    }
    if (!scope->open) {
      break;
    }
  }
  return -1;
}

/** \brief Return TRUE if sptr is in the exception list for this scope
    at this level.
 */
LOGICAL
is_except_in_scope(SCOPESTACK *scope, int sptr)
{
  return sym_in_sym_list(sptr, scope->except);
}

/** \brief Return TRUE if scope is private and has sptr in its 'only' list.
 */
LOGICAL
is_private_in_scope(SCOPESTACK *scope, int sptr)
{
  return scope->Private && !sym_in_sym_list(sptr, scope->only);
}

/** \brief Push an entry on the scope stack with this symbol and kind.
 */
void
push_scope_level(int sptr, SCOPEKIND kind)
{
  SCOPESTACK *scope;
  if (sem.scope_stack == NULL) {
    return;
  }
  scope = push_scope();
  switch (kind) {
  case SCOPE_SUBPROGRAM:
    if (sem.which_pass == 1) {
      setfile(1, SYMNAME(sptr), 0);
    }
    scope->sptr = sptr;
    break;
  case SCOPE_NORMAL:
  case SCOPE_MODULE:
  case SCOPE_USE:
    scope->sptr = sptr;
    break;
  case SCOPE_OUTER:
  case SCOPE_INTERFACE:
  case SCOPE_PAR:
    ++sem.scope_extra;
    scope->sptr = sem.scope_extra;
    break;
  default:
    interr("push_scope_level: unknown scope kind", kind, ERR_Warning);
  }
  /*
   * When entering a parallel scope, the current scope is left to be the
   * scope of the outer nonparallel scope containing the parallel region.
   */
  if (kind != SCOPE_PAR) {
    stb.curr_scope = scope->sptr;
  }
  scope->kind = kind;
  scope->open = TRUE;
  scope->symavl = stb.stg_avail;
  scope->Private = FALSE;
  scope->sym = 0;
  scope->uplevel_sptr = 0;
#if DEBUG
  if (DBGBIT(5, 0x200)) {
    fprintf(gbl.dbgfil, "\n++++++++  push_scope_level(%s)  pass=%d  line=%d\n",
            kind_to_string(kind), sem.which_pass, gbl.lineno);
    dumpscope(gbl.dbgfil);
  }
#endif
}

/** \brief Push an interface entry on the scope stack and mark it closed.
 */
void
push_iface_scope_level()
{
  push_scope_level(0, SCOPE_INTERFACE);
  curr_scope()->open = FALSE;
}

/*
   Pop scope stack until popping off a frame of this kind.
 */
void
pop_scope_level(SCOPEKIND kind)
{
  if (sem.scope_stack == NULL) {
    return;
  }
  if (sem.scope_level == 0) {
    interr("trying to pop too many scope levels", sem.scope_level, ERR_Severe);
    return;
  }

  /* pop scope stack until popping a frame with this kind */
  for (;;) {
    int newscope;
    /* pop symbols */
    int top = curr_scope()->symavl;
    int scope = curr_scope()->sptr;
    SCOPEKIND curr_kind = curr_scope()->kind;
    switch (curr_kind) {
    case SCOPE_INTERFACE:
      newscope = get_scope(-1)->sptr;
      if (newscope != scope) {
        int sptr;
        for (sptr = stb.stg_avail - 1; sptr >= top; --sptr) {
          if (SCOPEG(sptr) == scope) {
            /* rehost to outer level */
            SCOPEP(sptr, newscope);
          }
        }
      }
      break;
    default:
      if (sem.interface && STYPEG(scope) != ST_MODULE) {
        int sptr;
        /* in an interface block; remove the symbols */
        for (sptr = stb.stg_avail - 1; sptr >= top; --sptr) {
          if (SCOPEG(sptr) == scope) {
            IGNOREP(sptr, 1);
          }
        }
      }
      break;
    }
    pop_scope();
    if (curr_kind == kind) {
      break;
    }
  }
  /*
   * When leaving a parallel scope, the current scope doesn't need to be
   * reset since it should always be the scope of the nonparallel region
   * containing the parallel region.
   */
  if (kind != SCOPE_PAR) {
    if (sem.scope_level > 0) {
      stb.curr_scope = curr_scope()->sptr;
    } else { /* leave scope symbol as most recent one */
      stb.curr_scope = sem.scope_stack[1].sptr;
    }
  }
#if DEBUG
  if (DBGBIT(5, 0x200)) {
    fprintf(gbl.dbgfil, "\n--------  pop_scope_level(%s)  pass=%d  line=%d\n",
            kind_to_string(kind), sem.which_pass, gbl.lineno);
    dumpscope(gbl.dbgfil);
  }
#endif
}

static SCOPESTACK saved_scope_stack[1];
static int count_scope_saved = 0;

/** \brief Pop the current scope into a save area; restore with
 * restore_scope_level() */
void
save_scope_level(void)
{
  if (count_scope_saved >= 1) {
    interr("trying to save too many scope levels", count_scope_saved, 3);
    return;
  }
  saved_scope_stack[count_scope_saved++] = *curr_scope();
  pop_scope();
  stb.curr_scope = curr_scope()->sptr;
#if DEBUG
  if (DBGBIT(5, 0x200)) {
    fprintf(gbl.dbgfil, "\n--------  save_scope_level  pass=%d  line=%d\n",
            sem.which_pass, gbl.lineno);
    dumpscope(gbl.dbgfil);
  }
#endif
}

/** \brief Restore the scope that was saved by save_scope_level() */
void
restore_scope_level(void)
{
  if (count_scope_saved <= 0) {
    interr("trying to restore too many scope levels", count_scope_saved, 3);
    return;
  }
  *push_scope() = saved_scope_stack[--count_scope_saved];
  stb.curr_scope = curr_scope()->sptr;
#if DEBUG
  if (DBGBIT(5, 0x200)) {
    fprintf(gbl.dbgfil, "\n++++++++  restore_scope_level  pass=%d  line=%d\n",
            sem.which_pass, gbl.lineno);
    dumpscope(gbl.dbgfil);
  }
#endif
}

void
par_push_scope(LOGICAL bind_to_outer)
{
  SCOPESTACK *scope, *next_scope;
  SC_KIND prev_sc = sem.sc;
  if (curr_scope()->kind != SCOPE_PAR && sem.parallel >= 1) {
    sem.sc = SC_PRIVATE;
  } else if (sem.task) {
    sem.sc = SC_PRIVATE;
  }
  else if (sem.teams >= 1) {
    sem.sc = SC_PRIVATE;
  } else if (sem.target && sem.parallel >= 1) {
    sem.sc = SC_PRIVATE;
  }
  push_scope_level(0, SCOPE_PAR);
  scope = curr_scope();
  next_scope = get_scope(-1); /* next to top of stack */
  if (!bind_to_outer || next_scope->kind != SCOPE_PAR) {
    scope->rgn_scope = sem.scope_level;
    scope->par_scope = PAR_SCOPE_SHARED;
  } else {
    scope->rgn_scope = next_scope->rgn_scope;
    scope->par_scope = next_scope->par_scope;
    scope->end_prologue = next_scope->end_prologue;
  }
  scope->di_par = sem.doif_depth;
  scope->shared_list = NULL;
  scope->prev_sc = prev_sc;
  enter_lexical_block(flg.debug && !XBIT(123, 0x400));
}

void
par_pop_scope(void)
{
  SCOPE_SYM *symp;
  int blksym;
  /*
   * Restore the scope of any symbols which appeared in a SHARED
   * clause -- this is only needed if the DEFAULT scope is 'PRIVATE' or
   * 'NONE".
   */
  for (symp = curr_scope()->shared_list; symp != NULL; symp = symp->next) {
    SCOPEP(symp->sptr, symp->scope);
  }
  blksym = curr_scope()->sym;
  if (blksym) {
    exit_lexical_block(flg.debug && !XBIT(123, 0x400));
  }

  sem.sc = curr_scope()->prev_sc;
  pop_scope_level(SCOPE_PAR);
  if (curr_scope()->kind != SCOPE_PAR) {
    sem.sc = SC_LOCAL;
  }
}

static SCOPESTACK *
push_scope(void)
{
  ++sem.scope_level;
  NEED(sem.scope_level + 1, sem.scope_stack, SCOPESTACK, sem.scope_size,
       sem.scope_size + 10);
  BZERO(sem.scope_stack + sem.scope_level, SCOPESTACK, 1);
  return curr_scope();
}

static void
pop_scope(void)
{
  --sem.scope_level;
  assert(sem.scope_level >= 0, "attempt to pop empty scope stack",
         sem.scope_level, ERR_Fatal);
}

#if DEBUG
void
dumpscope(FILE *f)
{
  int sl;
  if (f == NULL) {
    f = stderr;
  }
  if (sem.scope_stack == NULL) {
    fprintf(f, "no scope stack\n");
    return;
  }
  for (sl = 0; sl <= sem.scope_level; ++sl) {
    dump_one_scope(sl, f);
  }
}

void
dump_one_scope(int sl, FILE *f)
{
  SCOPESTACK *scope;
  SPTR sptr;
  if (f == NULL) {
    f = stderr;
  }
  if (sl < 0 || sl >= sem.scope_size) {
    interr("dump_one_scope: bad scope stack level", sl, ERR_Warning);
    return;
  }
  scope = sem.scope_stack + sl;
  sptr = scope->sptr;
  fprintf(f, "%ccope %2d. %-11s %-7s %-8s symavl=%3d  %d=%s\n",
          sem.which_pass ? 'S' : 's', sl, kind_to_string(scope->kind),
          scope->open ? "open" : "closed",
          scope->Private ? "private" : "public",
          scope->symavl, sptr,
          sptr >= stb.firstosym ? SYMNAME(sptr) : "");
  if (scope->except) {
    int ex;
    fprintf(f, "+ except");
    for (ex = scope->except; ex; ex = SYMI_NEXT(ex)) {
      fprintf(f, " %d(%s)", SYMI_SPTR(ex), SYMNAME(SYMI_SPTR(ex)));
    }
    fprintf(f, "\n");
  }
  if (scope->import) {
    int im;
    fprintf(f, "+ import");
    for (im = scope->import; im; im = SYMI_NEXT(im)) {
      fprintf(f, " %d(%s)", SYMI_SPTR(im), SYMNAME(SYMI_SPTR(im)));
    }
    fprintf(f, "\n");
  }
}

static const char *
kind_to_string(SCOPEKIND kind)
{
  switch (kind) {
  case SCOPE_OUTER:      return "Outer";
  case SCOPE_NORMAL:     return "Normal";
  case SCOPE_SUBPROGRAM: return "Subprogram";
  case SCOPE_MODULE:     return "Module";
  case SCOPE_INTERFACE:  return "Interface";
  case SCOPE_USE:        return "Use";
  case SCOPE_PAR:        return "Par";
  default:               return "<unknown>";
  }
}

#endif
