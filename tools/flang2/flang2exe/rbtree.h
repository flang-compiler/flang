/*
 * Copyright (c) 2014-2018, NVIDIA CORPORATION.  All rights reserved.
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

#ifndef RBTREE_H_
#define RBTREE_H_

typedef struct rbtree_root_struct rbroot;
typedef struct rbtree_struct *rbtree;
struct rbtree_root_struct {
  rbtree root;
  size_t blocksize, freesize;
  char *datablocklist, *datafree;
};

/* red-black tree */
struct rbtree_struct {
  rbtree left, right, parent;
  int color;
  int data[];
};

#define RBRED 1
#define RBBLK 2
#define RBCOLOR 3

#define ISBLACK(rb) (rb == NULL || (rb->color == RBBLK))
#define ISRED(rb) (rb != NULL && (rb->color == RBRED))
#define SETBLACK(rb)   \
  if (rb) {            \
    rb->color = RBBLK; \
  }
#define SETRED(rb)     \
  if (rb) {            \
    rb->color = RBRED; \
  }
#define COPYCOLOR(rb, fb) rb->color = fb->color

#define ALN(b, aln) ((((b) + (aln)-1)) & (~((aln)-1)))

typedef int (*rb_compare)(void *, void *);
typedef int (*rb_walk_proc)(rbtree, void *userdata);

/**
   \brief ...
 */
int rb_walk(rbroot *T, rb_walk_proc proc, void *userdata);

/**
   \brief ...
 */
rbtree rb_find(rbroot *T, void *data, rb_compare compare);

/**
   \brief ...
 */
rbtree rb_insert(rbroot *T, size_t datasize, void *data, rb_compare compare);

/**
   \brief ...
 */
void rb_free(rbroot *T);


#endif // RBTREE_H_
