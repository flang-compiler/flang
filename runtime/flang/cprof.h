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

#define VERSION 0 /* file format version */

/* profiling data for lines and routines */

struct cinfo {
  double count; /* execution count */
  double cost;  /* cost in seconds */
  double time;  /* time used in seconds */
  double datas; /* number of data vectors sent */
  double bytes; /* number of bytes sent */
  double datar; /* number of data vectors received */
  double byter; /* number of bytes received */
  double datac; /* number of data vectors copied */
  double bytec; /* number of bytes copied */
};

/* function entry */

struct cprof {
  char *func;         /* function name */
  int funcl;          /* length of above */
  char *file;         /* file name */
  int filel;          /* length of above */
  int line;           /* beginning line number of routine */
  int lines;          /* number of lines in routine */
  struct cinfo d;     /* routine info */
  struct cinfo *cptr; /* pointer to vector of lines info */
};
