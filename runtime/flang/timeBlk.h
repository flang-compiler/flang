/*
 * Copyright (c) 1995-2018, NVIDIA CORPORATION.  All rights reserved.
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
  * \file
  * \brief tb (time block) struct (all times in seconds)
  */

/** \brief time block (all times in seconds) */
struct tb {
  double r;        /**< real elapsed time */
  double u;        /**< user time */
  double s;        /**< system time */
  double bytes;    /**< number of bytes sent */
  double datas;    /**< number of data items sent */
  double byter;    /**< number of bytes recv`d */
  double datar;    /**< number of data items recv'd */
  double bytec;    /**< number of bytes copied */
  double datac;    /**< number of data items copied */
  double maxrss;   /**< max set size */
  double minflt;   /**< minor fault */
  double majflt;   /**< major fault */
  double nsignals; /**< number of signals */
  double nvcsw;    /**< voluntary switches */
  double nivcsw;   /**< involuntary switches */
  double sbrk;     /**< sbrk value (local heap) */
  double gsbrk;    /**< sbrk value (global heap) */
  char host[256];  /**< hostname */
};
