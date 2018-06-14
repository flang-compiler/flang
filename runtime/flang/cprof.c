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

/** \brief Profile initialization */
int
__fort_prof_init(void)
{
  return (0);
}

/** \brief Function entry  */
void
__fort_prof_function_entry(int line, int lines, int cline, char *func,
                           char *file, int funcl, int filel)
{
}

/** \brief Line entry  */
void
__fort_prof_line_entry(int line /* current line number */)
{
}

/** \brief Update start receive message stats
 * \param cpu: sending cpu
 * \param len: total length in bytes
 */
void
__fort_prof_recv(int cpu, long len)
{
}

/** \brief Update done receive message stats */
void
__fort_prof_recv_done(int cpu /* sending cpu */)
{
}

/** \brief Update start send message stats
 * \param cpu: receiving cpu
 * \param len: total length in bytes
 */
void
__fort_prof_send(int cpu, long len)
{
}

/** \brief Update done send message stats */
void
__fort_prof_send_done(int cpu /* receiving cpu */)
{
}

/** \brief Update start bcopy message stats
 * \param len: total length in bytes
 */
void
__fort_prof_copy(long len)
{
}

/** \brief Update done bcopy message stats */
void
__fort_prof_copy_done(void)
{
}

/** \brief Function exit  */
void
__fort_prof_function_exit(void)
{
}

/** \brief Profile termination */
void
__fort_prof_term(void)
{
}
