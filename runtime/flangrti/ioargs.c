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

char **__argv_save;
int __argc_save;

/* get saved argv */

char **
__io_get_argv()
{
  return (__argv_save);
}

/* set saved argv */

void
__io_set_argv(char **v)
{
  __argv_save = v;
}

/* get saved argc */

int
__io_get_argc()
{
  return (__argc_save);
}

/* set saved argc */

void
__io_set_argc(int v)
{
  __argc_save = v;
}

