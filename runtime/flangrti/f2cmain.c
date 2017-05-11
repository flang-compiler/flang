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

#include <stdioInterf.h>

/*
 * On linux, linking -g77libs (=> libf2c) may result in an 'undefined
 * reference to MAIN__' which is g77's name for a fortran main program.
 * Just define the function in libpgc.a and hope it's never called.
 */
int
MAIN__()
{
  fprintf(__io_stderr(),
          "MAIN__ called -- missing g77-compiled main program\n");
  return 0;
}
