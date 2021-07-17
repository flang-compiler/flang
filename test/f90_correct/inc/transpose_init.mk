#
# Copyright (c) 2016, NVIDIA CORPORATION.  All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

EXE=transpose_init.$(EXESUFFIX)

build:  $(SRC)/transpose_init.f90
	-$(RM) transpose_init.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	-$(RM) $(OBJ)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	@echo ------------------------------------ building test $@
	$(FC) $(FFLAGS) $(LDFLAGS) $(SRC)/transpose_init.f90 check.$(OBJX) -o transpose_init.$(EXESUFFIX)

run:
	@echo ------------------------------------ executing test transpose_init
	transpose_init.$(EXESUFFIX)

verify: ;

transpose_init.run: run
