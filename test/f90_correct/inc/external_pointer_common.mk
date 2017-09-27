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

OBJ=external_pointer_common.$(OBJX)

build:  $(SRC)/external_pointer_common.f90
	-$(RM) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	-$(RM) $(OBJ)
	@echo ------------------------------------ building test $@
	$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/external_pointer_common.f90 -o $(OBJ)

run:
	@echo nothing to run

verify: $(OBJ)
	@echo PASS

