#
# Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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
dt00: dt00.$(OBJX) check.$(OBJX)
	@echo ------------ executing test $@
	@$(RM) a.$(EXESUFFIX)
	$(F90) $(LDFLAGS) dt00.$(OBJX) check.$(OBJX) $(LIBS) -o a.$(EXESUFFIX)
	$(RUN2) a.$(EXESUFFIX)
dt00.$(OBJX): $(SRC)/dt00.f90
	$(F90) $(FFLAGS) $(SRC)/dt00.f90
build: dt00
run: ;
