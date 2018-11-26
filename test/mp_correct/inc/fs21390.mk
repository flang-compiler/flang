#
# Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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
SRC2=$(SRC)/src
fs21390.$(OBJX): $(SRC2)/fs21390.f90
	@echo ------------ building test $@
	-$(FC) $(FFLAGS) $(SRC2)/fs21390.f90 -o fs21390.$(OBJX)
	-$(FC) $(LDFLAGS) fs21390.$(OBJX) -o fs21390.out

fs21390: fs21390.$(OBJX)
	-rm -f fs21390_run.log
	$(RUN4) fs21390.out 2>&1 > fs21390_run.log
	-$(NGREP) DEALLOCATE fs21390_run.log

build: fs21390

run: ;
