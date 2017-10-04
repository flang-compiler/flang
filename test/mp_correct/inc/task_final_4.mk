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
build: task_final_4.$(OBJX)

run:
	@echo ------------ executing test $@
	-$(RUN2) ./task_final_4.$(EXESUFFIX) $(LOG)

verify: ;

task_final_4.$(OBJX): $(SRC)/task_final_4.f90 check.$(OBJX)
	@echo ------------ building test $@
	-$(FC) $(FFLAGS) $(SRC)/task_final_4.f90
	@$(RM) ./a.$(EXESUFFIX)
	-$(FC) $(LDFLAGS) task_final_4.$(OBJX) check.$(OBJX) $(LIBS) -o task_final_4.$(EXESUFFIX)

