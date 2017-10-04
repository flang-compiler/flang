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
par12: par12.$(OBJX)
	@echo ------------ executing test $@
	-$(RUN4) ./a.$(EXESUFFIX) $(LOG)
par12.$(OBJX): $(SRC)/par12.f check.$(OBJX)
	@echo ------------ building test $@
	-$(FC) $(FFLAGS) $(SRC)/par12.f
	@$(RM) ./a.$(EXESUFFIX)
	-$(FC) $(LDFLAGS) par12.$(OBJX) check.$(OBJX) $(LIBS) -o a.$(EXESUFFIX)
build: par12
run: ;
