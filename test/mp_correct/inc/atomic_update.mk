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
build: atomic_update.$(OBJX)

run:
	@echo ------------ executing test $@
	-$(RUN2) ./atomic_update.$(EXESUFFIX) $(LOG)

verify: ;

atomic_update.$(OBJX): $(SRC)/atomic_update.f check.$(OBJX)
	@echo ------------ building test $@
	-$(FC) $(FFLAGS) $(SRC)/atomic_update.f
	@$(RM) ./a.$(EXESUFFIX)
	-$(FC) $(LDFLAGS) atomic_update.$(OBJX) check.$(OBJX) $(LIBS) -o atomic_update.$(EXESUFFIX)

