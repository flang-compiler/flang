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
mul01: mul01.$(OBJX)
	@echo ------------ executing test $@
	$(RUN2) a.$(EXESUFFIX) $(LOG)
mul01.$(OBJX): $(SRC)/mul01.f check.$(OBJX)
	$(FC) $(FFLAGS) $(SRC)/mul01.f
	@$(RM) a.$(EXESUFFIX)
	$(FC) $(LDFLAGS) mul01.$(OBJX) check.$(OBJX) $(LIBS) -o a.$(EXESUFFIX)
build: mul01
run: ;
