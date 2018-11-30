#
# Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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

$(TEST): $(TEST).$(OBJX)
	@echo ------------ executing test $@
	-$(RUN1) ./$(TEST).$(EXESUFFIX) $(LOG)

$(TEST).$(OBJX): $(SRC)/src/$(TEST).f90 check.$(OBJX)
	@echo ------------ building test $@
	@echo $(FLAGS)
	-$(FC) -c $(MPFLAGS) $(OPT) -I$(SRC) $(SRC)/src/$(TEST).f90
	@$(RM) ./$(TEST).$(EXESUFFIX)
	-$(FC) $(MPFLAGS) $(OPT) $(TEST).$(OBJX) check.$(OBJX) $(LIBS) -o $(TEST).$(EXESUFFIX)

build: $(TEST)
run: ;
