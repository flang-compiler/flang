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

build:
	@echo ------------------------------------- building test $(TEST)
	$(FC) $(FFLAGS) $(SRC)/$(TEST).f90 -o $(TEST).$(EXE) > $(TEST).rslt 2>&1
	 
run:
	@echo ------------------------------------ executing test $(TEST)
	./$(TEST).$(EXE)
	 
verify: $(TEST).rslt
	@echo ------------------------------------ verifying test $(TEST)
	@if ! grep -q warn $(TEST).rslt; then \
	  echo "PASS"; \
	else \
	  grep -i warn $(TEST).rslt; \
	  echo "FAIL"; \
	fi
