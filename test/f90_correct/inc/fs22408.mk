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

########## Make rule for 22408 ##########
FILE = fs22408
SRC2=$(SRC)
EXT = f90
F90= $(FC)

build: $(SRC2)/$(FILE).$(EXT)
	@echo ----------------------------------------- building test $@
	$(FC) $(SRC2)/$(FILE).$(EXT) $(EXTRA_LDFLAGS) -o ./$(FILE).$(EXE)

run:
	@echo ------------------------------------------ nothing to run
	./$(FILE).$(EXE)

verify:
