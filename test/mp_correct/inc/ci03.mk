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
SRC2 = $(SRC)/src
FFLAGS += -Mpreprocess

build: check.$(OBJX)
	-$(FC) $(FFLAGS) $(SRC2)/ci03.f90 -I$(SRC2) -o ci03.$(OBJX)
	-$(FC) $(MPFLAGS) ci03.$(OBJX) $(OPT) check.$(OBJX) -I$(SRC2) -o ci03.$(EXE)

run:
	ci03.$(EXE)
