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

# Set compiler flags and definitions
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_L1")
get_property(FLAGS GLOBAL PROPERTY "FLAGS_L1")

if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "aarch64" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(SRCS gsincos.cpp)
  set_property(SOURCE ${SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-std=c++11 ")
else()
  set(SRCS gsincos.c)
endif()

# Set source files, compiler flags and definitions
if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "x86_64" AND NOT ${LIBPGMATH_WITH_GENERIC})
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos4")
    set_property(TARGET gssincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=4)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos2")
    set_property(TARGET gdsincos2 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=2)
    set_property(TARGET gssincos4 gdsincos2 APPEND_STRING PROPERTY COMPILE_FLAGS "-march=core2 ")

    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos8")
    set_property(TARGET gssincos8 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=8)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos4")
    set_property(TARGET gdsincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=4)
    set_property(TARGET gssincos8 gdsincos4 APPEND_STRING PROPERTY COMPILE_FLAGS "-march=sandybridge ")

    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos16")
    set_property(TARGET gssincos16 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=16)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos8")
    set_property(TARGET gdsincos8 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=8)
    set_property(TARGET gssincos16 gdsincos8 APPEND_STRING PROPERTY COMPILE_FLAGS "-mtune=knl -march=knl ")

    # Set source files and compilation flags
    set(FLAGS "${FLAGS} -Wno-attributes")
    set(DEFINITIONS "${DEFINITIONS} -DPGI")
    set(SRC_VECTOR fd_sincos_vector.cpp)
    set(SRC_SCALAR fd_sincos_scalar.cpp)
    
    set(AVX2_FLAGS "${FLAGS} -mtune=core-avx2 -march=core-avx2")
    set(AVX512_FLAGS "${FLAGS} -mtune=skylake-avx512 -march=skylake-avx512")
    set(KNL_FLAGS "${FLAGS} -mtune=knl -march=knl")
    
    # SINCOS
    libmath_add_object_library("${SRC_SCALAR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINCOS" "fd_sincos_1_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=2" "fd_sincos_4_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=4" "fd_sincos_8_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX512_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=8" "fd_sincos_16_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${KNL_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=8" "fd_sincos_16_knl")
    
    # COSINE
    libmath_add_object_library("${SRC_SCALAR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DCOSINE" "fd_cos_1_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=2" "fd_cos_4_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=4" "fd_cos_8_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX512_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=8" "fd_cos_16_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${KNL_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=8" "fd_cos_16_knl")
    
    # SINE
    libmath_add_object_library("${SRC_SCALAR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINE" "fd_sin_1_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINE -DVL=2" "fd_sin_4_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINE -DVL=4" "fd_sin_8_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX512_FLAGS}" "${DEFINITIONS} -DSINE -DVL=8" "fd_sin_16_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${KNL_FLAGS}" "${DEFINITIONS} -DSINE -DVL=8" "fd_sin_16_knl")

  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos4")
    set_property(TARGET gssincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=4)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos2")
    set_property(TARGET gdsincos2 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=2)
    set_property(TARGET gssincos4 gdsincos2 APPEND_STRING PROPERTY COMPILE_FLAGS "-msse2 ")

    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos8")
    set_property(TARGET gssincos8 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=8)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos4")
    set_property(TARGET gdsincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=4)
    set_property(TARGET gssincos8 gdsincos4 APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx ")

    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos16")
    set_property(TARGET gssincos16 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=16)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos8")
    set_property(TARGET gdsincos8 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=8)
    set_property(TARGET gssincos16 gdsincos8 APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx512f -mfma ")

    # Set source files and compilation flags
    set(SRC_VECTOR fd_sincos_vector.cpp)
    set(SRC_SCALAR fd_sincos_scalar.cpp)
    
    set(AVX2_FLAGS "${FLAGS} -mtune=core-avx2 -march=core-avx2")
    set(AVX512_FLAGS "${FLAGS} -mtune=skylake-avx512 -march=skylake-avx512")
    set(KNL_FLAGS "${FLAGS} -mtune=knl -march=knl")
    
    # SINCOS
    libmath_add_object_library("${SRC_SCALAR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINCOS" "fd_sincos_1_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=2" "fd_sincos_4_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=4" "fd_sincos_8_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX512_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=8" "fd_sincos_16_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${KNL_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=8" "fd_sincos_16_knl")
    
    # COSINE
    libmath_add_object_library("${SRC_SCALAR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DCOSINE" "fd_cos_1_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=2" "fd_cos_4_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=4" "fd_cos_8_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX512_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=8" "fd_cos_16_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${KNL_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=8" "fd_cos_16_knl")
    
    # SINE
    libmath_add_object_library("${SRC_SCALAR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINE" "fd_sin_1_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINE -DVL=2" "fd_sin_4_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINE -DVL=4" "fd_sin_8_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${AVX512_FLAGS}" "${DEFINITIONS} -DSINE -DVL=8" "fd_sin_16_avx2")
    libmath_add_object_library("${SRC_VECTOR}" "${KNL_FLAGS}" "${DEFINITIONS} -DSINE -DVL=8" "fd_sin_16_knl")


  endif()
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set_property(SOURCE ${SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mcpu=power8 ")

  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos4")
  set_property(TARGET gssincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=sincos PRECSIZE=8 VLEN=2)

  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos2")
  set_property(TARGET gdsincos2 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=sincosf PRECSIZE=4 VLEN=4)
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "aarch64" AND NOT ${LIBPGMATH_WITH_GENERIC})
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos4")
  set_property(TARGET gssincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=sincos PRECSIZE=8 VLEN=2)

  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos2")
  set_property(TARGET gdsincos2 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=sincosf PRECSIZE=4 VLEN=4)
else()
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos4")
  set_property(TARGET gssincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=sincos PRECSIZE=8 VLEN=2)

  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos2")
  set_property(TARGET gdsincos2 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=sincosf PRECSIZE=4 VLEN=4)
endif()
