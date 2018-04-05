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

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

if(NOT ${CMAKE_SYSTEM_NAME} MATCHES "Windows")
  add_subdirectory("acos")
  add_subdirectory("asin")
  add_subdirectory("atan")
  add_subdirectory("atan2")
  add_subdirectory("cos")
  add_subdirectory("cosh")
  add_subdirectory("exp")
  add_subdirectory("log")
  add_subdirectory("log10")
  add_subdirectory("mod")
  add_subdirectory("pow")
  add_subdirectory("powi")
  add_subdirectory("sin")
  add_subdirectory("sincos")
  add_subdirectory("sinh")
  add_subdirectory("tan")
  add_subdirectory("tanh")
endif()

# Get compiler definitions
get_property(FLAGS GLOBAL PROPERTY "FLAGS_L1")
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_L1")

set(SRCS)
if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "x86_64")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    add_subdirectory("cdiv")
    add_subdirectory("cosf")
    add_subdirectory("csqrt")
    add_subdirectory("sincosf")
    add_subdirectory("sinf")
    add_subdirectory("tanf")

    # Specific definition, will probably need to remove when we create a single file for each def
    set_property(SOURCE dispatch.c APPEND_STRING PROPERTY COMPILE_FLAGS "-fno-builtin-c{sqrt,pow,log,exp,acos,asin,atan,cos,sin,tan,cosh,sinh,tanh}{,f}")
    set_property(SOURCE dispatch.c APPEND PROPERTY COMPILE_DEFINITIONS FOR_LIBPGC)
    set_property(SOURCE mth_xintrinsics.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=core2 ")
    set_property(SOURCE mth_yintrinsics.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=sandybridge ")
    set_property(SOURCE mth_zintrinsics.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mtune=knl -march=knl ")
    set_property(SOURCE mth_vreturns.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=core2 ")
    set_property(SOURCE p_dpowi.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=core2 ")
    set_property(SOURCE p_rpowi.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=core2 ")

    set_property(SOURCE mth_128defs.c mth_128defs_init.c mth_128defs_stats.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=core2 ")
    set_property(SOURCE mth_128defs_init.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_INIT)
    set_property(SOURCE mth_128defs_stats.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_STATS)

    set_property(SOURCE mth_256defs.c mth_256defs_init.c mth_256defs_stats.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=sandybridge ")
    set_property(SOURCE mth_256defs_init.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_INIT)
    set_property(SOURCE mth_256defs_stats.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_STATS)

    set_property(SOURCE mth_512defs.c mth_512defs_init.c mth_512defs_stats.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mtune=knl -march=knl ")
    set_property(SOURCE mth_512defs_init.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_INIT)
    set_property(SOURCE mth_512defs_stats.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_STATS)
    set(MTH_INTRINSICS_SRCS
      mth_128defs.c
      mth_128defs_init.c
      mth_128defs_stats.c
      mth_256defs.c
      mth_256defs_init.c
      mth_256defs_stats.c
      mth_512defs.c
      mth_512defs_init.c
      mth_512defs_stats.c)
    libmath_add_object_library("${MTH_INTRINSICS_SRCS}" "${FLAGS}" "${DEFINITIONS}" "mth_intrinsics")

    set(SRCS
      kmax.c
      kmin.c
      mth_yintrinsics.c
      mth_zintrinsics.c)
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    add_subdirectory("cdiv")
    add_subdirectory("cosf")
    add_subdirectory("csqrt")
    add_subdirectory("sincosf")
    add_subdirectory("sinf")
    add_subdirectory("tanf")

    # Specific definition, will probably need to remove when we create a single file for each def
    set_property(SOURCE dispatch.c APPEND_STRING PROPERTY COMPILE_FLAGS "-fno-builtin-c{sqrt,pow,log,exp,acos,asin,atan,cos,sin,tan,cosh,sinh,tanh}{,f}")
    set_property(SOURCE dispatch.c APPEND PROPERTY COMPILE_DEFINITIONS FOR_LIBPGC)
    set_property(SOURCE mth_xintrinsics.c APPEND_STRING PROPERTY COMPILE_FLAGS "-msse2 ")
    set_property(SOURCE mth_yintrinsics.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx ")
    set_property(SOURCE mth_zintrinsics.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx512f -mfma ")
    set_property(SOURCE mth_vreturns.c APPEND_STRING PROPERTY COMPILE_FLAGS "-msse2 ")
    set_property(SOURCE p_dpowi.c APPEND_STRING PROPERTY COMPILE_FLAGS "-msse2 ")
    set_property(SOURCE p_rpowi.c APPEND_STRING PROPERTY COMPILE_FLAGS "-msse2 ")

    set_property(SOURCE mth_128defs.c mth_128defs_init.c mth_128defs_stats.c APPEND_STRING PROPERTY COMPILE_FLAGS "-msse2 ")
    set_property(SOURCE mth_128defs_init.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_INIT)
    set_property(SOURCE mth_128defs_stats.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_STATS)

    set_property(SOURCE mth_256defs.c mth_256defs_init.c mth_256defs_stats.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx ")
    set_property(SOURCE mth_256defs_init.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_INIT)
    set_property(SOURCE mth_256defs_stats.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_STATS)

    set_property(SOURCE mth_512defs.c mth_512defs_init.c mth_512defs_stats.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx512f ")
    set_property(SOURCE mth_512defs_init.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_INIT)
    set_property(SOURCE mth_512defs_stats.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_STATS)
    set(MTH_INTRINSICS_SRCS
      mth_128defs.c
      mth_128defs_init.c
      mth_128defs_stats.c
      mth_256defs.c
      mth_256defs_init.c
      mth_256defs_stats.c
      mth_512defs.c
      mth_512defs_init.c
      mth_512defs_stats.c)
    libmath_add_object_library("${MTH_INTRINSICS_SRCS}" "${FLAGS}" "${DEFINITIONS}" "mth_intrinsics")

    set(SRCS
      kmax.c
      kmin.c
      mth_yintrinsics.c
      mth_zintrinsics.c)
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
  endif()
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le")
  set_property(SOURCE dispatch.c APPEND_STRING PROPERTY COMPILE_FLAGS "-fno-builtin")
  set_property(SOURCE mth_xintrinsics.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mcpu=power8 ")
  set_property(SOURCE mth_128defs.c mth_128defs_init.c mth_128defs_stats.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mcpu=power8 ")
  set_property(SOURCE mth_128defs_init.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_INIT)
  set_property(SOURCE mth_128defs_stats.c APPEND PROPERTY COMPILE_DEFINITIONS MTH_I_INTRIN_STATS)
  set(MTH_INTRINSICS_SRCS
    mth_128defs.c
    mth_128defs_init.c
    mth_128defs_stats.c)
  libmath_add_object_library("${MTH_INTRINSICS_SRCS}" "${FLAGS}" "${DEFINITIONS}" "mth_intrinsics")

  set(SRCS
    kidnnt.c
    merge.c
    sincos.c
    sincosf.c)
endif()

# Set source files
set(MTH_CMPLX_SRCS
  cabs.c
  cacos.c
  casin.c
  catan.c
  ccos.c
  ccosh.c
  cdabs.c
  cdacos.c
  cdasin.c
  cdatan.c
  cdcos.c
  cdcosh.c
  cddiv.c
  cddivd.c
  cdexp.c
  cdiv.c
  cdivr.c
  cdlog.c
  cdpowcd.c
  cdpowi.c
  cdpowk.c
  cdsin.c
  cdsinh.c
  cdsqrt.c
  cdtan.c
  cdtanh.c
  cexp.c
  clog.c
  cpowc.c
  cpowi.c
  cpowk.c
  csin.c
  csinh.c
  csqrt.c
  ctan.c
  ctanh.c)

set(SRCS
  ${SRCS}
  acosd.c
  amod.c
  asind.c
  atan2d.c
  atand.c
  bessel_j0.c
  bessel_j1.c
  bessel_jn.c
  bessel_y0.c
  bessel_y1.c
  bessel_yn.c
  ${MTH_CMPLX_SRCS}
  cosd.c
  cvt.c
  dacosd.c
  dasind.c
  datan2d.c
  datand.c
  dbessel_j0.c
  dbessel_j1.c
  dbessel_jn.c
  dbessel_y0.c
  dbessel_y1.c
  dbessel_yn.c
  dcosd.c
  dmod.c
  dpowi.c
  dpowk.c
  dsign.c
  dsind.c
  dtand.c
  erf.c
  erfc.c
  erfc_scaled.c
  erfc_scaledf.c
  erfcf.c
  erff.c
  fpcvt.c
  gamma.c
  gammaf.c
  hypot.c
  hypotf.c
  i1shft.c
  i2powi.c
  iishft.c
  ileadz.c
  ileadzi.c
  ipopcnt.c
  ipopcnti.c
  ipoppar.c
  ipoppari.c
  ipowi.c
  kabs.c
  kidim.c
  kisign.c
  kleadz.c
  kpopcnt.c
  kpoppar.c
  kpowi.c
  kpowk.c
  log_gamma.c
  log_gammaf.c
  mod.c
  mth_vreturns.c
  mth_xintrinsics.c
  p_dpowi.c
  p_rpowi.c
  rpowi.c
  rpowk.c
  sign.c
  sind.c
  tand.c)
libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")

set(DEFINITIONS_CMPLX ${DEFINITIONS} MTH_CMPLX_C99_ABI)
libmath_add_object_library("${MTH_CMPLX_SRCS}" "${FLAGS}" "${DEFINITIONS_CMPLX}" "common_mth_cmplx")

if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "x86_64")
  # Generate mth_128mask.c
  set(TARGET_NAME "mth_128mask")
  add_custom_command(OUTPUT ${TARGET_NAME}.c PRE_BUILD
    COMMAND awk -v MAX_VREG_SIZE=128 -v TARGET=X8664 -f ${LIBPGMATH_TOOLS_DIR}/mth_mask.awk > ${TARGET_NAME}.c)
  add_custom_target(${TARGET_NAME} ALL DEPENDS "${TARGET_NAME}.c")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=core2")
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-msse2")
  endif()
  libmath_add_object_library("${TARGET_NAME}.c" "${FLAGS}" "${DEFINITIONS}" "${TARGET_NAME}_build")
  add_dependencies("${TARGET_NAME}_build" ${TARGET_NAME})

  # Generate mth_128generic.c
  set(TARGET_NAME "mth_128generic")
  add_custom_command(OUTPUT ${TARGET_NAME}.c PRE_BUILD
    COMMAND awk -v MAX_VREG_SIZE=128 -v TARGET=X8664 -f ${LIBPGMATH_TOOLS_DIR}/mth_generic_frp.awk > ${TARGET_NAME}.c)
  add_custom_target(${TARGET_NAME} ALL DEPENDS "${TARGET_NAME}.c")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=core2")
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-msse2")
  endif()
  libmath_add_object_library("${TARGET_NAME}.c" "${FLAGS}" "${DEFINITIONS}" "${TARGET_NAME}_build")
  add_dependencies("${TARGET_NAME}_build" ${TARGET_NAME})

  # Generate mth_256mask.c
  set(TARGET_NAME "mth_256mask")
  add_custom_command(OUTPUT ${TARGET_NAME}.c PRE_BUILD
    COMMAND awk -v MAX_VREG_SIZE=256 -v TARGET=X8664 -f ${LIBPGMATH_TOOLS_DIR}/mth_mask.awk > ${TARGET_NAME}.c)
  add_custom_target(${TARGET_NAME} ALL DEPENDS "${TARGET_NAME}.c")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=sandybridge")
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx")
  endif()
  libmath_add_object_library("${TARGET_NAME}.c" "${FLAGS}" "${DEFINITIONS}" "${TARGET_NAME}_build")
  add_dependencies("${TARGET_NAME}_build" ${TARGET_NAME})

  # Generate mth_256generic.c
  set(TARGET_NAME "mth_256generic")
  add_custom_command(OUTPUT ${TARGET_NAME}.c PRE_BUILD
    COMMAND awk -v MAX_VREG_SIZE=256 -v TARGET=X8664 -f ${LIBPGMATH_TOOLS_DIR}/mth_generic_frp.awk > ${TARGET_NAME}.c)
  add_custom_target(${TARGET_NAME} ALL DEPENDS "${TARGET_NAME}.c")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-march=sandybridge")
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx")
  endif()
  libmath_add_object_library("${TARGET_NAME}.c" "${FLAGS}" "${DEFINITIONS}" "${TARGET_NAME}_build")
  add_dependencies("${TARGET_NAME}_build" ${TARGET_NAME})

  # Generate mth_512mask.c
  set(TARGET_NAME "mth_512mask")
  add_custom_command(OUTPUT ${TARGET_NAME}.c PRE_BUILD
    COMMAND awk -v MAX_VREG_SIZE=512 -v TARGET=X8664 -f ${LIBPGMATH_TOOLS_DIR}/mth_mask.awk > ${TARGET_NAME}.c)
  add_custom_target(${TARGET_NAME} ALL DEPENDS "${TARGET_NAME}.c")
  set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mtune=knl -march=knl")
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx512f -mfma")
  endif()
  libmath_add_object_library("${TARGET_NAME}.c" "${FLAGS}" "${DEFINITIONS}" "${TARGET_NAME}_build")
  add_dependencies("${TARGET_NAME}_build" ${TARGET_NAME})

  # Generate mth_512generic.c
  set(TARGET_NAME "mth_512generic")
  add_custom_command(OUTPUT ${TARGET_NAME}.c PRE_BUILD
    COMMAND awk -v MAX_VREG_SIZE=512 -v TARGET=X8664 -f ${LIBPGMATH_TOOLS_DIR}/mth_generic_frp.awk > ${TARGET_NAME}.c)
  add_custom_target(${TARGET_NAME} ALL DEPENDS "${TARGET_NAME}.c")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mtune=knl -march=knl")
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx512f -mfma")
  endif()
  libmath_add_object_library("${TARGET_NAME}.c" "${FLAGS}" "${DEFINITIONS}" "${TARGET_NAME}_build")
  add_dependencies("${TARGET_NAME}_build" ${TARGET_NAME})

  # Generate mth_z2yy.c
  set(TARGET_NAME "mth_z2yy")
  add_custom_command(OUTPUT ${TARGET_NAME}.c PRE_BUILD
    COMMAND awk -v MAX_VREG_SIZE=512 -v TARGET=X8664 -f ${LIBPGMATH_TOOLS_DIR}/mth_z2yy.awk > ${TARGET_NAME}.c)
  add_custom_target(${TARGET_NAME} ALL DEPENDS "${TARGET_NAME}.c")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mtune=knl -march=knl")
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_property(SOURCE ${TARGET_NAME}.c APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx512f -mfma")
  endif()
  libmath_add_object_library("${TARGET_NAME}.c" "${FLAGS}" "${DEFINITIONS}" "${TARGET_NAME}_build")
  add_dependencies("${TARGET_NAME}_build" ${TARGET_NAME})
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le")
  # Generate mth_128mask.c
  set(TARGET_NAME "mth_128mask")
  add_custom_command(OUTPUT ${TARGET_NAME}.c PRE_BUILD
    COMMAND awk -v MAX_VREG_SIZE=128 -v TARGET=POWER -f ${LIBPGMATH_TOOLS_DIR}/mth_mask.awk > ${TARGET_NAME}.c)
  add_custom_target(${TARGET_NAME} ALL DEPENDS "${TARGET_NAME}.c")
  libmath_add_object_library("${TARGET_NAME}.c" "${FLAGS}" "${DEFINITIONS}" "${TARGET_NAME}_build")
  add_dependencies("${TARGET_NAME}_build" ${TARGET_NAME})

  # Generate mth_128generic.c
  set(TARGET_NAME "mth_128generic")
  add_custom_command(OUTPUT ${TARGET_NAME}.c PRE_BUILD
    COMMAND awk -v MAX_VREG_SIZE=128 -v TARGET=POWER -f ${LIBPGMATH_TOOLS_DIR}/mth_generic_frp.awk > ${TARGET_NAME}.c)
  add_custom_target(${TARGET_NAME} ALL DEPENDS "${TARGET_NAME}.c")
  libmath_add_object_library("${TARGET_NAME}.c" "${FLAGS}" "${DEFINITIONS}" "${TARGET_NAME}_build")
  add_dependencies("${TARGET_NAME}_build" ${TARGET_NAME})
endif()

# Generate tmp-mth_statsdefs.h
set(TARGET_NAME "tmp-mth_statsdefs")
add_custom_command(OUTPUT ${TARGET_NAME}.h PRE_BUILD
  COMMAND sh "${LIBPGMATH_TOOLS_DIR}/${TARGET_NAME}_${CMAKE_SYSTEM_PROCESSOR}.sh" ${CMAKE_CURRENT_SOURCE_DIR} ${TARGET_NAME}.h)

add_custom_target(${TARGET_NAME} ALL DEPENDS "${TARGET_NAME}.h")
libmath_add_object_library("dispatch.c" "${FLAGS}" "${DEFINITIONS}" "dispatch")
add_dependencies(dispatch ${TARGET_NAME} tmp-mth_alldefs)
target_include_directories(dispatch
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR}
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../${CMAKE_SYSTEM_PROCESSOR}/math_tables)
