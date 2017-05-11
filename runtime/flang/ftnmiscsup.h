
/*
 * Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

/* fortran miscellaneous support routines */

typedef int INT;

/**
 * \brief buf receives date in the form dd-mmm-yy trucated or padded to buf_len
 */
void Ftn_date(char *buf, INT buf_len);

/**
 * \brief buffer receives date in the form dd-mmm-yy.
 */
void Ftn_datew(char buf[9]);

/**
 * \brief i,j,k receive integer values for month, day, and year
 */
void Ftn_jdate(INT *i, INT *j, INT *k);

/**
 * \brief i,j,k receive short integer values for month, day, and year
 */
void Ftn_idate(short *i, short *j, short *k);

/**
 * \brief Returns the number of seconds since midnight minus the supplied value
 */
float Ftn_secnds(float x);

/**
 * \brief double precision version of secnds.
 */
double Ftn_dsecnds(double x);

/*
 * \brief buf returns time in the form hh:mm:ss padded or blank filled to
 * 8 characters.
 */
void Ftn_time(char *buf, INT buf_len);

/*
 * \brief buf returns time in the form hh:mm:ss.
 */
void Ftn_timew(char buf[8]);

/** \brief
 *  returns VMS-compatible random number sequence
 */
float Ftn_ran(unsigned *seed);

/**
 * \brief Ftn_dran  double precision version of ran):
 */
double Ftn_dran(unsigned *seed);

#if defined(TARGET_WIN)
/*
 * Miscellaneous support routines for windows '3f-like' routines
 * which are self-contained as opposed to fortran interfaces to
 * C system routines.
 */
void CopyMemory(char *to, char *from, size_t n);
int MakeWord(int lo, int hi);
#endif
