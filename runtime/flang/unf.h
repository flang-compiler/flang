/*
 * Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
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

/** \file
 * \brief Fortran unformatted I/O utility routines defined in unf.c and
 * visible to the rest of the runtime.
 *
 * The calling sequence for these routines is always __f90io_unf_init(),
 * __f90io_unf_read/write()... __f90io_unf_end(). __f90io_unf_init()
 * initializes globals. __f90io_unf_read() reads one record into a
 * buffer the first time it is called and then processes that record
 * in subsequent calls. __f90io_unf_write() collects data in a buffer
 * which is reallocated as needed in subsequent calls.
 * __f90io_unf_end() either writes the buffer to the file in the case
 * of a write, or seeks to the next record (if a variable length file)
 * in the case of a read.  If __fortio_error is called any time during or
 * after an init, and is allowed to return (as opposed to exiting) by
 * iostat, a global variable will be set and all subsequent calls to
 * __f90io_unf_read() or __f90io_unf_write() and __f90io_unf_end()
 * will return with an error.  The next call to __f90io_unf_init()
 * resets these flags.
 */

/** \brief
 * Initialize global flags to prepare for unformatted I/O, and if the
 * file isn't opened, open it (if possible).
 *   \param read    TRUE indicates READ statement
 *   \param unit    unit number
 *   \param rec     record number for direct access
 *   \param bitv    same as for ENTF90IO(open_)
 *   \param iostat  same as for ENTF90IO(open_)
 */
int __f90io_unf_init(__INT_T *read, __INT_T *unit, __INT_T *rec, __INT_T *bitv,
                     __INT_T *iostat);

/** \brief
 * Read/copy data from an unformatted record file.
 * \param type     Type of data long length  number of items of specified
 *                      type to read.  May be <= 0
 * \param length number of items of specified type to read.  May be <= 0
 * \param stride   distance in bytes between items
 * \param item   where to xfer data
 * \param item_length
 */
int __f90io_unf_read(int type, long length, int stride, char *item,
                     __CLEN_T item_length);

/** \brief
 * Write data to an unformatted file.
 * \param type    data type of data (see above).
 * \param count  number of items of specified type to write.  May be <= 0
 * \param stride  distance in bytes between items.
 * \param item  where to get data from
 * \param item_length
 */
int __f90io_unf_write(int type, long count, int stride, char *item,
                      __CLEN_T item_length);

/** \brief
 * Finish up unformatted read or write.  If current I/O is a read,
 * write the current buffer to the file.  Whether a read or a write,
 * free the buffer. */
int __f90io_unf_end(void);

/** \brief
 * Initialize global flags to prepare for byte swapped unformatted I/O, and
 * if the file isn't opened, open it (if possible).
 * \param read    TRUE indicates READ statement.
 * \param unit    unit number.
 * \param rec     record number for direct access
 * \param bitv    same as for ENTF90IO(open_).
 * \param iostat  same as for ENTF90IO(open_).
 */
int __f90io_usw_init(__INT_T *read, __INT_T *unit, __INT_T *rec, __INT_T *bitv,
                     __INT_T *iostat);

/** \brief
 * Read/copy data from an unformatted record file with byte swapping.
 * \param type    Type of data
 * \param count  number of items of specified type to read.  May be <= 0
 * \param stride  distance in bytes between items
 * \param item  where to xfer data
 * \param item_length
 */
int __f90io_usw_read(int type, long count, int stride, char *item,
                     __CLEN_T item_length);

/** \brief
 * Write data to an unformatted file with byte swapping.
 * \param type    data type of data (see above).
 * \param count  number of items of specified type to write.  May be <= 0
 * \param stride  distance in bytes between items.
 * \param item  where to get data from
 * \param item_length
 */
int __f90io_usw_write(int type, long count, int stride, char *item,
                      __CLEN_T item_length);

/** \brief
 * Finish up unformatted, byte swapped read or write.  If current I/O is a
 * read,  write the current buffer to the file.  Whether a read or a write,
 * free the buffer. */
int __f90io_usw_end(void);
