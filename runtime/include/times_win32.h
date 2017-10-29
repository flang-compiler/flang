#ifndef _FLANG_TIMES_WIN32
#define _FLANG_TIMES_WIN32
  #include <Windows.h>

  typedef __int64 clock_t;

  typedef struct tms {
    clock_t tms_utime;  /* user time */
    clock_t tms_stime;  /* system time */
    clock_t tms_cutime; /* user time of children */
    clock_t tms_cstime; /* system time of children */
  } tms;

  clock_t convert_filetime( const FILETIME *ac_FileTime );

  /*
    Thin emulation of the unix times function
  */
  void times(tms *time_struct);
#endif
