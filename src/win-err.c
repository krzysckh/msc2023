#ifdef WIN32

#include <stdlib.h>
#include <stdio.h>

void errx(int v, char *fmt, va_list vl)
{
  vfprintf(stderr, fmt, vl);

  exit(v);
}

#endif
