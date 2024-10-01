#ifndef COMMON_H_
#define COMMON_H_

#include <stdio.h>
#include <stdlib.h>

#define perror_die(msg)                                                        \
  {                                                                            \
    perror(msg);                                                               \
    exit(EXIT_FAILURE);                                                        \
  }

#define die(...)                                                               \
  {                                                                            \
    fprintf(stderr, __VA_ARGS__);                                              \
    exit(EXIT_FAILURE);                                                        \
  }

#ifndef NDEBUG
#define debugf printf
#else
inline int noop(const char *s, ...) {
  (void)s;
  return 0;
}
#define debugf noop
#endif

#endif
