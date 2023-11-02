#ifndef BSUTILS_H
#define BSUTILS_H
#include <stddef.h>

void *memcpy_r(void *restrict dest, void const *restrict src, size_t n,
               size_t src_len);

#endif // BSUTILS_H
