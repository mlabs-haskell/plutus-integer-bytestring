#include "bsutils.h"

void memcpy_r(unsigned char *dst, unsigned char const *src, size_t len,
              size_t end) {
  for (size_t i = 0; i < len; i++) {
    dst[end - 1 - i] = src[i];
  }
}
