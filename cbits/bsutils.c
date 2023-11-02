#include "bsutils.h"

void reverse(unsigned char *src, size_t len) {
  size_t limit = len / 2;
  for (size_t i = 0; i < limit; i++) {
    unsigned char leading = src[i];
    unsigned char trailing = src[len - i - 1];
    src[i] = trailing;
    src[len - i - 1] = leading;
  }
}
