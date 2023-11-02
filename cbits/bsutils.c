#include "bsutils.h"
#include <stdint.h>

void *memcpy_r(void *restrict dest, void const *restrict src, size_t n,
               size_t src_len) {
  // We use the 'strip mining' technique to speed up the reverse copy
  // (http://physics.ujep.cz/~zmoravec/prga/main_for/mergedProjects/optaps_for/common/optaps_vec_mine.htm).
  // Essentially, we first copy (and reverse) 64-bit blocks, then after we've
  // done as many as we can, finish up whatever remains byte-by-byte. This is
  // much more efficient than going byte-wise through the entire process, as it
  // reduces memory movement by a factor of 8.
  //
  // We choose a 64-bit block size for our large copy-and-reverse step because
  // both GCC and Clang (the only compilers we are concerned with given our
  // focus on Tier 1 architectures) provide the __builtin_bswap64 intrinsic,
  // which emits an efficient endianness swap regardless of machine
  // architecture.
  size_t stride = sizeof(uint64_t);
  size_t big_steps = n / stride;
  size_t small_steps = n % stride;
  // Big copy-and-swap steps.
  for (size_t i = 0; i < big_steps; i++) {
    uint64_t const *read_ptr = (uint64_t const *)(src + src_len - i * stride);
    uint64_t *write_ptr = (uint64_t *)(dest + i * stride);
    uint64_t const big_read = *read_ptr;
    *write_ptr = __builtin_bswap64(big_read);
  }
  size_t small_step_start = big_steps * stride;
  // Small copy-and-swap steps.
  for (size_t i = 0; i < small_steps; i++) {
    uint8_t const *read_ptr =
        (uint8_t const *)(src + src_len - small_step_start - i);
    uint8_t *write_ptr = (uint8_t *)(dest + small_step_start + i);
    uint8_t const small_read = *read_ptr;
    *write_ptr = small_read;
  }
  // We return the destination for uniformity of interface with memcpy.
  return dest;
}
