#include "bsutils.h"
#include <stdbool.h>

size_t find_first_nonzero(unsigned char const *ptr, size_t len) {
  size_t big_steps = len / sizeof(unsigned long long);
  size_t small_steps = len % sizeof(unsigned long long);
  size_t result = 0;
  for (size_t i = 0; i < big_steps; i++) {
    unsigned long long const *big_ptr = (unsigned long long const *)ptr;
    unsigned long long block = big_ptr[i];
    if (block == 0) {
      result += sizeof(unsigned long long);
    } else {
      // Look deeper inside.
      size_t current_byte_position = i * sizeof(unsigned long long);
      for (size_t j = 0; j < sizeof(unsigned long long); j++) {
        unsigned char byte = ptr[current_byte_position + j];
        if (byte != 0) {
          return result;
        } else {
          result++;
        }
      }
    }
  }
  // Big strides were all zeroes, check whatever remains at the end slowly.
  size_t small_step_start = big_steps * sizeof(unsigned long long);
  for (size_t i = 0; i < small_steps; i++) {
    unsigned char byte = ptr[small_step_start + i];
    if (byte != 0) {
      return result;
    } else {
      result++;
    }
  }
  // If we get this far, there's nothing to find.
  return len;
}

size_t find_last_nonzero(unsigned char const *ptr, size_t len) {
  size_t big_steps = len / sizeof(unsigned long long);
  size_t small_steps = len % sizeof(unsigned long long);
  // Based on https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord,
  // using the hasless method, expanded to 8 bytes instead of 4. This requires
  // two masks.
  unsigned long long low_bit_mask = 0x0101010101010101ULL;
  unsigned long long high_bit_mask = 0x8080808080808080ULL;
  size_t result = 0;
  for (size_t i = 0; i < big_steps; i++) {
    unsigned long long const *big_ptr = (unsigned long long const *)ptr;
    unsigned long long block = big_ptr[i];
    // Check if we are anywhere zero.
    if ((block - low_bit_mask) & ~block & high_bit_mask) {
      // Look deeper inside.
      size_t current_byte_position = i * sizeof(unsigned long long);
      size_t last_known_nonzero = 0;
      for (size_t j = 0; j < sizeof(unsigned long long); j++) {
        unsigned char byte = ptr[current_byte_position + j];
        if (byte != 0) {
          last_known_nonzero = i;
        }
      }
      return current_byte_position + last_known_nonzero;
    } else {
      result += sizeof(unsigned long long);
    }
  }
  // Big strides were all nonzeroes, check whatever remains at the end slowly.
  size_t small_step_start = big_steps * sizeof(unsigned long long);
  size_t last_known_nonzero = 0;
  bool found_anything_at_all = false;
  for (size_t i = 0; i < small_steps; i++) {
    unsigned char byte = ptr[small_step_start + i];
    if (byte != 0) {
      found_anything_at_all = true;
      last_known_nonzero = i;
    }
  }
  // If we found any nonzeroes, that gives us the real position.
  if (found_anything_at_all) {
    return small_step_start + last_known_nonzero;
  } else {
    // If we get this far, there's nothing to find.
    return len;
  }
}
