/* The MIT License

   Copyright (C) 2011, 2012 Zilong Tan (eric.zltan@gmail.com)

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/

/* This implementation is based on the one from Linux kernel. */

#ifndef _ULIB_BITMAP_H
#define _ULIB_BITMAP_H
#include <string.h>
#include <stdint.h>

/*
 * A bitmap can be seen as an unsigned long array thus operations that
 * apply to a single unsigned long or unsigned long array can also be
 * used here. Some useful operations can be found in math_bit.h.
 *
 * Note that nbits should be always a compile time evaluable constant.
 * Otherwise many inlines will generate horrible code.
 *
 * bitmap_zero(dst, nbits)      *dst = 0UL
 * bitmap_fill(dst, nbits)      *dst = ~0UL
 * bitmap_copy(dst, src, nbits)     *dst = *src
 * bitmap_and(dst, src1, src2, nbits)   *dst = *src1 & *src2
 * bitmap_or(dst, src1, src2, nbits)    *dst = *src1 | *src2
 * bitmap_xor(dst, src1, src2, nbits)   *dst = *src1 ^ *src2
 * bitmap_andnot(dst, src1, src2, nbits)  *dst = *src1 & ~(*src2)
 * bitmap_complement(dst, src, nbits)   *dst = ~(*src)
 * bitmap_equal(src1, src2, nbits)    Are *src1 and *src2 equal?
 * bitmap_intersects(src1, src2, nbits)   Do *src1 and *src2 overlap?
 * bitmap_subset(src1, src2, nbits)   Is *src1 a subset of *src2?
 * bitmap_empty(src, nbits)     Are all bits zero in *src?
 * bitmap_full(src, nbits)      Are all bits set in *src?
 * bitmap_weight(src, nbits)      Hamming Weight: number set bits
 * bitmap_set(dst, pos, nbits)      Set specified bit area
 * bitmap_clear(dst, pos, nbits)    Clear specified bit area
 * bitmap_find_next_zero_area(buf, len, pos, n, mask) Find bit free area
 * bitmap_shift_right(dst, src, n, nbits) *dst = *src >> n
 * bitmap_shift_left(dst, src, n, nbits)  *dst = *src << n
 * bitmap_remap(dst, src, old, new, nbits)  *dst = map(old, new)(src)
 * bitmap_bitremap(oldbit, old, new, nbits) newbit = map(old, new)(oldbit)
 * bitmap_onto(dst, orig, relmap, nbits)  *dst = orig relative to relmap
 * bitmap_fold(dst, orig, sz, nbits)    dst bits = orig bits mod sz
 * bitmap_snprintf(buf, len, src, nbits)  Print bitmap src to buf
 * bitmap_parse(buf, buflen, dst, nbits)  Parse bitmap dst from kernel buf
 * bitmap_snlistprintf(buf, len, src, nbits) Print bitmap src as list to buf
 * bitmap_parselist(buf, dst, nbits)    Parse bitmap dst from list
 * bitmap_find_free_region(bitmap, bits, order) Find and allocate bit region
 * bitmap_release_region(bitmap, pos, order)  Free specified bit region
 * bitmap_allocate_region(bitmap, pos, order) Allocate specified bit region
 */

int  __bitmap_empty(const unsigned long *bitmap, int bits);
int  __bitmap_full(const unsigned long *bitmap, int bits);
int  __bitmap_equal(const unsigned long *bitmap1,
        const unsigned long *bitmap2, int bits);
void __bitmap_complement(unsigned long *dst, const unsigned long *src, int bits);
void __bitmap_shift_right(unsigned long *dst,
        const unsigned long *src, int shift, int bits);
void __bitmap_shift_left(unsigned long *dst,
       const unsigned long *src, int shift, int bits);
int  __bitmap_and(unsigned long *dst, const unsigned long *bitmap1,
      const unsigned long *bitmap2, int bits);
void __bitmap_or(unsigned long *dst, const unsigned long *bitmap1,
     const unsigned long *bitmap2, int bits);
void __bitmap_xor(unsigned long *dst, const unsigned long *bitmap1,
      const unsigned long *bitmap2, int bits);
int  __bitmap_andnot(unsigned long *dst, const unsigned long *bitmap1,
         const unsigned long *bitmap2, int bits);
int  __bitmap_intersects(const unsigned long *bitmap1,
       const unsigned long *bitmap2, int bits);
int  __bitmap_subset(const unsigned long *bitmap1,
         const unsigned long *bitmap2, int bits);
int  __bitmap_weight(const unsigned long *bitmap, int bits);

void bitmap_set(unsigned long *map, int i, int len);
void bitmap_clear(unsigned long *map, int start, int nr);
unsigned long bitmap_find_next_zero_area(unsigned long *map,
           unsigned long size,
           unsigned long start,
           unsigned int nr,
           unsigned long align_mask);

int  bitmap_snprintf(char *buf, unsigned int len,
         const unsigned long *src, int nbits);
int  __bitmap_parse(const char *buf, unsigned int buflen,
        unsigned long *dst, int nbits);
int  bitmap_parse_user(const char *ubuf, unsigned int ulen,
           unsigned long *dst, int nbits);
int  bitmap_snlistprintf(char *buf, unsigned int len,
        const unsigned long *src, int nbits);
int  bitmap_parselist(const char *buf, unsigned long *maskp,
          unsigned int nmaskbits);
void bitmap_remap(unsigned long *dst, const unsigned long *src,
      const unsigned long *old, const unsigned long *new, int bits);
int  bitmap_bitremap(int oldbit,
         const unsigned long *old, const unsigned long *new, int bits);
void bitmap_onto(unsigned long *dst, const unsigned long *orig,
     const unsigned long *relmap, int bits);
void bitmap_fold(unsigned long *dst, const unsigned long *orig,
     int sz, int bits);
int  bitmap_find_free_region(unsigned long *bitmap, int bits, int order);
void bitmap_release_region(unsigned long *bitmap, int pos, int order);
int  bitmap_allocate_region(unsigned long *bitmap, int pos, int order);
void bitmap_copy_le(void *dst, const unsigned long *src, int nbits);

#define BITS_PER_BYTE     8
#define BITS_PER_LONG     __WORDSIZE

#define DIV_ROUND_UP(n,d)   (((n) + (d) - 1) / (d))
#define BITS_TO_LONGS(nr)   DIV_ROUND_UP(nr, BITS_PER_BYTE * sizeof(long))
#define BIT_WORD(nr)      ((nr) / BITS_PER_LONG)
#define BIT_MASK(nr)      (1UL << ((nr) % BITS_PER_LONG))
#define ALIGN_MASK(x, mask) (((x) + (mask)) & ~(mask))
#define ALIGN(x, a)     ALIGN_MASK(x, (__typeof__(x))(a) - 1)
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#define for_each_set_bit(bit, addr, size)     \
  for ((bit) = find_first_bit((addr), (size));    \
       (bit) < (size);          \
       (bit) = find_next_bit((addr), (size), (bit) + 1))

#define DEFINE_BITMAP(name,bits)    \
  unsigned long name[BITS_TO_LONGS(bits)]

#define BITMAP_LAST_WORD_MASK(nbits)        \
  (             \
    ((nbits) % BITS_PER_LONG) ?     \
    (1UL<<((nbits) % BITS_PER_LONG))-1 : ~0UL \
    )

#define small_const_nbits(nbits)          \
  (__builtin_constant_p(nbits) && (nbits) <= BITS_PER_LONG)


static inline void set_bit(int nr, volatile unsigned long *addr)
{
  *(addr + BIT_WORD(nr)) |= BIT_MASK(nr);
}

static inline void clear_bit(int nr, volatile unsigned long *addr)
{
  *(addr + BIT_WORD(nr)) &= ~BIT_MASK(nr);
}

static inline void change_bit(int nr, volatile unsigned long *addr)
{
  *(addr + BIT_WORD(nr)) ^= BIT_MASK(nr);
}

static inline int test_bit(int nr, const volatile unsigned long *addr)
{
  return 1UL & (addr[BIT_WORD(nr)] >> (nr & (BITS_PER_LONG-1)));
}

static inline unsigned int hweight32(uint32_t w)
{
#ifdef BIT_HAS_FAST_MULT
  w -= (w >> 1) & 0x55555555;
  w =  (w & 0x33333333) + ((w >> 2) & 0x33333333);
  w =  (w + (w >> 4)) & 0x0f0f0f0f;
  return (w * 0x01010101) >> 24;
#else
  uint32_t res = w - ((w >> 1) & 0x55555555);
  res = (res & 0x33333333) + ((res >> 2) & 0x33333333);
  res = (res + (res >> 4)) & 0x0F0F0F0F;
  res = res + (res >> 8);
  return (res + (res >> 16)) & 0x000000FF;
#endif
}

static inline unsigned int hweight64(uint64_t w)
{
#if BITS_PER_LONG == 32
  return hweight32((unsigned int)(w >> 32)) +
    hweight32((unsigned int)w);
#elif BITS_PER_LONG == 64
#ifdef BIT_HAS_FAST_MULT
  w -= (w >> 1) & 0x5555555555555555ul;
  w =  (w & 0x3333333333333333ul) + ((w >> 2) & 0x3333333333333333ul);
  w =  (w + (w >> 4)) & 0x0f0f0f0f0f0f0f0ful;
  return (w * 0x0101010101010101ul) >> 56;
#else
  uint64_t res = w - ((w >> 1) & 0x5555555555555555ul);
  res = (res & 0x3333333333333333ul) + ((res >> 2) & 0x3333333333333333ul);
  res = (res + (res >> 4)) & 0x0F0F0F0F0F0F0F0Ful;
  res = res + (res >> 8);
  res = res + (res >> 16);
  return (res + (res >> 32)) & 0x00000000000000FFul;
#endif
#endif
}

static inline unsigned int hweight_long(unsigned long a)
{
  return sizeof(a) == 4? hweight32(a): hweight64(a);
}

/* use gcc builtin instead if possible */
#define __ffs(w) (__builtin_ffsl(w) - 1)

/* ffz - find the first zero in word.
 * Undefined if no zero exists, so code should check against ~0UL first. */
#define ffz(x)  __ffs(~(x))

/* find the next set bit in a memory region */
static inline unsigned long
find_next_bit(const unsigned long *addr, unsigned long size, unsigned long offset)
{
  const unsigned long *p = addr + BIT_WORD(offset);
  unsigned long result = offset & ~(BITS_PER_LONG-1);
  unsigned long tmp;

  if (offset >= size)
    return size;
  size -= result;
  offset %= BITS_PER_LONG;
  if (offset) {
    tmp = *(p++);
    tmp &= (~0UL << offset);
    if (size < BITS_PER_LONG)
      goto found_first;
    if (tmp)
      goto found_middle;
    size -= BITS_PER_LONG;
    result += BITS_PER_LONG;
  }
  while (size & ~(BITS_PER_LONG-1)) {
    if ((tmp = *(p++)))
      goto found_middle;
    result += BITS_PER_LONG;
    size -= BITS_PER_LONG;
  }
  if (!size)
    return result;
  tmp = *p;

found_first:
  tmp &= (~0UL >> (BITS_PER_LONG - size));
  if (tmp == 0UL)   /* Are any bits set? */
    return result + size; /* Nope. */
found_middle:
  return result + __ffs(tmp);
}

static inline unsigned long
find_next_zero_bit(const unsigned long *addr, unsigned long size, unsigned long offset)
{
  const unsigned long *p = addr + BIT_WORD(offset);
  unsigned long result = offset & ~(BITS_PER_LONG-1);
  unsigned long tmp;

  if (offset >= size)
    return size;
  size -= result;
  offset %= BITS_PER_LONG;
  if (offset) {
    tmp = *(p++);
    tmp |= ~0UL >> (BITS_PER_LONG - offset);
    if (size < BITS_PER_LONG)
      goto found_first;
    if (~tmp)
      goto found_middle;
    size -= BITS_PER_LONG;
    result += BITS_PER_LONG;
  }
  while (size & ~(BITS_PER_LONG-1)) {
    if (~(tmp = *(p++)))
      goto found_middle;
    result += BITS_PER_LONG;
    size -= BITS_PER_LONG;
  }
  if (!size)
    return result;
  tmp = *p;

found_first:
  tmp |= ~0UL << size;
  if (tmp == ~0UL)  /* Are any bits zero? */
    return result + size; /* Nope. */
found_middle:
  return result + ffz(tmp);
}

static inline unsigned long
find_first_bit(const unsigned long *addr, unsigned long size)
{
  const unsigned long *p = addr;
  unsigned long result = 0;
  unsigned long tmp;

  while (size & ~(BITS_PER_LONG-1)) {
    if ((tmp = *(p++)))
      goto found;
    result += BITS_PER_LONG;
    size -= BITS_PER_LONG;
  }
  if (!size)
    return result;

  tmp = (*p) & (~0UL >> (BITS_PER_LONG - size));
  if (tmp == 0UL)   /* Are any bits set? */
    return result + size; /* Nope. */
found:
  return result + __ffs(tmp);
}

static inline unsigned long
find_first_zero_bit(const unsigned long *addr, unsigned long size)
{
  const unsigned long *p = addr;
  unsigned long result = 0;
  unsigned long tmp;

  while (size & ~(BITS_PER_LONG-1)) {
    if (~(tmp = *(p++)))
      goto found;
    result += BITS_PER_LONG;
    size -= BITS_PER_LONG;
  }
  if (!size)
    return result;

  tmp = (*p) | (~0UL << size);
  if (tmp == ~0UL)  /* Are any bits zero? */
    return result + size; /* Nope. */
found:
  return result + ffz(tmp);
}

static inline void bitmap_zero(unsigned long *dst, int nbits)
{
  if (small_const_nbits(nbits))
    *dst = 0UL;
  else {
    int len = BITS_TO_LONGS(nbits) * sizeof(unsigned long);
    memset(dst, 0, len);
  }
}

static inline void bitmap_fill(unsigned long *dst, int nbits)
{
  size_t nlongs = BITS_TO_LONGS(nbits);
  if (!small_const_nbits(nbits)) {
    int len = (nlongs - 1) * sizeof(unsigned long);
    memset(dst, 0xff,  len);
  }
  dst[nlongs - 1] = BITMAP_LAST_WORD_MASK(nbits);
}

static inline void bitmap_copy(unsigned long *dst, const unsigned long *src,
             int nbits)
{
  if (small_const_nbits(nbits))
    *dst = *src;
  else {
    int len = BITS_TO_LONGS(nbits) * sizeof(unsigned long);
    memcpy(dst, src, len);
  }
}

static inline int bitmap_and(unsigned long *dst, const unsigned long *src1,
           const unsigned long *src2, int nbits)
{
  if (small_const_nbits(nbits))
    return (*dst = *src1 & *src2) != 0;
  return __bitmap_and(dst, src1, src2, nbits);
}

static inline void bitmap_or(unsigned long *dst, const unsigned long *src1,
           const unsigned long *src2, int nbits)
{
  if (small_const_nbits(nbits))
    *dst = *src1 | *src2;
  else
    __bitmap_or(dst, src1, src2, nbits);
}

static inline void bitmap_xor(unsigned long *dst, const unsigned long *src1,
            const unsigned long *src2, int nbits)
{
  if (small_const_nbits(nbits))
    *dst = *src1 ^ *src2;
  else
    __bitmap_xor(dst, src1, src2, nbits);
}

static inline int bitmap_andnot(unsigned long *dst, const unsigned long *src1,
        const unsigned long *src2, int nbits)
{
  if (small_const_nbits(nbits))
    return (*dst = *src1 & ~(*src2)) != 0;
  return __bitmap_andnot(dst, src1, src2, nbits);
}

static inline void bitmap_complement(unsigned long *dst, const unsigned long *src,
             int nbits)
{
  if (small_const_nbits(nbits))
    *dst = ~(*src) & BITMAP_LAST_WORD_MASK(nbits);
  else
    __bitmap_complement(dst, src, nbits);
}

static inline int bitmap_equal(const unsigned long *src1,
             const unsigned long *src2, int nbits)
{
  if (small_const_nbits(nbits))
    return ! ((*src1 ^ *src2) & BITMAP_LAST_WORD_MASK(nbits));
  else
    return __bitmap_equal(src1, src2, nbits);
}

static inline int bitmap_intersects(const unsigned long *src1,
            const unsigned long *src2, int nbits)
{
  if (small_const_nbits(nbits))
    return ((*src1 & *src2) & BITMAP_LAST_WORD_MASK(nbits)) != 0;
  else
    return __bitmap_intersects(src1, src2, nbits);
}

static inline int bitmap_subset(const unsigned long *src1,
        const unsigned long *src2, int nbits)
{
  if (small_const_nbits(nbits))
    return ! ((*src1 & ~(*src2)) & BITMAP_LAST_WORD_MASK(nbits));
  else
    return __bitmap_subset(src1, src2, nbits);
}

static inline int bitmap_empty(const unsigned long *src, int nbits)
{
  if (small_const_nbits(nbits))
    return ! (*src & BITMAP_LAST_WORD_MASK(nbits));
  else
    return __bitmap_empty(src, nbits);
}

static inline int bitmap_full(const unsigned long *src, int nbits)
{
  if (small_const_nbits(nbits))
    return ! (~(*src) & BITMAP_LAST_WORD_MASK(nbits));
  else
    return __bitmap_full(src, nbits);
}

static inline int bitmap_weight(const unsigned long *src, int nbits)
{
  if (small_const_nbits(nbits))
    return hweight_long(*src & BITMAP_LAST_WORD_MASK(nbits));
  return __bitmap_weight(src, nbits);
}

static inline void bitmap_shift_right(unsigned long *dst,
              const unsigned long *src, int n, int nbits)
{
  if (small_const_nbits(nbits))
    *dst = *src >> n;
  else
    __bitmap_shift_right(dst, src, n, nbits);
}

static inline void bitmap_shift_left(unsigned long *dst,
             const unsigned long *src, int n, int nbits)
{
  if (small_const_nbits(nbits))
    *dst = (*src << n) & BITMAP_LAST_WORD_MASK(nbits);
  else
    __bitmap_shift_left(dst, src, n, nbits);
}

static inline int bitmap_parse(const char *buf, unsigned int buflen,
             unsigned long *maskp, int nmaskbits)
{
  return __bitmap_parse(buf, buflen, maskp, nmaskbits);
}

#endif /* _ULIB_BITMAP_H */
