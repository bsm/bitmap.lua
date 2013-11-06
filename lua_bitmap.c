/* The MIT License

   Copyright (C) 2013 Black Square Media

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

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

#include <lua.h>
#include <lauxlib.h>
#include <malloc.h>
#include <stdio.h>
#include "bitmap.h"

#define BMAP_LUA_MT "bitmap_mt"

typedef struct {
  unsigned long *map;
  size_t bits;
} bitmapL_t;

static bitmapL_t* luaL_checkbitmapL_t(lua_State *L, int n) {
  return *(bitmapL_t**)luaL_checkudata(L, n, BMAP_LUA_MT);
}

static void lua_pushbitmap_t(lua_State *L, bitmapL_t *wrap) {
  bitmapL_t **ptr = (bitmapL_t **) lua_newuserdata(L, sizeof(bitmapL_t *));
  *ptr = wrap;
  luaL_getmetatable(L, BMAP_LUA_MT);
  lua_setmetatable(L, -2);
}

static bitmapL_t* bitmapL_t_create(size_t bits) {
  bitmapL_t *wrap = malloc(sizeof(bitmapL_t));
  wrap->map = calloc(BITS_TO_LONGS(bits), sizeof(unsigned long));
  wrap->bits = bits;
  return wrap;
}

int bitmapL_new(lua_State *L) {
  size_t bits = (size_t) luaL_checkint(L, 1);
  if (bits < 1) {
    lua_pushstring(L, "bad argument #2 (must be greater 0)");
    lua_error(L);
  }

  bitmapL_t *wrap = bitmapL_t_create(bits);
  lua_pushbitmap_t(L, wrap);
  return 1;
}

int bitmapL_free(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  free(wrap->map);
  free(wrap);
  return 0;
}

int bitmapL_zero(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  bitmap_zero(wrap->map, wrap->bits);
  return 0;
}

int bitmapL_fill(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  bitmap_fill(wrap->map, wrap->bits);
  return 0;
}

int bitmapL_size(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  lua_pushinteger(L, wrap->bits);
  return 1;
}

int bitmapL_get(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  int offset = luaL_checkint(L, 2);

  if (offset < 0 || offset > (long) wrap->bits) {
    lua_pushstring(L, "bad argument #2 (out of bounds)");
    lua_error(L);
  }

  lua_pushboolean(L, test_bit(offset, wrap->map));
  return 1;
}


int bitmapL_set(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  int offset = luaL_checkint(L, 2);
  int bits =  luaL_checkint(L, 3);

  if (offset < 0 || bits < 1 || offset + bits > (long) wrap->bits) {
    lua_pushstring(L, "bad argument #2 (out of bounds)");
    lua_error(L);
  }

  bitmap_set(wrap->map, offset, bits);
  lua_pop(L, 2);
  return 1;
}

int bitmapL_clear(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  int offset = luaL_checkint(L, 2);
  int bits =  luaL_checkint(L, 3);

  if (offset < 0 || bits < 1 || offset + bits > (long) wrap->bits) {
    lua_pushstring(L, "bad argument #2 (out of bounds)");
    lua_error(L);
  }

  bitmap_clear(wrap->map, offset, bits);
  lua_pop(L, 2);
  return 1;
}

int bitmapL_band(lua_State *L) {
  bitmapL_t *w1 = luaL_checkbitmapL_t(L, 1);
  bitmapL_t *w2 = luaL_checkbitmapL_t(L, 2);

  size_t bits = MIN(w1->bits, w2->bits);
  bitmapL_t *res = bitmapL_t_create(bits);
  bitmap_and(res->map, w1->map, w2->map, bits);

  lua_pushbitmap_t(L, res);
  return 1;
}

int bitmapL_bor(lua_State *L) {
  bitmapL_t *w1 = luaL_checkbitmapL_t(L, 1);
  bitmapL_t *w2 = luaL_checkbitmapL_t(L, 2);

  size_t bits = MAX(w1->bits, w2->bits);
  bitmapL_t *res = bitmapL_t_create(bits);
  bitmap_or(res->map, w1->map, w2->map, bits);

  lua_pushbitmap_t(L, res);
  return 1;
}

int bitmapL_bxor(lua_State *L) {
  bitmapL_t *w1 = luaL_checkbitmapL_t(L, 1);
  bitmapL_t *w2 = luaL_checkbitmapL_t(L, 2);

  size_t bits = MAX(w1->bits, w2->bits);
  bitmapL_t *res = bitmapL_t_create(bits);
  bitmap_xor(res->map, w1->map, w2->map, bits);

  lua_pushbitmap_t(L, res);
  return 1;
}

int bitmapL_bandnot(lua_State *L) {
  bitmapL_t *w1 = luaL_checkbitmapL_t(L, 1);
  bitmapL_t *w2 = luaL_checkbitmapL_t(L, 2);

  bitmapL_t *res = bitmapL_t_create(w1->bits);
  bitmap_andnot(res->map, w1->map, w2->map, w1->bits);

  lua_pushbitmap_t(L, res);
  return 1;
}

int bitmapL_bnot(lua_State *L) {
  bitmapL_t *src = luaL_checkbitmapL_t(L, 1);
  bitmapL_t *res = bitmapL_t_create(src->bits);
  bitmap_complement(res->map, src->map, res->bits);

  lua_pushbitmap_t(L, res);
  return 1;
}

int bitmapL_equal(lua_State *L) {
  bitmapL_t *w1 = luaL_checkbitmapL_t(L, 1);
  bitmapL_t *w2 = luaL_checkbitmapL_t(L, 2);
  int truth = (w1->bits == w2->bits && bitmap_equal(w1->map, w2->map, w1->bits));

  lua_pushboolean(L, truth);
  return 1;
}

int bitmapL_intersects(lua_State *L) {
  bitmapL_t *w1 = luaL_checkbitmapL_t(L, 1);
  bitmapL_t *w2 = luaL_checkbitmapL_t(L, 2);
  int truth = bitmap_intersects(w1->map, w2->map, MIN(w1->bits, w2->bits));

  lua_pushboolean(L, truth);
  return 1;
}

int bitmapL_subset(lua_State *L) {
  bitmapL_t *w1 = luaL_checkbitmapL_t(L, 1);
  bitmapL_t *w2 = luaL_checkbitmapL_t(L, 2);
  int truth = bitmap_subset(w1->map, w2->map, MIN(w1->bits, w2->bits));

  lua_pushboolean(L, truth);
  return 1;
}

int bitmapL_empty(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  lua_pushboolean(L, bitmap_empty(wrap->map, wrap->bits));
  return 1;
}

int bitmapL_full(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  lua_pushboolean(L, bitmap_full(wrap->map, wrap->bits));
  return 1;
}

int bitmapL_weight(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  lua_pushinteger(L, bitmap_weight(wrap->map, wrap->bits));
  return 1;
}

int bitmapL_csv(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  char buf[1024];
  int n = bitmap_snlistprintf(buf, 100, wrap->map, wrap->bits);
  lua_pushlstring(L, buf, n);
  return 1;
}

int bitmapL_offsets(lua_State *L) {
  bitmapL_t *wrap = luaL_checkbitmapL_t(L, 1);
  int bits = (int) wrap->bits;
  int cur, i = 1;

  lua_newtable(L);

  cur = find_first_bit(wrap->map, bits);
  while (cur < bits) {
    lua_pushinteger(L, cur);
    lua_rawseti(L, -2, i++);
    cur = find_next_bit(wrap->map, bits, cur+1);
  }

  return 1;
}

static const struct luaL_reg bitmapL_mt[] = {
    {"__gc",   bitmapL_free},
    {"zero",   bitmapL_zero},
    {"fill",   bitmapL_fill},
    {"csv",    bitmapL_csv},
    {"size",   bitmapL_size},
    {"get",    bitmapL_get},
    {"set",    bitmapL_set},
    {"clear",  bitmapL_clear},
    {"band",   bitmapL_band},
    {"bor",    bitmapL_bor},
    {"bxor",   bitmapL_bxor},
    {"bandnot",bitmapL_bandnot},
    {"bnot",   bitmapL_bnot},
    {"equal",  bitmapL_equal},
    {"intersects", bitmapL_intersects},
    {"subset", bitmapL_subset},
    {"empty",  bitmapL_empty},
    {"full",   bitmapL_full},
    {"weight", bitmapL_weight},
    {"offsets", bitmapL_offsets},
    {NULL, NULL}
};

static const struct luaL_reg thislib[] = {
    {"new", bitmapL_new},
    {NULL, NULL}
};

LUALIB_API int luaopen_bitmap (lua_State *L) {
  luaL_newmetatable(L, BMAP_LUA_MT);
  lua_pushvalue(L, -1);
  lua_setfield(L, -2, "__index");
  luaL_register(L, NULL, bitmapL_mt);

  luaL_register(L, "bitmap", thislib);
  return 1;
}