CC = cc
W = -W -Wall
OPT = -O2 -g -pedantic -std=c99 -fPIC
CFLAGS	= $(OPT) $(W) $(XCFLAGS)
LDFLAGS = -O -shared $(XLDFLAGS)
LUA_H = $(shell locate 'lua.h' | head -1)
LUAINC = -I$(realpath $(dir $(LUA_H)))

ifeq ($(OS),Darwin)
  CFLAGS = $(CFLAGS) -fno-common
  LDFLAGS = -bundle -undefined dynamic_lookup
endif

default: all

all: bitmap.so

clean:
	rm -rf *.o *.so

test: all
	lua bitmap_test.lua

benchmark: all
	lua bitmap_benchmark.lua

bitmap.so: lua_bitmap.o bitmap.o
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@

lua_bitmap.o: lua_bitmap.c
	$(CC) $(CFLAGS) $(LUAINC) -c $<

bitmap.o: bitmap.c
	$(CC) $(CFLAGS) $(LUAINC) -c $<
