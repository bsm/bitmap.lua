CC = cc
W = -W -Wall
OPT = -O2 -g -pedantic -std=c99 -fPIC
CFLAGS = $(OPT) $(W) $(XCFLAGS)
LDFLAGS = -O -shared $(XLDFLAGS)
LUADIR = /usr/include/lua5.1/

ifeq ($(OS),Darwin)
  CFLAGS= $(CFLAGS) -fno-common
  LDFLAGS= -bundle -undefined dynamic_lookup
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

lua_bitmap.o: lua_bitmap.c bitmap.h
	$(CC) $(CFLAGS) -I$(LUADIR) -c $<

bitmap.o: bitmap.c bitmap.h
	$(CC) $(CFLAGS) -I$(LUADIR) -c $<
