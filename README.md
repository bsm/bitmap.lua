# Bitmap.lua

Lua bitmaps (aka bitstrings or bitsets) implemented in C

## API Examples

Load the library:

    local bitmap = require 'bitmap'

Create a new bitmap with 1000 bits:

    local map = bitmap.new(1000)

Read the size:

    map:size() -- 1000

Fill with zeros:

    map:zero()

Fill with ones:

    map:fill()

Set bits:

    map:set(100, 1) -- set 1 bit at offset 100
    map:set(50, 5)  -- set 5 bits at offset 50

Read bits:

    map:get(52) -- true
    map:get(62) -- false

Read the weight (number of set bits):

    map:weight() -- 6

Clear bits:

    map:clear(0, 100) -- clear the first 100 bits

Bit operations:

    map:band(other)    -- bit-AND
    map:bor(other)     -- bit-OR
    map:bxor(other)    -- bit-XOR
    map:bandnot(other) -- bit-AND+NOT
    map:bnot()         -- bit-NOT

Boolean tests:

    map:empty()
    map:full()
    map:equal(other)
    map:intersects(other)
    map:subset(other)

## Testing

Run tests via:

    $ make test

To run benchmarks, try:

    $ make benchmark

## Licence

    Copyright (c) 2013 Black Square Media Ltd

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
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
