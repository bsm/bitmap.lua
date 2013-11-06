local BMSPF = "[B] %-12s%-15s%dms"

function benchmark(context, cycles, fun)
  local start = os.clock()
  local cycles = cycles or 1e6
  for c=0,cycles-1 do fun(c) end
  print(BMSPF:format(context, " (" .. cycles .. "x)", (os.clock()-start) * 1000))
end

local bitmap = require 'bitmap'
local a = bitmap.new(1e6)
local b = bitmap.new(1e6)

benchmark('set', 1e6, function(i)
  a:set(i, 1)
end)

benchmark('get', 1e6, function(i)
  a:get(i, 1)
end)

benchmark('clear', 1e6, function(i)
  a:clear(i, 1)
end)

a:set(0, 70000)
b:set(30000, 70000)

benchmark('offsets', 50, function(i)
  a:offsets()
end)

benchmark('weight', 600, function(i)
  a:weight()
end)

benchmark('bor', 300, function(i)
  a:bor(b)
end)

benchmark('band', 2000, function(i)
  a:band(b)
end)

benchmark('bandnot', 200, function(i)
  a:bandnot(b)
end)
