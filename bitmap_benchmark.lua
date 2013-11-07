local BMSPF = "[B] %-12s%-12s%9d ms%10d ns/ops"

function benchmark(context, cycles, fun)
  local start = os.clock()
  local cycles = cycles
  for c=0,cycles-1 do fun(c) end
  local ms = (os.clock()-start) * 1e3
  print(BMSPF:format(context, " (" .. cycles .. "x)", ms, ms/cycles*1e6))
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

benchmark('offsets', 1000, function(i)
  a:offsets()
end)

benchmark('weight', 50000, function(i)
  a:weight()
end)

benchmark('bor', 50000, function(i)
  a:bor(b)
end)

benchmark('band', 50000, function(i)
  a:band(b)
end)

benchmark('bandnot', 50000, function(i)
  a:bandnot(b)
end)
