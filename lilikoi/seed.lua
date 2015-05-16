
-- lilikoi/seed: the core lib used by lilikoi code after translation.
-- Written by Tommy Ettinger. Public Domain.

local seed = {}

-- runs a partial function once it has been supplied all needed args,
-- returning its result, if any, or returns a new partial function
-- if not supplied enough args. handles grouping as well.
function seed.__step(f, a)
	if a == nil then
		if #(f["\6args"]) == f["\6arity"] then
			return f["\6op"](unpack(f["\6args"]))
		else
			return f
		end
	end
	local f2 = table.deepcopy(f)
	if f2["\6group"] == a then
		return f2["\6op"](unpack(f2["\6args"]))
	elseif f2["\6group"] ~= nil or
			#(f2["\6args"]) < f2["\6arity"] then
		table.insert(f2["\6args"], a)
	end
	if #(f2["\6args"]) == f2["\6arity"] then
		return f2["\6op"](unpack(f2["\6args"]))
	end
	return f2
end


local function helper(a, i)
  if i < a.n then return i+1,a[i+1] end
end
local function apairs(...)
  return helper, {n=select('#', ...), ...}, 0
end

-- takes a sequence of generated function tables and data, and
-- steps through it until it has exhausted the sequence,
-- returning the final stack.
function seed.__eval(...)
	local stack = {}
	for i,a in apairs(...) do
		if type(a) == 'table' and a["\6op"] ~= nil then
			stack[#stack] = seed.__step(a, stack[#stack])
		else
			stack[#stack] = a
		end
	end
end
local function coredef(op, arity, show, ender)
	return {["\6op"] = op,
			["\6arity"] = arity,
			["\6name"] = show,
			["\6group"] = ender,
			["\6args"] = {}}
end
--[[
	+     -     *     /     %     ^     #
	==    ~=    <=    >=    <     >     =
	(     )     {     }     [     ]
	;no   :     ,no   .     ..    ...no
--]]
local function add(a, b)
	return a + b
end
seed.__add = coredef(add, 2, "+")
local function sub(a, b)
	return a - b
end
seed.__sub = coredef(sub, 2, "-")
local function mul(a, b)
	return a * b
end
seed.__mul = coredef(mul, 2, "*")
local function div(a, b)
	return a / b
end
seed.__div = coredef(div, 2, "/")
local function pow(a, b)
	return a ^ b
end
seed.__pow = coredef(pow, 2, "^")
local function mod(a, b)
	return a % b
end
seed.__mod = coredef(mod, 2, "%")
local function eq(a, b)
	return a == b
end
seed.__eq = coredef(eq, 2, "=")
local function noteq(a, b)
	return a ~= b
end
seed.__noteq = coredef(noteq, 2, "!=")
local function lt(a, b)
	return a < b
end
seed.__lt = coredef(lt, 2, "<")
local function lteq(a, b)
	return a <= b
end
seed.__lteq = coredef(lteq, 2, "<=")
local function gt(a, b)
	return a > b
end
seed.__gt = coredef(gt, 2, ">")
local function gteq(a, b)
	return a >= b
end
seed.__gteq = coredef(gteq, 2, ">=")
local function conc(a, b)
	return a .. b
end
seed.__conc = coredef(conc, 2, "..")

seed.__par = coredef(seed.__eval, -1, "(", ")")

return seed
