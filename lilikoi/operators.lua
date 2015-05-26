local seed = require'lilikoi.seed'

local function coredef(op, arity, name)
	seed[name] = seed._functor(op, arity, name)  
end

local function add(a, b)
	return a + b
end
coredef(add, 2, "+")
local function sub(a, b)
	return a - b
end
coredef(sub, 2, "-")
local function mul(a, b)
	return a * b
end
coredef(mul, 2, "*")
local function div(a, b)
	return a / b
end
coredef(div, 2, "/")
local function pow(a, b)
	return a ^ b
end
coredef(pow, 2, "^")
local function mod(a, b)
	return a % b
end
seed["\6mod"] = seed._functor(mod, 2, "%")
local function eq(a, b)
	return a == b
end
coredef(eq, 2, "=")
local function noteq(a, b)
	return a ~= b
end
coredef(noteq, 2, "!=")
local function lt(a, b)
	return a < b
end
coredef(lt, 2, "<")
local function lteq(a, b)
	return a <= b
end
coredef(lteq, 2, "<=")
local function gt(a, b)
	return a > b
end
coredef(gt, 2, ">")
local function gteq(a, b)
	return a >= b
end
coredef(gteq, 2, ">=")
local function concat(a, b)
	return a .. b
end
coredef(concat, 2, "concat")
