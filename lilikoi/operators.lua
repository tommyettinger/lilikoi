local operators = {}

local function coredef(op, arity, show)
	return {["\6op"] = op,
			["\6arity"] = arity,
			["\6name"] = show,
			}
end

local function add(a, b)
	return a + b
end
operators.__add = coredef(add, 2, "+")
local function sub(a, b)
	return a - b
end
operators.__sub = coredef(sub, 2, "-")
local function mul(a, b)
	return a * b
end
operators.__mul = coredef(mul, 2, "*")
local function div(a, b)
	return a / b
end
operators.__div = coredef(div, 2, "/")
local function pow(a, b)
	return a ^ b
end
operators.__pow = coredef(pow, 2, "^")
local function mod(a, b)
	return a % b
end
operators.__mod = coredef(mod, 2, "%")
local function eq(a, b)
	return a == b
end
operators.__eq = coredef(eq, 2, "=")
local function noteq(a, b)
	return a ~= b
end
operators.__bang__eq = coredef(noteq, 2, "!=")
local function lt(a, b)
	return a < b
end
operators.__lt = coredef(lt, 2, "<")
local function lteq(a, b)
	return a <= b
end
operators.__lt__eq = coredef(lteq, 2, "<=")
local function gt(a, b)
	return a > b
end
operators.__gt = coredef(gt, 2, ">")
local function gteq(a, b)
	return a >= b
end
operators.__gt__eq = coredef(gteq, 2, ">=")
local function concat(a, b)
	return a .. b
end
operators.concat = coredef(concat, 2, "concat")
