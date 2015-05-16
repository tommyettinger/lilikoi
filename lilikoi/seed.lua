
-- lilikoi/seed: the core lib used by lilikoi code after translation.
-- Written by Tommy Ettinger. Public Domain.

local seed = {}

-- runs a partial function once it has been supplied all needed args,
-- which are stored in given. returns the function's result, if it has one,
-- or if it has not been supplied enough args, it returns the function
-- and a larger list of given args. handles grouping as well.
function seed.__step(f, given, arg)
	if a == nil then
		if #given == f["\6arity"] then
			return f["\6op"](unpack(given)), "\0"
		else
			return f, given
		end
	end
	if f["\6group"] == arg then
		return f["\6op"](unpack(given)), "\0"
	elseif f["\6group"] ~= nil or
			#given < f["\6arity"] then
		table.insert(given, arg)
	end
	if #given == f["\6arity"] then
		return f["\6op"](unpack(given)), "\0"
	end
	return f, given
end


local function ap_helper(a, i)
  if i < a.n then return i+1,a[i+1] end
end
local function apairs(...)
  return ap_helper, {n=select('#', ...), ...}, 0
end

-- takes a sequence of generated function tables and data, and
-- steps through it until it has exhausted the sequence,
-- returning the final stack.
function seed.__eval(...)
	local stack = {}
	local sexps = {}
	for i,a in apairs(...) do
		if type(a) == 'table' and a["\6op"] ~= nil then
			if type(stack[#stack]) == 'table' and stack[#stack]["\6op"] ~= nil and
					stack[#stack]["\6quote"] ~= nil then
				-- if we are continuing a function that is on the stack,
				-- and that function quotes its arguments, disregard
				-- starting any new functions and put the quoted stuff
				-- on the stack. Do the normal behavior for continuing,
				-- replace the top of the stack with the quoting function
				-- with the latest item received.
				local g
				stack[#stack], g = seed.__step(stack[#stack], sexps[#sexps], a)
				if(g ~= "\0") then
					sexps[#sexps] = {op=stack[#stack], unpack(g)}
				else
					table.remove(sexps)
				end
			else
			-- if we have just started executing a function,
			-- replace the top of the stack with the function called
			-- with the content of the top of the stack.
				local g
				stack[#stack], g = seed.__step(a, {}, stack[#stack])
				if(g ~= "\0") then
					sexps[#sexps + 1] = {op=a, unpack(g)}
				end
			end
		elseif type(stack[#stack]) == 'table' and stack[#stack]["\6op"] ~= nil then
			-- if we are continuing a function that is on the stack,
			-- replace the top of the stack with the function called
			-- with the latest item received.
			local g
			stack[#stack], g = seed.__step(stack[#stack], sexps[#sexps], a)
			if(g ~= "\0") then
				sexps[#sexps] = {op=stack[#stack], unpack(g)}
			else
				table.remove(sexps)
			end
		else
			-- if we are not continuing or starting a function,
			-- append a piece of data to the stack.
			stack[#stack + 1] = a
		end
	end
	return stack
end
local function coredef(op, arity, show, ender, quote)
	return {["\6op"] = op,
			["\6arity"] = arity,
			["\6name"] = show,
			["\6group"] = ender,
			["\6quote"] = quote,
			}
end

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
local function concat(a, b)
	return a .. b
end
seed.__concat = coredef(concat, 2, "..")

seed.__par = coredef(seed.__eval, -1, "(", ")")

return seed
