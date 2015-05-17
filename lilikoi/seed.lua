
-- lilikoi/seed: the core lib used by lilikoi code after translation.
-- Written by Tommy Ettinger. Public Domain.

local seed = {}
local glue = require'glue'

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
local function coredef(op, arity, show, ender)
	return {["\6op"] = op,
			["\6arity"] = arity,
			["\6name"] = show,
			["\6group"] = ender,
			}
end

seed.__par = coredef(seed.__eval, -1, "(", ")")

return glue.autoload(seed,
{
   __add = 'operators',
   __sub = 'operators',
   __mul = 'operators',
   __div = 'operators',
   __pow = 'operators',
   __mod = 'operators',
   __eq = 'operators',
   __bang__eq = 'operators',
   __lt = 'operators',
   __lt__eq = 'operators',
   __gt = 'operators',
   __gt__eq = 'operators',
   concat = 'operators',
   
})