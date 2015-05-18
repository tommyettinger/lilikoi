
-- lilikoi/seed: the core lib used by lilikoi code after translation.
-- Written by Tommy Ettinger. Public Domain.

local seed = {}
local glue = require'glue'
local va = require'vararg'

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
	if type(arg) == 'table' and arg["\6op"] ~= nil and f["\6group"] == arg["\6name"] then
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

-- takes a sequence of generated function tables and data, and
-- steps through it until it has exhausted the sequence,
-- returning the final stack.
function seed.__eval(...)
	local v = va.pack(...)
	local stack = {}
	local sexps = {}
	for i,a in v do
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
					g.op = a
					sexps[#sexps + 1] = g
				end
			end
		elseif type(stack[#stack]) == 'table' and stack[#stack]["\6op"] ~= nil then
			-- if we are continuing a function that is on the stack,
			-- replace the top of the stack with the function called
			-- with the latest item received.
			local g
			stack[#stack], g = seed.__step(stack[#stack], sexps[#sexps], a)
			if(g ~= "\0") then
				g.op = stack[#stack]
				sexps[#sexps] = g
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
function seed.__def(op, arity, show, ender)
	return {["\6op"] = op,
			["\6arity"] = arity,
			["\6name"] = show,
			["\6group"] = ender,
			}
end

function seed.__sequence(...)
	return {...}
end

seed["("] = seed.__def(seed.__eval, -1, "(", ")")

seed[")"] = seed.__def(glue.pass, 0, ")")

seed["["] = seed.__def(seed.__sequence, -1, "[", "]")

seed["]"] = seed.__def(glue.pass, 0, "]")

return glue.autoload(seed,
{
   ["+"] = 'lilikoi.operators',
   ["-"] = 'lilikoi.operators',
   ["*"] = 'lilikoi.operators',
   ["/"] = 'lilikoi.operators',
   ["^"] = 'lilikoi.operators',
   ["\5mod"] = 'lilikoi.operators',
   ["="] = 'lilikoi.operators',
   ["!="] = 'lilikoi.operators',
   ["<"] = 'lilikoi.operators',
   ["<="] = 'lilikoi.operators',
   [">"] = 'lilikoi.operators',
   [">="] = 'lilikoi.operators',
   concat = 'lilikoi.operators',
   
})