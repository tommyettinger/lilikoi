
-- lilikoi/seed: the core lib used by lilikoi code after translation.
-- Written by Tommy Ettinger. Public Domain.

local seed = {}
local glue = require'glue'
local va = require'vararg'
local pp = require'pp'

local nests = {}

-- runs a partial function once it has been supplied all needed args,
-- which are stored in given. returns the function's result, if it has one,
-- or if it has not been supplied enough args, it returns the function
-- and a larger list of given args. handles grouping as well.
function seed.__step(f, given, arg)
	if arg == nil then
		if f["\6group"] then
			nests[#nests + 1] = f["\6group"]
		end
		if #given == f["\6arity"] then
			return f["\6op"](unpack(given)), "\0"
		else
			return f, given
		end
	end
	if type(arg) == 'table' and arg["\6op"] and nests[#nests] == arg["\6name"] then
		table.remove(nests)
		if #nests == 0 then
			return f["\6op"](unpack(given)), "\0"
		else
			given[#given + 1] = arg
		end
	elseif type(arg) == 'table' and arg["\6op"] and arg["\6group"] then
		nests[#nests + 1] = arg["\6group"]
		given[#given + 1] = arg
	elseif f["\6group"] or #given < f["\6arity"] then
		given[#given + 1] = arg
	end
	if #given == f["\6arity"] then
		return f["\6op"](unpack(given)), "\0"
	end
	return f, given
end
-- the full list of unconsumed tokens.
----seed.__ahead = nil
-- takes a sequence of generated function tables and data, and
-- steps through it until it has exhausted the sequence,
-- returning the final stack.
function seed.__eval(...)
	local ahead = glue.reverse({...})
	local stack = {}
	local sexps = {}
	while #ahead > 0 do
		-- consume any tokens that were used
		local a = table.remove(ahead)
		
		if type(a) == 'table' and a["\6op"] then
			if type(stack[#stack]) == 'table' and stack[#stack]["\6op"] and
					stack[#stack]["\6quote"] then
				-- if we are continuing a function that is on the stack,
				-- and that function quotes its arguments, disregard
				-- starting any new functions and put the quoted stuff
				-- on the stack. Do the normal behavior for continuing,
				-- replace the top of the stack with the quoting function
				-- with the latest item received.
				local g, r
				r, g = seed.__step(stack[#stack], sexps[#sexps], a)
				if(g ~= "\0") then
					stack[#stack] = r
					sexps[#sexps] = g
				else
					table.remove(stack)
					table.insert(ahead, r)
					table.remove(sexps)
				end
			elseif a["\6group"] then
			-- if we have just started executing a grouping function,
			-- ignore the current stack and start the grouping.
				local g
				stack[#stack + 1], g = seed.__step(a, {}, nil)
				if(g ~= "\0") then
					sexps[#sexps + 1] = g
				end
			else
			-- if we have just started executing a function,
			-- replace the top of the stack with the function called
			-- with the content of the top of the stack.
				local g
				local start = #stack
				if start == 0 then start = 1 end
				stack[start], g = seed.__step(a, {}, stack[#stack])
				if(g ~= "\0") then
					sexps[#sexps + 1] = g
				end
			end
		elseif type(stack[#stack]) == 'table' and stack[#stack]["\6op"] then
			-- if we are continuing a function that is on the stack,
			-- replace the top of the stack with the function called
			-- with the latest item received.			
			local g
			stack[#stack], g = seed.__step(stack[#stack], sexps[#sexps], a)
			if(g ~= "\0") then
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
	if #stack == 1 then return stack[1] end
	return stack
end
function seed.__def(op, arity, show, ender, quote)
	return {["\6op"] = op,
			["\6arity"] = arity,
			["\6name"] = show,
			["\6group"] = ender,
			["\6quote"] = quote
			}
end

function seed.__sequence(...)
	return {...}
end

seed["("] = seed.__def(seed.__eval, -1, "(", ")", true)

seed[")"] = seed.__def(glue.pass, 0, ")")

seed["["] = seed.__def(seed.__sequence, -1, "[", "]", true)

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