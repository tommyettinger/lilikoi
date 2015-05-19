
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
function seed.__step(f, given, arg, terminal)
	if arg == nil then
		if f["\6group"] then
			nests[#nests + 1] = f["\6group"]
		end
		if #given == f["\6arity"] or (-1 == f["\6arity"] and terminal) then
			return f["\6op"](unpack(given)), "\0"
		else
			return f, given
		end
	end
	if type(arg) == 'table' and arg["\6op"] and nests[#nests] == arg["\6name"] then
		table.remove(nests)
		if #nests == 0 then
			return f["\6op"](seed.__eval(unpack(given))), "\0"
		else
			given[#given + 1] = arg
		end
	elseif type(arg) == 'table' and arg["\6op"] and arg["\6group"] then
		nests[#nests + 1] = arg["\6group"]
		given[#given + 1] = arg
	elseif f["\6arity"] == -1 or #given < f["\6arity"] then
		given[#given + 1] = arg
	end
	if #given == f["\6arity"] or (-1 == f["\6arity"] and terminal) then
		return f["\6op"](unpack(given)), "\0"
	end
	return f, given
end
-- takes a sequence of generated function tables and data, and
-- steps through it until it has exhausted the sequence,
-- returning the final stack (unpacked).
function seed.__eval(...)
	local ahead = glue.reverse({...})
	local stack = {}
	local sexps = {}
	while #ahead > 0 do
		-- consume any tokens that were used
		local a = table.remove(ahead)
		local terminal = #ahead == 0
		if type(a) == 'function' then
			a = {["\6op"] = a,
				["\6arity"] = -1,
				["\6name"] = "__NATIVE"
			}
		end
		
		if type(a) == 'table' and a["\6op"] then
			if type(stack[#stack]) == 'table' and stack[#stack]["\6op"] and
					stack[#stack]["\6group"] then
				-- if we are continuing a function that is on the stack,
				-- and that function is a grouper, disregard starting any
				-- new functions and put the quoted args on the stack for
				-- later eval. Do the normal behavior for continuing,
				-- replace the top of the stack with the quoting function
				-- with the latest item received.
				local g, r
				r, g = seed.__step(stack[#stack], sexps[#sexps], a, terminal)
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
				stack[#stack + 1], g = seed.__step(a, {}, nil, terminal)
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
				stack[start], g = seed.__step(a, {}, stack[#stack], terminal)
				if(g ~= "\0") then
					sexps[#sexps + 1] = g
				end
			end
		elseif type(stack[#stack]) == 'table' and stack[#stack]["\6op"] then
			-- if we are continuing a function that is on the stack,
			-- replace the top of the stack with the function part-called
			-- with the latest item received.
			local g
			stack[#stack], g = seed.__step(stack[#stack], sexps[#sexps], a, terminal)
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
	return unpack(stack)
end

-- the entry point for a program. Clears any possible lingering state,
-- then returns any number of args (ideally 1, if the program
-- completed with one return value) based on evaluating a list of
-- code tables and data.
function seed.__run(...)
	nests = {}
	return seed.__eval(...)
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

function seed.__basic_get(t, idxs)
	local elem = t
	if idxs then
		if type(idxs) == 'table' then
			for i,v in ipairs(idxs) do
				elem = elem[v]
			end
		else
			return elem[idxs]
		end
	end
	return elem
end

seed["("] = seed.__def(glue.pass, -1, "(", ")", true)

seed[")"] = seed.__def(glue.pass, 0, ")")

seed["["] = seed.__def(seed.__sequence, -1, "[", "]")

seed["]"] = seed.__def(glue.pass, 0, "]")

seed["=get"] = seed.__def(seed.__basic_get, 2, "=get")

seed.math = math

return glue.autoload(seed,
{
   ["+"] = 'lilikoi.operators',
   ["-"] = 'lilikoi.operators',
   ["*"] = 'lilikoi.operators',
   ["/"] = 'lilikoi.operators',
   ["^"] = 'lilikoi.operators',
   ["\6mod"] = 'lilikoi.operators',
   ["="] = 'lilikoi.operators',
   ["!="] = 'lilikoi.operators',
   ["<"] = 'lilikoi.operators',
   ["<="] = 'lilikoi.operators',
   [">"] = 'lilikoi.operators',
   [">="] = 'lilikoi.operators',
   concat = 'lilikoi.operators',
})