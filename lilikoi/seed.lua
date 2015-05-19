
-- lilikoi/seed: the core lib used by lilikoi code after translation.
-- Written by Tommy Ettinger. Public Domain.

local seed = {}
local glue = require'glue'
local pp = require'pp'

local nests = {}

-- runs a partial function once it has been supplied all needed args.
-- returns the function's result, if it has one, followed by another return
-- "\0", or if it has not been supplied enough args, it returns the function
-- with a larger list of supplied args and no extra return. handles grouping.
function seed.__step(partial, arg, terminal)
	local f, given
	-- in the case that we have been given a functor, not a partial
	if partial["\6op"] then
		f = {}
		given = {}
		for k,v in pairs(partial) do f[k] = v end
		partial = {["\6f"]=f, ["\6g"]=given}
	else
		f = partial["\6f"]
		given = partial["\6g"]
	end
	if arg == nil then
		if f["\6group"] then
			nests[#nests + 1] = f["\6group"]
		end
		if #given == f["\6arity"] or (-1 == f["\6arity"] and terminal) then
			return f["\6op"](unpack(given)), "\0"
		else
			return partial, nil
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
	return partial, nil
end
-- takes a sequence of generated function tables and data, and
-- steps through it until it has exhausted the sequence,
-- returning the final stack (unpacked).
function seed.__eval(...)
	local ahead = glue.reverse({...})
	local stack = {}
	--local sexps = {}
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
			if type(stack[#stack]) == 'table' and ((stack[#stack]["\6op"] and
					stack[#stack]["\6group"]) or (stack[#stack]["\6f"] and
			stack[#stack]["\6f"]["\6op"] and stack[#stack]["\6f"]["\6group"])) then
				-- if we are continuing a function that is on the stack,
				-- and that function is a grouper, disregard starting any
				-- new functions and put the un-evaled args on the stack for
				-- later eval. Do the normal behavior for continuing,
				-- replace the top of the stack with the grouping function
				-- with the latest item received.
				local g, r
				r, g = seed.__step(stack[#stack], a, terminal) --sexps[#sexps], 
				if(g ~= "\0") then
					stack[#stack] = r
					-- sexps[#sexps] = g
				else
					table.remove(stack)
					table.insert(ahead, r)
					--table.remove(sexps)
				end
			elseif a["\6group"] or (a["\6f"] and a["\6f"]["\6group"]) then
			-- if we have just started executing a grouping function,
			-- ignore the current stack and start the grouping.
				stack[#stack + 1] = seed.__step(a, nil, terminal)
--				if(g ~= "\0") then
--					sexps[#sexps + 1] = g
--				end
			else
			-- if we have just started executing a function,
			-- replace the top of the stack with the function called
			-- with the content of the top of the stack.
				local start = #stack
				if start == 0 then start = 1 end
				stack[start] = seed.__step(a, stack[#stack], terminal)
--				if(g ~= "\0") then
--					sexps[#sexps + 1] = g
--				end
			end
		elseif type(stack[#stack]) == 'table' and
			(stack[#stack]["\6op"] or stack[#stack]["\6f"]["\6op"]) then
			-- if we are continuing a function that is on the stack,
			-- replace the top of the stack with the function part-called
			-- with the latest item received.
			stack[#stack] = seed.__step(stack[#stack], a, terminal)
--			if(g ~= "\0") then
--				sexps[#sexps] = g
--			else
--				table.remove(sexps)
--			end
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

seed.__munge_table = {
["%"]="\6mod",
["\\"]="\6back",
}

local function semi_munge(name)
	return name:gsub(
		"^%.", "\6dot"):gsub(
			"%.$", "\6dot"):gsub(
				"[%%\\]", seed.__munge_table);
end

function seed.__def(op, arity, name, group)
	seed[semi_munge(name)] =
	{
		["\6op"] = op,
		["\6arity"] = arity,
		["\6name"] = name,
		["\6group"] = group
	}
end

function seed.__sequence(...)
	return {...}
end

local function basic_get(t, idxs)
	local elem = t
	if idxs ~= nil then
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

function seed.format(val)
	local tp = type(val)
	if val == nil then
		return '_'
	elseif tp == 'string' then
		return '"' .. val .. '"'
	elseif tp == 'number' then
		return tostring(val)
	elseif tp == 'table' then
		if val["\6name"] then
			return val["\6name"]
		elseif val["\6g"] and val["\6f"] and val["\6f"]["\6name"] then
			local s = '( ' .. val["\6f"]["\6name"] .. ' '
			for k,v in ipairs(val["\6g"]) do s = s .. seed.format(v) .. ' ' end
			s = s .. ')'
			return s
		else
			local s = '[ '
			for k,v in ipairs(val) do s = s .. seed.format(v) .. ' ' end
			s = s .. ']'
			return s
		end
	end
	return elem
end

seed.__def(glue.pass, -1, "(", ")")

seed.__def(glue.pass, 0, ")")

seed.__def(seed.__sequence, -1, "[", "]")

seed.__def(glue.pass, 0, "]")

seed.__def(basic_get, 2, "=get")

seed.math = math
seed.io = io
seed.file = file
seed.os = os
seed.string = string

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
   transpile = 'lilikoi.transpiler',
   execute = 'lilikoi.transpiler',
   munge = 'lilikoi.transpiler'
})