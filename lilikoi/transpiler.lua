
-- lilikoi/transpiler: an internal module for producing lua from lilikoi code.
-- Written by Tommy Ettinger. Public Domain.

local seed = require'lilikoi.seed'
local grammar = require'lilikoi.grammar'
local glue = require'glue'

local function transfer(capt)
	if capt[1] then
		if capt[2] == "_" then
			return '"\5%nil"'
		elseif capt[1] == 'STRING' or capt[1] == 'NUMBER' then
			return capt[2]
		elseif capt[1] == 'COMMENT' then
			return capt[2] .. "\n"
		elseif capt[1] == 'KEYWORD' then
			return '"\5' .. capt[2] .. '"'
		elseif capt[1] == 'IDENTIFIER' then
			return '"\5%' .. seed.munge(capt[2]) .. '"'
		end
	end
	return " "
end

function seed.transpile(llk, retain)
	local lexed = grammar.lex(llk)
	local lu = 'local __s=__s or require"lilikoi.seed"\nreturn __s.__run({"\5%(",'
  if retain then lu = 'local __s=__s or require"lilikoi.seed"\nreturn __s.__run_in({"\5%(",' end
	local c = seed._map(transfer, lexed)
	if #c > 0 then
		for i,v in ipairs(c) do
			if v:sub(-1) == '\n' then 
				lu = lu .. v
			else
				lu = lu .. v .. ','
			end
		end
	end
  lu = lu .. '"\5%)"})'
	return lu
end

function seed.execute(llk)
	return assert(loadstring(seed.transpile(llk)))()
end

function seed.execute_in(llk)
	return assert(loadstring(seed.transpile(llk, true)))()
end

return transpiler
