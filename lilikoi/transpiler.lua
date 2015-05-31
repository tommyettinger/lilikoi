
-- lilikoi/transpiler: an internal module for producing lua from lilikoi code.
-- Written by Tommy Ettinger. Public Domain.

local seed = require'lilikoi.seed'
local grammar = require'lilikoi.grammar'
local glue = require'glue'
local fun = require 'fun' ()
local function transfer(capt)
	if capt[1] then
		if capt[2] == "nil" then
			return '"\1nil"'
		elseif capt[2] == "true" then
			return 'true'
		elseif capt[2] == "false" then
			return 'false'
		elseif capt[1] == 'STRING' then
			return '"\2'.. capt[2] .. '"'
		elseif capt[1] == 'NUMBER' then
			return capt[2]
		elseif capt[1] == 'COMMENT' then
			return '--' .. capt[2] .. '\n'
		elseif capt[1] == 'KEYWORD' then
			return '"\5' .. capt[2] .. '"'
		elseif capt[1] == 'IDENTIFIER' then
			return '"\1' .. capt[2] .. '"'
		elseif capt[1] == 'CHAIN' then
			return '{"\1chain",' .. reduce(function(a, b) return a .. "," .. b end, map(transfer, capt[2])) .. '}'
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
