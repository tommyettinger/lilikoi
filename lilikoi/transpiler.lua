
-- lilikoi/transpiler: an internal module for producing lua from lilikoi code.
-- Written by Tommy Ettinger. Public Domain.

local transpiler = {}
local grammar = require'lilikoi.grammar'
local glue = require'glue'
local va = require'vararg'

--[[
["-"]="_",
["+"]="\5add",
["*"]="\5mul",
["/"]="\5div",
["^"]="\5pow",
["="]="\5eq",
["!"]="\5bang",
["~"]="\5wave",
["`"]="\5tick",
["#"]="\5hash",
["$"]="\5cash",
["&"]="\5and",
["|"]="\5pipe",
["@"]="\5at",
[";"]="\5semi",
["<"]="\5lt",
[">"]="\5gt",
["("]="\5lpar",
[")"]="\5rpar",
["["]="\5lsq",
["]"]="\5rsq",
["{"]="\5lcurl",
["}"]="\5rcurl",

[":"]="\5col",
--]]

local munge_table = {
["%"]="\6mod",
["\\"]="\6back",
}

function transpiler.munge(name)
	return name:gsub(
		"^%.", "\6dot"):gsub(
			"%.$", "\6dot"):gsub(
				"%.", '"]["'):gsub(
					"[%%\\]", munge_table);
end

local function transfer(capt)
	if capt[1] then
		if capt[2] == "_" then
			return "nil"
		elseif capt[1] == 'STRING' or capt[1] == 'NUMBER' then
			return capt[2]
		elseif capt[1] == 'COMMENT' then
			return capt[2] .. "\n"
		elseif capt[1] == 'KEYWORD' then
			return '"\5' .. capt[2] .. '"'
		elseif capt[1] == 'IDENTIFIER' then
			return '__s["' .. transpiler.munge(capt[2]) .. '"]'
		end
	end
	return " "
end

function transpiler.gen(llk)
	local lexed = grammar.lex(llk)
	local lu = 'local __s=require"lilikoi.seed"\nreturn __s.__run('
	local c = va.pack(va.map(transfer, unpack(lexed)))
	local e1, erest = c(1), va.pack(c(2, c'#'))
	lu = lu .. e1
	if erest then
		for i,v in erest do
			if v:sub(-1) == '\n' then 
				lu = lu .. v
			else
				lu = lu .. ',' .. v
			end
		end
	end
	lu = lu .. ")"
	return lu
end

return transpiler
