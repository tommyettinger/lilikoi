
-- lilikoi/transpiler: an internal module for producing lua from lilikoi code.
-- Written by Tommy Ettinger. Public Domain.

local seed = require'lilikoi.seed'
local grammar = require'lilikoi.grammar'

local function transfer_helper(capt)
  if capt[3] == "nil" then
		return '{"nil","nil",'..capt[2]..','..capt[4]..'},'
	elseif capt[3] == "true" then
		return '{"boolean",true,'..capt[2]..','..capt[4]..'},'
	elseif capt[3] == "false" then
		return '{"boolean",false,'..capt[2]..','..capt[4]..'},'
	elseif capt[1] == 'STRING' then
		return '{"string","' .. string.gsub(capt[3], "([\n\"\'])", "\\%1") .. '",'..capt[2]..','..capt[4]..'},'
	elseif capt[1] == 'LONGSTRING' then
		return '{"string",' .. capt[3] .. ',' .. capt[2] .. ',' .. capt[5] ..'},' --		return '"string",' .. capt[2] .. capt[3] .. ','
	elseif capt[1] == 'NUMBER' then
		return '{"number",' .. capt[3] .. ','..capt[2]..','..capt[4]..'},'
	elseif capt[1] == 'COMMENT' then
		return '--' .. capt[2] .. '\n'
	elseif capt[1] == 'KEYWORD' then
		return '{"keyword","' .. capt[3] .. '",'..capt[2]..','..capt[4]..'},'
	elseif capt[1] == 'IDENTIFIER' then
    return '{"id","' .. capt[3] .. '",'..capt[2]..','..capt[4]..'},'
  else
    return false
  end
 end
 
---
-- transfers all the elements in a lexed/parsed AST to a code string
local function transfer(capt, position, inner_start, inner_end)
  local state = {}
  local pos = position or 0
  while pos < #capt do
    pos = pos + 1
    local term = capt[pos]
    local th = transfer_helper(term)
    if th then
      state[#state + 1] = th
    else
      -- we have encountered a non-simple form
      if term[1] == 'CHAIN' then
        state[#state + 1] = '{"macro",{{"id","access",' .. term[2] .. ',' .. term[4] .. '},'
        state[#state + 1] = transfer(term[3], 0, term[2], term[4])
      elseif term[1] == 'PAREN' then
        if term[3] == '(' then
          state[#state + 1] = '{"list",{'
          state[#state + 1] = transfer(term[4], 0, term[2], term[5])
        else
          state[#state + 1] = '{"macro",{{"id","lambda",' .. term[2] .. ',' .. term[5] .. '},'
          state[#state + 1] = transfer(term[4], 0, term[2], term[5])
        end
      elseif term[1] == 'BRACKET' then
        state[#state + 1] = '{{"number",op="vector"},{'
        state[#state + 1] = transfer(term[4], 0, term[2], term[5])
      elseif term[1] == 'BRACE' then
        if term[2] == '{' then
          state[#state + 1] = '{{"any",op="dict"},{'
          state[#state + 1] = transfer(term[4], 0, term[2], term[5])
        else
          state[#state + 1] = '{{op="set"},{'
          state[#state + 1] = transfer(term[4], 0, term[2], term[5])
        end
      elseif term[1] == 'META' then
          state[#state + 1] = '{"macro",{{"id","attach-meta",' .. term[2] .. ',' .. term[5] .. '},'
          state[#state + 1] = transfer(term[4], 0, term[2], term[5])
      elseif term[1] == 'PREFIX' then
        if term[2] == "'" then
          state[#state + 1] = '{"macro",{{"id","quote",' .. term[2] .. ',' .. term[5] .. '},'
          state[#state + 1] = transfer(term[4], 0, term[2], term[5])
        elseif term[2] == '$' then
          state[#state + 1] = '{"macro",{{"id","auto-gensym",' .. term[2] .. ',' .. term[5] .. '},'
          state[#state + 1] = transfer(term[4], 0, term[2], term[5])
        elseif term[2] == '~' then
          state[#state + 1] = '{"unquote",{{"id","unquote",' .. term[2] .. ',' .. term[5] .. '},'
          state[#state + 1] = transfer(term[4], 0, term[2], term[5])
        elseif term[2] == '`' then
          state[#state + 1] = '{"syntax",{{"id","syntax-quote",' .. term[2] .. ',' .. term[5] .. '},'
          state[#state + 1] = transfer(term[4], 0, term[2], term[5])
        end
      end
    end
  end
  if position then
    return table.concat(state) .. '},'.. inner_start .. ',' .. inner_end ..'},'
  else
    return table.concat(state)
  end
end

function seed.transpile(llk, retain)
	local lexed = grammar.lex(llk)
	local lu = 'local __s=__s or require"lilikoi.seed"\nreturn __s.run({{"id","do",0,0},'
  if retain then lu = 'local __s=__s or require"lilikoi.seed"\nreturn __s.run_in({{"id","do",0,0},' end
	lu = lu .. transfer(lexed) .. '})'
	return lu
end

function seed.execute(llk)
	return assert(loadstring(seed.transpile(llk)))()
end

function seed.execute_in(llk)
	return assert(loadstring(seed.transpile(llk, true)))()
end

return transpiler
