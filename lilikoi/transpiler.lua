
-- lilikoi/transpiler: an internal module for producing lua from lilikoi code.
-- Written by Tommy Ettinger. Public Domain.

local seed = require'lilikoi.seed'
local grammar = require'lilikoi.grammar'
local fun = require 'fun' ()


local function transfer_helper(capt)
  if capt[2] == "nil" then
		return '"\1nil",'
	elseif capt[2] == "true" then
		return 'true,'
	elseif capt[2] == "false" then
		return 'false,'
	elseif capt[1] == 'STRING' then
		return '"\2'.. capt[2] .. '",'
	elseif capt[1] == 'NUMBER' then
		return capt[2] .. ","
	elseif capt[1] == 'COMMENT' then
		return '--' .. capt[2] .. '\n'
	elseif capt[1] == 'KEYWORD' then
		return '"\5' .. capt[2] .. '",'
	elseif capt[1] == 'IDENTIFIER' then
    return '"\1' .. capt[2] .. '",'
  else
    return false
  end
 end
 
---
-- transfers all the elements in a lexed/parsed AST to a code string
local function transfer(capt, pos)
  local state = {}
  pos = pos or 0
  while pos < #capt do
    pos = pos + 1
    local term = capt[pos]
    local th = transfer_helper(term)
    if th then
      state[#state + 1] = th
    else
      state.close = true
      -- we have encountered a non-simple form
      if term[1] == 'CHAIN' then
        state[#state + 1] = '{"\1chain",'
        state[#state + 1] = transfer(term, 1)        
      elseif term[1] == 'PAREN' then
        if term[2] == '(' then
          state[#state + 1] = '{'
          state[#state + 1] = transfer(term, 2)
        else
          state[#state + 1] = '{"\1lambda",'
          state[#state + 1] = transfer(term, 2)
        end
      elseif term[1] == 'BRACKET' then
        if term[2] == '[' then
          state[#state + 1] = '{"\1vector",'
          state[#state + 1] = transfer(term, 2)
        else
          state[#state + 1] = '{"\1tuple",'
          state[#state + 1] = transfer(term, 2)
        end
      elseif term[1] == 'BRACE' then
        if term[2] == '{' then
          state[#state + 1] = '{"\1dict",'
          state[#state + 1] = transfer(term, 2)
        else
          state[#state + 1] = '{"\1set",'
          state[#state + 1] = transfer(term, 2)
        end
      elseif term[1] == 'META' then
          state[#state + 1] = '{"\1attach-meta",'
          state[#state + 1] = transfer(term, 1)
      elseif term[1] == 'PREFIX' then
        if term[2] == "'" then
          state[#state + 1] = '{"\1quote",'
          state[#state + 1] = transfer(term, 2)
        elseif term[2] == '$' then
          state[#state + 1] = '{"\1auto-gensym",'
          state[#state + 1] = transfer(term, 2)
        elseif term[2] == '~' then
          state[#state + 1] = '{"\1unquote",'
          state[#state + 1] = transfer(term, 2)
        elseif term[2] == '`' then
          state[#state + 1] = '{"\1syntax-quote",'
          state[#state + 1] = transfer(term, 2)
        end
      end
    end
  end
  if state.close then
    return table.concat(state) .. '},'
  else
    return table.concat(state)
  end
end

function seed.transpile(llk, retain)
	local lexed = grammar.lex(llk)
	local lu = 'local __s=__s or require"lilikoi.seed"\nreturn __s.__run({"\1do",'
  if retain then lu = 'local __s=__s or require"lilikoi.seed"\nreturn __s.__run_in({"\1do",' end
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
