
-- lilikoi/transpiler: an internal module for producing lua from lilikoi code.
-- Written by Tommy Ettinger. Public Domain.

local seed = require'lilikoi.seed'
local grammar = require'lilikoi.grammar'

local function transfer_helper(capt)
  if capt[3] == "nil" then
		return '{"nil","nil"},'
	elseif capt[3] == "true" then
		return '{"boolean",true},'
	elseif capt[3] == "false" then
		return '{"boolean",false},'
	elseif capt[1] == 'STRING' then
		return '{"string","' .. string.gsub(capt[3], "([\n\"\'])", "\\%1") .. '"},'
	elseif capt[1] == 'LONGSTRING' then
		return '{"string",' .. capt[3] ..'},' --		return '"string",' .. capt[2] .. capt[3] .. ','
	elseif capt[1] == 'NUMBER' then
		return '{"number",' .. capt[3] .. '},'
	elseif capt[1] == 'COMMENT' then
		return '--' .. capt[2] .. '\n'
	elseif capt[1] == 'KEYWORD' then
		return '{"keyword","' .. capt[3] .. '"},'
	elseif capt[1] == 'IDENTIFIER' then
    return '{"id","' .. capt[3] .. '"},'
  else
    return false
  end
 end

-- transfers all the elements in a lexed/parsed AST to a codeseq
local function transfer(capt, position)
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
        state[#state + 1] = '{"macro",{{"id","access"},'
        state[#state + 1] = transfer(term[3], 0)
      elseif term[1] == 'PAREN' then
        if term[3] == '(' then
          state[#state + 1] = '{"list",{'
          state[#state + 1] = transfer(term[4], 0)
        else
          state[#state + 1] = '{"macro",{{"id","lambda"},'
          state[#state + 1] = transfer(term[4], 0)
        end
      elseif term[1] == 'BRACKET' then
        state[#state + 1] = '{{"number",op="vector"},{'
        state[#state + 1] = transfer(term[4], 0)
      elseif term[1] == 'BRACE' then
        if term[2] == '{' then
          state[#state + 1] = '{{"any",op="dict"},{'
          state[#state + 1] = transfer(term[4], 0)
        else
          state[#state + 1] = '{{op="set"},{'
          state[#state + 1] = transfer(term[4], 0)
        end
      elseif term[1] == 'META' then
        state[#state + 1] = '{"macro",{{"id","attach-meta"},'
        state[#state + 1] = transfer(term[4], 0)
      elseif term[1] == 'PREFIX' then
        if term[3][1] == "'" then
          state[#state + 1] = '{"macro",{{"id","quote"},'
          state[#state + 1] = transfer(term[3], 1)
        elseif term[3][1] == '$' then
          state[#state + 1] = '{"macro",{{"id","auto-gensym"},'
          state[#state + 1] = transfer(term[3], 1)
        elseif term[3][1] == '~' then
          state[#state + 1] = '{"unquote",{'
          state[#state + 1] = transfer(term[3], 1)
        elseif term[3][1] == '`' then
          state[#state + 1] = '{"syntax",{'
          state[#state + 1] = transfer(term[3], 1)
        end
      end
    end
  end
  if position then
    return table.concat(state) .. '}},'
  else
    return table.concat(state)
  end
end

local function debug_helper(capt)
  if capt[1] == 'LONGSTRING' then
		return '{'..capt[2]..','..capt[5] ..'},'
  elseif capt[3] == "nil" or capt[3] == "true" or capt[3] == "false" then
		return '{'..capt[2]..','..capt[4]..'},'
	elseif capt[1] == 'STRING' or capt[1] == 'NUMBER' or capt[1] == 'KEYWORD' or capt[1] == 'IDENTIFIER' then
		return '{'..capt[2]..','..capt[4]..'},'
  else
    return false
  end
 end

-- transfers all the elements in a lexed/parsed AST to a debug seq
local function debug_transfer(capt, position)
  local state = {}
  local pos = position or 0
  while pos < #capt do
    pos = pos + 1
    local term = capt[pos]
    local th = debug_helper(term)
    if th then
      state[#state + 1] = th
    else
      -- we have encountered a non-simple form
      if term[1] == 'CHAIN' then
        state[#state + 1] = '{'..term[2]..','.. term[4]..',{{'..term[2] .. ',' .. term[4] .. '},'
        state[#state + 1] = debug_transfer(term[3], 0)
      elseif term[1] == 'PAREN' then
        if term[3] == '(' then
          state[#state + 1] = '{'..term[2]..','..term[5]..',{'
          state[#state + 1] = debug_transfer(term[4], 0)
        else
          state[#state + 1] = '{'..term[2]..','..term[5]..',{{' .. term[2] .. ',' .. term[5] .. '},'
          state[#state + 1] = debug_transfer(term[4], 0)
        end
      elseif term[1] == 'BRACKET' or term[1] == 'BRACE' then
        state[#state + 1] = '{'..term[2]..','..term[5]..',{'
        state[#state + 1] = debug_transfer(term[4], 0)
      elseif term[1] == 'META' then
        state[#state + 1] = '{'..term[2]..','..term[5]..',{{' .. term[2] .. ',' .. term[5] .. '},'
        state[#state + 1] = debug_transfer(term[4], 0)
      elseif term[1] == 'PREFIX' then
        state[#state + 1] = '{'..term[2]..','..term[5]..',{{' .. term[2] .. ',' .. term[5] .. '},'
        state[#state + 1] = debug_transfer(term[4], 0)
      end
    end
  end
  if position then
    return table.concat(state) .. '}},'
  else
    return table.concat(state)
  end
end

function seed.transpile(llk, retain, debug_mode)
	local lexed = grammar.lex(llk)
	local lu = 'local __s=__s or require"lilikoi.seed"\nreturn __s.run({{"id","do"},'
  if retain then lu = 'local __s=__s or require"lilikoi.seed"\nreturn __s.run_in({{"id","do"},' end
	lu = lu .. transfer(lexed) .. '})'
  if debug_mode then
    local db = "return {{0,0}," .. debug_transfer(lexed) .. "}"
    return lu, db
  else
    return lu
  end
end

function seed.execute(llk)
	return assert(loadstring(seed.transpile(llk)))()
end

function seed.execute_in(llk)
	return assert(loadstring(seed.transpile(llk, true)))()
end

return transpiler
