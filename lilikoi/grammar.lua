-- lilikoi/grammar: Simple lexer/parser for lilikoi code.
-- Written by Tommy Ettinger. Public Domain.
-- Original for Lua written by Peter Odding, 2007/04/04.
-- Includes portions of the Scintilla lexer by Mitchell, MIT license.
--[[ License for lexer.lua from Scintilla provided here.
The MIT License

Copyright (c) 2007-2015 Mitchell

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
--]]

local M = {}

local lpeg = require('lpeg')

local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local Cc, Ct, Cp = lpeg.Cc, lpeg.Ct, lpeg.Cp
local C, Cg, Cb, Cmt = lpeg.C, lpeg.Cg, lpeg.Cb, lpeg.Cmt

local lpmatch = lpeg.match


-- Common patterns.
M.any = P(1)
M.ascii = R('\000\127')
M.extend = R('\000\255')
M.alpha = R('AZ', 'az')
M.digit = R('09')
M.alnum = R('AZ', 'az', '09')
M.lower = R('az')
M.upper = R('AZ')
M.xdigit = R('09', 'AF', 'af')
M.cntrl = R('\000\031')
M.graph = R('!~')
M.print = R(' ~')
M.punct = R('!/', ':@', '[\'', '{~')
M.space = S('\t\v\f\n\r ')

M.newline = S('\r\n\f')^1
M.nonnewline = 1 - M.newline
M.nonnewline_esc = 1 - (M.newline + '\\') + '\\' * M.any

M.dec_num = M.digit^1
M.hex_num = '0' * S('xX') * M.xdigit^1
M.oct_num = '0' * R('07')^1
M.integer = S('+-')^-1 * (M.hex_num + M.oct_num + M.dec_num)
M.float = S('+-')^-1 *
          (M.digit^0 * '.' * M.digit^1 + M.digit^1 * '.' * M.digit^0 +
           M.digit^1) *
          S('eE') * S('+-')^-1 * M.digit^1
M.word = (M.alpha + '_') * (M.alnum + '_')^0

---
-- Creates and returns a token pattern with token name *name* and pattern
-- *patt*.
-- @param name The kind of token. Used for logic stuff. A string.
-- @param patt The LPeg pattern associated with the token.
-- @return pattern
-- @usage local ws = token(l.WHITESPACE, l.space^1)
-- @usage local annotation = token('annotation', '@' * l.word)
-- @name token
local function token(name, patt)
  return Cg(Cc(name) * C(patt) * Cp())
end
M.token = token
---
-- Creates and returns a pattern that matches a range of text bounded by
-- *chars* characters.
-- This is a convenience function for matching more complicated delimited ranges
-- like strings with escape characters and balanced parentheses. *single_line*
-- indicates whether or not the range must be on a single line, *no_escape*
-- indicates whether or not to ignore '\' as an escape character, and *balanced*
-- indicates whether or not to handle balanced ranges like parentheses and
-- requires *chars* to be composed of two characters.
-- @param chars The character(s) that bound the matched range.
-- @param single_line Optional flag indicating whether or not the range must be
--   on a single line.
-- @param no_escape Optional flag indicating whether or not the range end
--   character may be escaped by a '\\' character.
-- @param balanced Optional flag indicating whether or not to match a balanced
--   range, like the "%b" Lua pattern. This flag only applies if *chars*
--   consists of two different characters (e.g. "()").
-- @return pattern
-- @usage local dq_str_escapes = l.delimited_range('"')
-- @usage local dq_str_noescapes = l.delimited_range('"', false, true)
-- @usage local unbalanced_parens = l.delimited_range('()')
-- @usage local balanced_parens = l.delimited_range('()', false, false, true)
-- @see nested_pair
-- @name delimited_range
local function delimited_range(chars, single_line, no_escape, balanced)
  local s = chars:sub(1, 1)
  local e = #chars == 2 and chars:sub(2, 2) or s
  local range
  local b = balanced and s or ''
  local n = single_line and '\n' or ''
  if no_escape then
    local invalid = S(e..n..b)
    range = M.any - invalid
  else
    local invalid = S(e..n..b) + '\\'
    range = M.any - invalid + '\\' * M.any
  end
  if balanced and s ~= e then
    return P{s * (range + V(1))^0 * e}
  else
    return s * range^0 * P(e)^-1
  end
end
M.delimited_range = delimited_range




local lexer = {}
local space = S('\t\v\f\n\r ,')
-- Whitespace.
local ws = token('WHITESPACE', space^1)

local equals = P"="^0
local open = "[" * Cg(equals, "init") * "[" * P"\n"^-1
local close = "]" * C(equals) * "]"
local closeeq = Cmt(close * Cb("init"), function (s, i, a, b) return a == b end)
local longstring = open * C((P(1) - closeeq)^0) * close / 1

-- Comments.
local line_comment = '--' * M.nonnewline^0
local block_comment = '--' * longstring
local comment = token('COMMENT', block_comment + line_comment)

-- Strings.
local sq_str = delimited_range("'")
local dq_str = delimited_range('"')
local string = token('STRING', sq_str + dq_str + longstring)

-- Numbers.
local lj_int = S('-')^-1 * ((M.dec_num + M.hex_num) * (P('ULL') + P('ull') + P('LL') + P('ll'))^-1)
local number = token('NUMBER', M.float + lj_int)

-- Identifiers.

local un_ids = S("\"',")^1
local ids = 1 - un_ids
local identifier = token('IDENTIFIER', ids)

lexer._rules = {
  {'whitespace', ws},
  {'string', string},
  {'comment', comment},
  {'number', number},
  {'identifier', identifier},
}

-- Adds a rule to a lexer's current ordered list of rules.
-- @param lexer The lexer to add the given rule to.
-- @param id The name associated with this rule. It is used for other lexers
--   to access this particular rule from the lexer's `_RULES` table. It does not
--   have to be the same as the name passed to `token`.
-- @param rule The LPeg pattern of the rule.
local function add_rule(id, rule)
  if not lexer._RULES then
    lexer._RULES = {}
    -- Contains an ordered list (by numerical index) of rule names. This is used
    -- in conjunction with lexer._RULES for building _TOKENRULE.
    lexer._RULEORDER = {}
  end
  lexer._RULES[id] = rule
  lexer._RULEORDER[#lexer._RULEORDER + 1] = id
end

for i, v in ipairs(lexer._rules) do add_rule(v[1], v[2]) end

-- (Re)constructs `lexer._TOKENRULE`.
-- @param parent The parent lexer.
local function join_tokens()
  local patterns, order = lexer._RULES, lexer._RULEORDER
  local token_rule = patterns[order[1]]
  for i = 2, #order do token_rule = token_rule + patterns[order[i]] end
  lexer._TOKENRULE = token_rule -- + M.token(M.DEFAULT, M.any)
  return lexer._TOKENRULE
end
lexer._GRAMMAR = Ct(join_tokens()^0)


---
-- Lexes a chunk of text *text*.
-- The text is lexed as a whole.
-- @param text The text in the buffer to lex.
-- @return table of token names and positions.
-- @name lex
function M.lex(text)
    return lpmatch(lexer._GRAMMAR, text)
end

return M
