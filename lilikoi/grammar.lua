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
local patterns = {}

-- Common patterns.
patterns.any = P(1)
patterns.ascii = R('\000\127')
patterns.extend = R('\000\255')
patterns.alpha = R('AZ', 'az')
patterns.digit = R('09')
patterns.alnum = R('AZ', 'az', '09')
patterns.lower = R('az')
patterns.upper = R('AZ')
patterns.xdigit = R('09', 'AF', 'af')
-- patterns.cntrl = R('\000\031')
-- patterns.graph = R('!~')
-- patterns.print = R(' ~')
-- patterns.punct = R('!/', ':@', '[\'', '{~')
-- patterns.space = S('\t\v\f\n\r ')

patterns.newline = S('\r\n\f')^1
patterns.nonnewline = 1 - patterns.newline
patterns.nonnewline_esc = 1 - (patterns.newline + '\\') + '\\' * patterns.any

patterns.dec_num = patterns.digit^1
patterns.hex_num = '0' * S('xX') * patterns.xdigit^1
patterns.oct_num = '0' * R('07')^1
patterns.integer = S('+-')^-1 * (patterns.hex_num + patterns.dec_num)
patterns.float = S('+-')^-1 *
          (patterns.digit^0 * '.' * patterns.digit^1 + patterns.digit^1 * '.' * patterns.digit^0 +
           patterns.digit^1) *
          (S('eE') * S('+-')^-1 * patterns.digit^1)^-1
-- patterns.word = (patterns.alpha + '_') * (patterns.alnum + '_')^0

---
-- Creates and returns a token pattern with token name *name* and pattern
-- *patt*.
-- @param name The kind of token. Used for logic stuff. A string.
-- @param patt The LPeg pattern associated with the token.
-- @return pattern
-- @usage local ws = token('WHITESPACE', l.space^1)
-- @name token
local function token(name, patt)
  if name == 'WHITESPACE' then
    return patt  -- do not capture whitespace.
  end
  return Ct(Cc(name) * C(patt) * Cp())
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
    range = patterns.any - invalid
  else
    local invalid = S(e..n..b) + '\\'
    range = patterns.any - invalid + '\\' * patterns.any
  end
  if balanced and s ~= e then
    return P{s * (range + V(1))^0 * e}
  else
    return s * range^0 * P(e) -- was:    return s * range^0 * P(e)^-1
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
local longstring = open * (P(1) - closeeq)^0 * close / 1

-- Comments.
local line_comment = '--' * patterns.nonnewline^0
local block_comment = '--' * longstring
local comment = token('COMMENT', block_comment + line_comment) * (space^1 + -1)

-- Strings.
local sq_str = delimited_range("'")
local dq_str = delimited_range('"')
local string = token('STRING', sq_str + dq_str + longstring) * (space^1 + -1)

-- Numbers.
local lj_int = S('-')^-1 * ((patterns.dec_num + patterns.hex_num) * (P('ULL') + P('ull') + P('LL') + P('ll'))^-1)
local number = token('NUMBER', patterns.float + lj_int) * (space^1 + -1)

-- Identifiers.

local un_ids = S("\"',")^1
local ids = 1 - un_ids
local identifier = token('IDENTIFIER', ids) * (space^1 + -1)

lexer._RULES = {
  whitespace=ws,
  string=string,
  comment=comment,
  number=number,
  identifier=identifier
}
lexer._RULEORDER = {
  'whitespace',
  'string',
  'comment',
  'number',
  'identifier'
}

-- (Re)constructs `lexer._TOKENRULE`.
local function join_tokens()
  local patterns, order = lexer._RULES, lexer._RULEORDER
  local token_rule = patterns[order[1]]
  for i = 2, #order do token_rule = token_rule + patterns[order[i]] end
  lexer._TOKENRULE = token_rule -- + M.token('DEFAULT', patterns.any)
  return lexer._TOKENRULE
end
lexer._GRAMMAR = Ct(join_tokens()^0)


---
-- Lexes a chunk of text *text*.
-- The text is lexed as a whole.
-- @param text The text in the buffer to lex.
-- @return table of token kinds, contents, positions.
-- @name lex
function M.lex(text)
    return lpmatch(lexer._GRAMMAR, text)
end

return M
