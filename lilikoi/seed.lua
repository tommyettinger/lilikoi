
-- lilikoi/seed: the core lib used by lilikoi code after translation.
-- Written by Tommy Ettinger. Public Domain.

-- notes:
-- the entry point of a program is always a single form, a 'do' macro that then
-- evaluates the given forms of the program in order. the 'do' is inserted
-- automatically.
-- a 'function' is a lua function. these are black boxes.
-- an 'sexp' is a table-as-list where the first element is a function or fn called on
--    the rest of the elements in the list. any inner sexps are evaluated first.
-- an 'fn' is a table with at least one numerical arity given as a key, with either
--    a function or sexp as its value. it may also have other keys that are not numbers,
--    containing information such as whether or not the fn is a macro.
-- a lilikoi program is transpiled to a 'codeseq' consisting of lua numbers, booleans,
--    strings (always with one control character prepended to show if it is an
--    identifier with '\1', real string with '\2', keyword with '\5', or quoted
--    identfier with '\6'), or tables containing sub-programs.
-- macros treat identifiers passed to them as quoted identfiers. a macro returns a
--    codeseq, which when the macro is called, is evaluated immediately.


local seed = {}

local glue = require'glue'
glue.luapath(glue.bin)
glue.cpath(glue.bin)
glue.cpath(glue.bin .. "/bin")
glue.cpath(glue.bin .. "/bin/linux32")
glue.cpath(glue.bin .. "/bin/linux32/clib")
glue.cpath(glue.bin .. "/bin/linux64")
glue.cpath(glue.bin .. "/bin/linux64/clib")
glue.cpath(glue.bin .. "/bin/osx32")
glue.cpath(glue.bin .. "/bin/osx32/clib")
glue.cpath(glue.bin .. "/bin/osx64")
glue.cpath(glue.bin .. "/bin/osx64/clib")
glue.cpath(glue.bin .. "/bin/mingw64")
glue.cpath(glue.bin .. "/bin/mingw64/clib")
glue.cpath(glue.bin .. "/bin/mingw32")
glue.cpath(glue.bin .. "/bin/mingw32/clib")

local pp = require'pp'
local fp = require'fun'

local uit = fp.uit
seed.__module_name = 'seed'
seed.__scopes = {{}, {}}
seed.__module = {}
seed.__macros = {defmacro='seed'}

seed.gensym_counter = 13579

seed.reset = function(modname)
  if modname then seed.__module_name = modname end
  seed.__scopes = {{}, {}}
  seed.__module = {}
  seed.__macros = fp.tomap(fp.filter(function(k, v) return v == 'seed' end, seed.__macros))
  seed.gensym_counter = 13579
end

local true_id = {'boolean', true}
local false_id = {'boolean', false}

seed["nil"] = {"nil", nil}

local function directcall(fun, ...)
  local arglen = select('#', ...)
  local f = rawget(fun, arglen) or rawget(fun, -1)
  assert(f, "Function does not have a matching argument list for arguments:\n" .. pp.format({...}))
  if(arglen > 0) then
    return f(...)
  else
    return f()
  end
end
seed._directcall = directcall
local directmeta = {__call = directcall}
seed._directmeta = directmeta

local function make_fn(ops, name, macro)
  ops.name = name
  ops.macro = macro
	return setmetatable(ops, directmeta)
end

seed._make_fn = make_fn
seed.make_fn = {"lua", make_fn}

local function raw_defn(ops, name, macro)
	seed[name] = {"fn", make_fn(ops, name, macro)}
  if macro == true then
    seed.__macros[name] = seed.__module_name
  end
end

seed._raw_defn = raw_defn

local function deflua(ops, name, macro)
	seed[name] = {"lua", make_fn(ops, name, macro)}
end

seed._deflua = deflua


local function defunction(fun, name)
  if type(fun) ~= 'function' then return fun end
  return {'lua', make_fn({[-1]=fun}, name or "anonymous")}
end

--[[
a[1] == 'nil' or
a[1] == 'boolean' or
a[1] == 'number' or
a[1] == 'string' or
a[1] == 'keyword' or
a[1] == 'id' or
a[1] == 'fn' or
a[1] == 'lua' then
--]]

local function strip_args(...)
  local args, len = {}, select('#', ...)
  for i = 1,len do
    local a = select(i, ...)
    if type(a[1]) == 'table' or a[1] == 'list' then
      args[i] = strip_args(unpack(a[2]))
    else
      args[i] = a[2]
    end
  end
  return args
end

local function strip_wrap(fun)
  return function(...)
    return fun(unpack(strip_args(...)))
  end
end

local function val_wrap(v)
  if type(v) == 'table' then
    return {{'any',op='table',done=true}, {v}}
  elseif type(v) == 'function' then
    return {'lua', v}
  else
    return {type(v), v}
  end
end

local function get_item(tab, key)
  assert(type(tab) == 'table', "Attempt to lookup a key:\n" .. pp.format(key) .. "\n  in non-table value:\n" .. pp.format(tab))
  return rawget(tab, key)
end

seed.get = {"lua", get_item}

local function kw_get(kw, tab)
  assert(type(tab) == 'table', "Attempt to lookup keyword:\n" .. pp.format(k) .. "\n  in non-table value:\n" .. pp.format(tab))
  return rawget(tab, kw)
end

seed["kw-get"] = {"lua", kw_get}

local function gensym(prefix)
  seed.gensym_counter = seed.gensym_counter + 1
  prefix = type(prefix) == 'table' and prefix[2] or "GS"
  return {'id', prefix .. "_-" .. seed.gensym_counter}
end

raw_defn({[0]=gensym,[1]=gensym}, 'gensym', true)

local function auto_gensym(pre)
  local prefix = pre[2]
  for revi=#seed.__scopes, 1, -1 do
    if seed.__scopes[revi]['\1auto_gensyms'] then
      if seed.__scopes[revi]['\1auto_gensyms'][prefix] then
        return seed.__scopes[revi]['\1auto_gensyms'][prefix]
      end
    end
  end
  local slen = #seed.__scopes
  if seed.__scopes[slen]['\1auto_gensyms'] then
    seed.__scopes[slen]['\1auto_gensyms'][prefix] = gensym(pre)
    return seed.__scopes[slen]['\1auto_gensyms'][prefix]
  else
    seed.__scopes[slen]['\1auto_gensyms'] = {[prefix]=gensym(pre)}
    return seed.__scopes[slen]['\1auto_gensyms'][prefix]
  end
end

raw_defn({[1]=auto_gensym}, "auto-gensym", true)

local function lookup(t)
  local name, max_scope, special_default = t[2], t[3], t.default
  if name == 'seed' then return seed end
  
  if seed.__scopes[#seed.__scopes][name] ~= nil then
    local ret = seed.__scopes[#seed.__scopes][name]
    if type(ret) == 'table' then return ret end
    return {'lua',ret}
  else
    for revi=(max_scope or #seed.__scopes - 1), 1, -1 do
      if seed.__scopes[revi][name] ~= nil then
        local ret = seed.__scopes[revi][name]
        if type(ret) == 'table' then return ret end
        return {'lua',ret}
      end
    end
  end
  
	if seed.__module and seed.__module[name] ~= nil then
    local ret = seed.__module[name]
    if type(ret) == 'table' then return ret end
    return {'lua',ret}
  end
  
  if seed[name] ~= nil then
    local ret = seed[name]
    if type(ret) == 'table' then return ret end
    return {'lua',ret}
  end
  
  if special_default then
    seed.__scopes[#seed.__scopes][name] = special_default
    return special_default
  end
  
	return nil
end

raw_defn({[1]=lookup}, 'lookup')

local function _import(newname, oldname)
  oldname = oldname[2] or newname[2]
  seed[newname[2]] = require(oldname)
  return seed[newname[2]]
end

raw_defn({[1]=_import, [2]=_import}, 'import')

local function is_identifier(t)
  if type(t) == 'table' and t[1] == 'id' then
    return true_id
  end
  return false_id
end

local function is_quoted(t)
  if type(t) == 'table' and t[1] == 'quote' then
    return true_id
  end
  return false_id
end


local function is_keyword(t)
  if type(t) == 'table' and t[1] == 'keyword' then
    return true_id
  end
  return false_id
end

local function is_string(t)
  if type(t) == 'string' or (type(t) == 'table' and t[1] == 'string') then
    return true_id
  end
  return false_id
end

seed["identifier?"] = {"fn", is_identifier}
seed["quoted?"] = {"fn", is_quoted}
seed["keyword?"] = {"fn", is_keyword}
seed["string?"] = {"fn", is_string}

local function identify(t)
  if t[1] == 'id' then
    return lookup(t)
  end
  return nil
end

local function quote_id(t)
  if t[1] == 'id' then
    return {'quote', t[2]}
  end
  return t
end

local function dequote(t, max_scope)
  if t[1] == 'quote' then
    return {'id', t[2], max_scope}
  end
  return t
end

seed.identify = {"fn", identify}
seed["quote-id"] = {"fn", quote_id}
seed.dequote = {"fn", dequote}

local function macroexpand1(codeseq)
  local newcode, expanded = {}, 0
  for i=1,#codeseq do
    local term = codeseq[i]
    if type(term[2]) == 'table' then
      if type(term[2][1]) == 'table' and (term[1] == 'macro' or
          (term[2][1][1] == 'id' and seed.__macros[term[2][1][2]])) then
        local op = table.remove(term[2], 1)
        local args, counter = macroexpand1(term[2])
        newcode[i] = identify(op)[2](unpack(args))
        expanded = expanded + 1 + counter
      else
        local inner, counter = macroexpand1(term[2])
        newcode[i] = {term[1], inner}
        expanded = expanded + counter
      end
    else
      newcode[i] = fp.clone(term)
    end
  end
  return newcode, expanded
end

seed["macroexpand-1"] = {"fn", macroexpand1, true}

local function macroexpand(codeseq)
  local code, expansions = macroexpand1(codeseq)
  while expansions > 0 do
    code, expansions = macroexpand1(code)
  end
  return code
end

seed._macroexpand = macroexpand
seed.macroexpand = {"fn", macroexpand, true}

-- reads a sequence of generated sexps and data from
-- the specified codeseq, which should already have a program,
-- and steps through it until it has read a single form.
local function eval(codeseq, quotelevel, kind, codify)
  if type(codeseq) ~= 'table' then return codeseq end
  quotelevel = quotelevel or 0
  if #codeseq == 0 then return {} end
  if kind == 'syntax' then quotelevel = quotelevel + 1 end
  local arglist = {}
  local term
  if quotelevel > 0 then
    for i=1, #codeseq do
      local ql = quotelevel
      term = codeseq[i]
      local tv = term[2]
      if kind == "unquote" then
        ql = ql - 1
      end
      if type(tv) == 'table' then 
        arglist[i] = eval(tv, ql, term[1], codify)
      else
        if ql == 0 then
          arglist[i] = (identify(term)) or term
        else
          arglist[i] = term
        end
      end
    end
    if kind == "unquote" then
      return (unpack(arglist))
    elseif kind == "syntax" then
      return (unpack(arglist))
    else
      return {kind,arglist}
    end
  else
    local prime, op, nm
    local special_open = true
    if type(kind) == 'table' then
      if kind.done then return {kind, codeseq} end
      op, nm = lookup({'id', kind.op}), kind.op
      special_open = false
    else
      prime = codeseq[1]
      if type(prime[2]) == 'table' then
        op = eval(prime[2], quotelevel, prime[1], codify)
        nm = op.name
      else
        if codify then
          op, nm = identify(dequote(prime, codify))
        else 
          op, nm = identify(prime)
        end
        op = defunction(op, nm)
      end
    end
    if special_open and op and is_keyword(op)[2] then
      term = codeseq[2]
      local tv = term[2]
      if type(tv) == 'table' then 
        arglist[1] = eval(tv, quotelevel, term[1], codify)
      else
        arglist[1], nm = identify(term)
        if arglist[1] == nil then arglist[1] = term end
      end
      return kw_get(op, arglist[1][2])
    end
    
    if kind == 'macro' or op[2].macro then
      for i=(special_open and 2 or 1), #codeseq do
        term = codeseq[i]
        local tv = term[2]
        if type(tv) == 'table' then 
          arglist[i-(special_open and 1 or 0)] = eval(tv, quotelevel + 1, term[1], codify)
        else
          arglist[i-(special_open and 1 or 0)] = quote_id(term)
        end
      end
      if op[1] == 'lua' then
        return val_wrap(op[2](unpack(strip_args(unpack(arglist)))))
      end
      return op[2](unpack(arglist))
    end
    
    for i=(special_open and 2 or 1), #codeseq do
      term = codeseq[i]
      local tv = term[2]
      local idx = i-(special_open and 1 or 0)
      if type(tv) == 'table' then 
        arglist[idx] = eval(tv, quotelevel, term[1], codify)
      else
        if codify then
          arglist[idx], nm = identify(dequote(term, codify))
        else
          arglist[idx], nm = identify(term)
        end
        if arglist[idx] == nil then arglist[idx] = term end
      end
    end
    if op[1] == 'lua' then
      return val_wrap(op[2](unpack(strip_args(unpack(arglist)))))
    end
    return op[2](unpack(arglist))
  end
end

seed.eval = {'lua',eval}

-- a way to run a program that was not generated by the transpiler,
-- ensuring the body of the program is wrapped in a call to do so
-- all forms in the program are evaluated in order and the last
-- result is returned. the optional argument 'quoting' allows
-- the evaluation to be carried out at as if it is inside a macro
-- (for positive values), or if the argument is negative, to unquote any
-- identifiers encountered unless something else changed the quote level.
local function minirun(program, quoting, kind, codify)
  return eval({{'id','do'},unpack(program)}, quoting, kind, codify)
end
seed.minirun = {"lua", minirun}
-- the entry point for a program. Clears any possible lingering state,
-- then returns any number of args (ideally 1, if the program
-- completed with one return value) based on evaluating a codeseq.
-- returns a bare value.
function seed.run(program)
  seed.reset()
  return unpack(strip_args(eval(program)))
end

-- a re-entry point for a program. Keeps any possible lingering state,
-- then returns any number of args (ideally 1, if the program
-- completed with one return value) based on evaluating a codeseq.
-- returns a form with additional information, not a bare value.
function seed.run_in(program)
  return eval(program)
end

local function _do(...)
  local res = nil
  for i=1, select('#', ...) do
    res = (select(i, ...))
  end
  return res
end

raw_defn({[-1]=_do}, "do")

local function seq(t)
  return uit.iter(t, nil, nil)
end

deflua({[1]=seq}, "seq")

local function def(name, val)
  local value = minirun({val}, 0, "call", #seed.__scopes)
  if seed.__module then
    seed.__module[name[2]] = value
  else
    if seed[name[2]] ~= nil then error("Cannot define a new toplevel value for name because it already exists:\n" .. pp.format(name[2] ..
          "\nExisting value for this name is:\n" .. pp.format(seed[name[2]])))
    end
    seed[name[2]] = value
  end
  return {'id', name[2]}
end

raw_defn({[2]=def}, "def", "special")

local function access(top, ...)
  local res = lookup({'id',top})
  for i=1, select('#', ...) do
    res = res[select(i, ...)]
    if res == nil then return res end
  end
  return res
end

deflua({[-1]=access}, "access", "special")

local function list(...)
  return {"list", {...}}
end

raw_defn({[-1]=list}, "list")

local function vector(...)
  return {{"number",op="vector",done=true}, {...}}
end

raw_defn({[-1]=vector}, "vector")

local function _table(t)
  return {{"any",op="table",done=true}, {t}}
end

raw_defn({[1]=_table}, "table")

local function count(t)
	if type(t) == 'table' then
		return #t, true
  else
    return nil, false
	end
end

deflua({[1]=count}, "count")

local function call(name, ...)
  local f = lookup({'id', name})
  if f and f[2] then
    return f[2](...)
  else
    return nil
  end
end

raw_defn({[-1]=call}, "call")

deflua({[2]=function(f, coll) return uit.reduce(f, seq(coll)) end, 
     [3]=function(f, start, coll) return uit.foldl(f, start, seq(coll)) end }, "reduce")

deflua({[2]=function(f, coll) return uit.reductions(f, seq(coll)) end,
     [3]=function(f, start, coll) return uit.scan(f, start, seq(coll)) end}, "reductions")

local function quote(term)
  return term
end

raw_defn({[1]=quote}, "quote", "special")

-- (fn [[a [b c]]] ...)
-- [a [b c]]
local function destructure(ks, coll)
  if type(ks[2]) ~= 'table' then
    seed.__scopes[#seed.__scopes][ks[2]] = coll
    return nil
  end
  for i=1, #ks[2] do
    destructure(ks[2][i], coll[2][i])
  end
end

local function bind_once_(k, v)
  v = minirun({v}, 0, "call", #seed.__scopes)
  if type(k[2]) ~= 'table' then
    seed.__scopes[#seed.__scopes][k[2]] = v
    return v
  end
  destructure(k, v)
  return v
end

-- meant to be called at runtime, not on quoted forms
local function bind_all_(argnames, argvals, arity)
  if arity == -1 then
    local used_names = uit.take_while(function(id) return id[2] ~= '&' end, seq(argnames))
    local used_len = uit.length(used_names)
    uit.each(bind_once_, uit.zip(used_names, uit.take_n(used_len, seq(argvals))))
    if argnames[used_len + 2] then
      bind_once_(argnames[used_len + 2], {{"number",op='vector',done=true}, uit.totable(uit.drop_n(used_len, seq(argvals)))})
    end
    return nil
  end
  uit.each(bind_once_, uit.zip(seq(argnames), seq(argvals)))
end

local function bind_all_alternating_(args)
  local arglist = args[2]
  for i=1,#arglist-1,2 do
    bind_once_(arglist[i],arglist[i+1])
  end
end

local function fn_(name, arglist, argnum, my_scope, is_macro, ...)
  local is_variadic = false
  if argnum > 1 then
    local s2l = arglist[argnum - 1]
    if type(s2l) == 'table' and s2l[1] == 'id' and s2l[2] == '&' then
      is_variadic = true
    end
  end
  local arity = is_variadic and -1 or argnum
  local ft = {...}
  return make_fn({[arity]=
      function(...)
        seed.__scopes[#seed.__scopes + 1] = {}
        bind_all_(arglist, {...}, arity)
        local result = minirun(ft, 0, "call", my_scope)
        table.remove(seed.__scopes)
        return result
      end}, name, is_macro)
end
local function choose_fn_(name, my_scope, is_macro, ...)
  local optab = {}
  for i=1, select('#', ...) do
    local current = (select(i, ...))[2]
    local arglist = current[1][2]
    local argnum = #arglist
    
    local is_variadic = false
    if argnum > 1 then
      local s2l = arglist[argnum - 1]
      if type(s2l) == 'table' and s2l[1] == 'id' and s2l[2] == '&' then
        is_variadic = true
      end
    end
    local arity = is_variadic and -1 or argnum
--    table.remove(current, 1)
    local ft = {current[2]}
    optab[arity]=
    function(...)
      seed.__scopes[#seed.__scopes + 1] = {}
      bind_all_(arglist, {...}, arity)
      local result = minirun(ft, 0, "call", my_scope)
      table.remove(seed.__scopes)
      return result
    end
  end
  return make_fn(optab, name, is_macro)
end

local function fn(...)
  local nm = 'anonymous_fn'
  local arglist = select(1, ...)
  if type(arglist[1]) == 'table' and arglist[1].op == 'vector' then
    return {"fn", fn_(nm, arglist[2], #arglist[2], #seed.__scopes, nil, select(2, ...))}
  elseif arglist[1] == 'quote' then
    nm = arglist[2]
    arglist = select(2, ...)
    if type(arglist[1]) == 'table' and arglist[1].op == 'vector' then
      return {"fn", fn_(nm, arglist[2], #arglist[2], #seed.__scopes, nil, select(3, ...))}
    elseif arglist[1] == 'list' then
      -- multiple arglists and bodies
      return {"fn", choose_fn_(nm, #seed.__scopes, nil, select(2, ...))}
    end
  elseif arglist[1] == 'list' then
    -- multiple arglists and bodies
    return {"fn", choose_fn_(nm, #seed.__scopes, nil, ...)}
  end
  error("Declaring this fn failed because it does not match the expected structure:\n" .. pp.format({...}))
end

raw_defn({[-1]=fn}, "fn", "special")


local function let(arglist, ...)
  local argnum = #arglist / 2
  if argnum ~= math.floor(argnum) then
    error("let binding must have an even number of forms; binding received was:\n" .. pp.format(arglist))
  end
  
  local body = {...}
  local my_scope = #seed.__scopes
  seed.__scopes[#seed.__scopes + 1] = {}
  bind_all_alternating_(arglist)
  local result = minirun(body, 0, "call", my_scope)
  table.remove(seed.__scopes)
  return result
end

raw_defn({[-1]=let}, "let", "special")


local function defn(name, ...)
  return {'list',{{'id', 'def'},{'id',name[2]},
      {'list',{{'id','fn'}, {'id',name[2]}, ...}}}}
end

raw_defn({[-1]=defn}, "defn", true)

local function defmacro_helper(name, value)
  if seed.__module then
    seed.__module[name[2]] = value
  else
    if seed[name[2]] ~= nil then error("Cannot define a new toplevel value for name because it already exists:\n" .. pp.format(name[2] ..
          "\nExisting value for this name is:\n" .. pp.format(seed[name[2]])))
    end
    seed[name[2]] = value
  end
  return {'id', name[2]}
end

local function defmacro(name, ...)
  local arglist = select(1, ...)
  if type(arglist[1]) == 'table' and arglist[1].op == 'vector' then
    seed.__macros[name[2]] = seed.__module_name
    return defmacro_helper (name, {"fn", fn_(name, arglist[2], #arglist[2], #seed.__scopes, true, select(2, ...))})
  elseif arglist[1] == 'list' then
    seed.__macros[name[2]] = seed.__module_name
    -- multiple arglists and bodies
    return defmacro_helper(name, {"fn", choose_fn_(name, #seed.__scopes, true, ...)})
  end
  error("Declaring this macro failed because it does not match the expected structure:\n" .. pp.format({...}))
end

raw_defn({[-1]=defmacro}, "defmacro", "special")

seed.math = math
seed.io = io
seed.file = file
seed.os = os
seed.string = string
seed.print = print
seed.str = pp.format
seed.fp = fun

return glue.autoload(seed,
{
   ["+"] = 'lilikoi.operators',
   ["-"] = 'lilikoi.operators',
   ["*"] = 'lilikoi.operators',
   ["/"] = 'lilikoi.operators',
   ["/|"] = 'lilikoi.operators',
   ["/_"] = 'lilikoi.operators',
   ["pow"] = 'lilikoi.operators',
   ["%"] = 'lilikoi.operators',
   ["="] = 'lilikoi.operators',
   ["!="] = 'lilikoi.operators',
   ["<"] = 'lilikoi.operators',
   ["<="] = 'lilikoi.operators',
   [">"] = 'lilikoi.operators',
   [">="] = 'lilikoi.operators',
   concat = 'lilikoi.operators',
   len = 'lilikoi.operators',
   transpile = 'lilikoi.transpiler',
   execute = 'lilikoi.transpiler',
   execute_in = 'lilikoi.transpiler',
})