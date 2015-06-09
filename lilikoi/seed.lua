
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

seed.__scopes = {{}, {}}
seed.__namespace = nil

local true_id = {'boolean', true}
local false_id = {'boolean', false}

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
    if type(a[1]) == 'table' then
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
    return {{'any'}, v}
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

local function lookup(t)
  local name = t[2]
	for revi=#seed.__scopes, 1, -1 do
		if seed.__scopes[revi][name] ~= nil then
			local ret = seed.__scopes[revi][name]
      if type(ret) == 'table' then return ret end
      return {'lua',ret}
		end
	end
  
	if seed.__namespace then
		if seed.__namespace[name] ~= nil then
      local ret = seed.__namespace[name]
      if type(ret) == 'table' then return ret end
      return {'lua',ret}
    end
  end 
  
  if seed[name] ~= nil then
    local ret = seed[name]
    if type(ret) == 'table' then return ret end
    return {'lua',ret}
  end
	return nil
end

seed.lookup = {"fn", lookup}

local function _import(newname, oldname)
  oldname = oldname or newname
  seed[newname] = {{'any',op='vector',done=true}, require(oldname)}
  return seed[newname]
end

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
    return {'quote', t[2], t[3], t[4]}
  end
  return t
end

local function dequote(t)
  if t == 'quote' then
    return {'id', t[2], t[3], t[4]}
  end
  return t
end

seed.identify = {"fn", identify}
seed["quote-id"] = {"fn", quote_id}
seed.dequote = {"fn", dequote}

-- reads a sequence of generated sexps and data from
-- the specified codeseq, which should already have a program,
-- and steps through it until it has read a single form.
local function eval(codeseq, quotelevel, kind)
  if type(codeseq) ~= 'table' then return codeseq end
  quotelevel = quotelevel or 0
  local codify = kind == 'call'
  if #codeseq == 0 then return {} end
  
  local arglist = {}
  local term
  if quotelevel > 0 then
    for i=1, #codeseq do
      local ql = quotelevel
      term = codeseq[i]
      local tv = term[2]
      if type(arglist[i-1]) == 'table' and arglist[i-1][1] == 'quote' and
          arglist[i-1][2] == "unquote" then
        ql = ql - 1
      end
      if type(tv) == 'table' then 
        arglist[i] = eval(tv, ql, term[1])
      else
        if ql == 0 then
          arglist[i] = (identify(term)) or term
        else
          arglist[i] = quote_id(term)
        end
      end
    end
    return arglist
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
        op = eval(prime[2], quotelevel, prime[1])
        nm = op.name
      else
        if codify then
          op, nm = identify(dequote(prime))
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
        arglist[1] = eval(tv, quotelevel, term[1])
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
          arglist[i-(special_open and 1 or 0)] = eval(tv, quotelevel + 1, term[1])
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
        arglist[idx] = eval(tv, quotelevel, term[1])
      else
        if codify then
          arglist[idx], nm = identify(dequote(term))
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
local function minirun(program, quoting)
  return eval({{'id','do'},program}, quoting)
end
seed.minirun = {"lua", minirun}
-- the entry point for a program. Clears any possible lingering state,
-- then returns any number of args (ideally 1, if the program
-- completed with one return value) based on evaluating a codeseq.
-- returns a bare value.
function seed.run(program)
	seed.__scopes = {{},{}}
	seed.__namespace = nil
  return unpack(strip_args(eval(program)))
end

-- a re-entry point for a program. Keeps any possible lingering state,
-- then returns any number of args (ideally 1, if the program
-- completed with one return value) based on evaluating a codeseq.
-- returns a form with additional information, not a bare value.
function seed.run_in(program)
  return eval(program)
end

seed["nil"] = {"nil", nil}

local function defn(ops, name, macro)
	seed[name] = {"fn", make_fn(ops, name, macro)}
end

seed._defn = defn

local function deflua(ops, name, macro)
	seed[name] = {"lua", make_fn(ops, name, macro)}
end

seed._deflua = deflua

local function count(t)
	if type(t) == 'table' then
		return #t, true
	elseif not not t then
		return 1, false
  else
    return 0, false
	end
end

defn({[1]=strip_wrap(count)}, "count")

local function _do(...)
  local res = nil
  for i=1, select('#', ...) do
    res = (select(i, ...))
  end
  return res
end

defn({[-1]=_do}, "do")

local function seq(t)
  return uit.iter(t, nil, nil)
end

defn({[1]=strip_wrap(seq)}, "seq")


local function access(top, ...)
  local res = lookup({'id',top})
  for i=1, select('#', ...) do
    res = res[select(i, ...)]
    if res == nil then return res end
  end
  return res
end

deflua({[-1]=access}, "access", true)

local function vector(...)
  return {{"number",op="vector",done=true}, {...}}
end

defn({[-1]=vector}, "vector")

deflua({[2]=function(f, coll) return uit.reduce(f, seq(coll)) end, 
     [3]=function(f, start, coll) return uit.foldl(f, start, seq(coll)) end }, "reduce")

deflua({[2]=function(f, coll) return uit.reductions(f, seq(coll)) end,
     [3]=function(f, start, coll) return uit.scan(f, start, seq(coll)) end}, "reductions")

local function quote(term)
  return term
end

defn({[1]=quote}, "quote", "quote")
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
      bind_once_(argnames[used_len + 2], {'vector', uit.totable(uit.drop_n(used_len, seq(argvals)))})
    end
  end
  uit.each(bind_once_, uit.zip(seq(argnames), seq(argvals)))
end

local function fn_(name, arglist, argnum, ...)
  local is_variadic = false
  if argnum > 1 then
    local s2l = arglist[argnum - 1]
    if type(s2l) == 'table' and s2l[1] == 'quote' and s2l[2] == '&' then
      is_variadic = true
    end
  end
  local arity = is_variadic and -1 or argnum
  local ft = {{'id','do'}, ...}
  return make_fn({[arity]=
      function(...)
        seed.__scopes[#seed.__scopes + 1] = {}
        bind_all_(arglist, {...}, arity)
        return minirun(ft, 0, "call")
      end}, name)
end
local function choose_fn_(name, ...)
  local optab = {}
  for i=1, select('#', ...) do
    local current = (select(i, ...))[2]
    local arglist = current[1][2]
    local argnum = #arglist
    
    local is_variadic = false
    if argnum > 1 then
      local s2l = arglist[argnum - 1]
      if type(s2l) == 'table' and s2l[1] == 'quote' and s2l[2] == '&' then
        is_variadic = true
      end
    end
    local arity = is_variadic and -1 or argnum
    table.remove(current, 1)
    local ft = current
    optab[arity]=
    function(...)
      seed.__scopes[#seed.__scopes + 1] = {}
      bind_all_(arglist, {...}, arity)
      return minirun(ft, 0, "call")
    end
  end
  return make_fn(optab, name)
end

local function fn(...)
  local nm = 'anonymous_fn'
  local arglist = select(1, ...)
  if arglist[1] == 'vector' then
    return {"fn", fn_(nm, arglist[2], #arglist[2], select(2, ...))}
  elseif arglist[1] == 'string' then
    nm = arglist[2]
    arglist = select(2, ...)
    if arglist[1] == 'vector' then
      return {"fn", fn_(nm, arglist[2], #arglist[2], select(3, ...))}
    elseif type(arglist[1] == 'list') then
      -- multiple arglists and bodies
      return {"fn", choose_fn_(nm, select(2, ...))}
    end
  elseif type(arglist[1] == 'list') then
    -- multiple arglists and bodies
    return {"fn", choose_fn_(nm, ...)}
  end
  error("Declaring this fn failed because it does not match the expected structure:\n" .. pp.format({...}))
end

defn({[-1]=fn}, "fn", true)

--[[
local function lambda(args)
end

def({[-1]=lambda}, "lambda", true)
--]]

--[==[
local function _portion(t, starter, stopper, halt)
	if type(t) == 'table' then
		local stop = stopper or #t
    local start = starter or 1
    local part = {}
    for i=start, #t do
      local v = t[i]
      if i > stop or v == halt then
        return part
      else
        part[#part + 1] = v
      end
    end
    return part
	else
		return t
	end
end

local function _read_portion(t, starter, stopper, halt)
	if type(t) == 'table' then
		local stop = stopper or #t
    local start = starter or 1
    local part = {}
    for i=start,#t do
      local v = t[i]
      if i > stop then
        return part, #part
      elseif v == halt then
        return part, #part + 1
      else
        part[#part + 1] = v
      end
    end
    return part, #part
	else
		return t, 1
	end
end

local function _defunctor(f)
	if type(f) == 'table' then
		if f["\6op"] then return f["\6op"]
		elseif f["\6f"] and f["\6f"]["\6op"] and f["\6g"] and #f["\6g"] > 0 then
			return function(...)
				return f["\6f"]["\6op"](unpack(f["\6g"]), ...)
			end
		elseif f["\6f"] and f["\6f"]["\6op"] then
			return function(...)
				return f["\6f"]["\6op"](...)
			end
		end
	end
	return f
end

local function _call(fun, ...)
	return fun(...)
end

function seed._map(f, coll, offset)
	--local f = _defunctor(fun)
	local coll2 = {}
	offset = offset or 0
	for i,v in ipairs(coll) do
		if i > offset then
      local res = f(v)
      if type(res) == 'table' and res["\6f"] then
        res["\6q"] = true
      end
			rawset(coll2, #coll2 + 1, res)
		end
	end
	return coll2
end

local function _apply(f, t)
	--local f = _defunctor(fun)
	return f(unpack(t))
end

local function _pack(...)
	return {...}
end

local function _supply(ar)
  local args = seed._map(seed._unquote, ar)
  local ff = table.remove(args, 1)
	local f
  if seed._is_identifier(ff) then
    f = (seed._identify(ff))
  elseif type(ff) == 'table' then
    f = ff
  elseif type(ff) == 'function' then
      return seed._partial(seed._functor(ff, -1, "anonymous-partial"), args, true)
  end
	if type(f) == 'table' then
    if f["\6g"] then
      glue.extend(f["\6g"], args)
      f["\6q"] = true
      return f
    elseif f["\6op"] then
      return seed._partial(f, args, true)
    end
  elseif type(f) == 'function' then
      return seed._partial(seed._functor(f, -1, "anonymous-partial"), args, true)
  end
  return nil
end

local function _reduce(f, coll, initial)
	--local f = _defunctor(fun)
	local ret = initial or coll[1]
	for i,v in ipairs(coll) do
		if initial or i > 1 then
			ret = f(ret, v)
		end
	end
	return ret
end

local function _multiget(t, i)
	local ret = {}
	for c,v in ipairs(t) do
		rawset(ret, c, rawget(v, i))
	end
	return ret
end

local function _vmap(f, ...)
	if (...) == nil then return nil end
	--local f = _defunctor(fun)
	local ret = {}
	local colls = {...}
	local minlength = math.min(unpack(seed._map(_length, colls)))
	for i,v in ipairs(rawget(colls, 1)) do
		if i <= minlength then
			rawset(ret, #ret + 1, f(unpack(_multiget(colls, i))))
		else
			return ret
		end
	end
	return ret
end

seed.__def(seed._clean, 1, "clean")
seed.__def(seed._unquote, 1, "unquote")
seed.__def(_call, -1, "call")
seed.__def(_call, -1, "@")
seed.__def(seed._map, 2, "map")
seed.__def(_portion, 3, "portion")
seed.__def(_portion, 4, "until")
seed.__def(seed._map, 3, "off-map")
seed.__def(_vmap, -1, "vmap")

seed.__def(_apply, 2, "apply")
seed.__def(_supply, -1, "supply", nil, true)
seed.__def(unpack, 1, "unpack")
seed.__def(_pack, -1, "pack")
seed.__def(_reduce, 2, "reduce")
seed.__def(_reduce, 3, "reduce-with")

local function _lucompose(ftable)
	--local ftable = seed._map(_defunctor, fs)
	return function(...)
		local ff = table.remove(ftable, 1)
		local ret = {ff(...)}
		for i,v in ipairs(ftable) do
			ret = v(unpack(ret))
		end
		return ret
	end
end

local function _compose(...)
	return seed._functor(_lucompose({...}), -1, "anonymous")
end

seed.__def(_compose, -1, "compose")

function seed._close()
  return '\5%stop'
end

seed.stop = "\5%"


local function _rawdefine(name, val)
	if type(val) == 'table' and val["\6q"] then val["\6q"] = nil end
	local scp = seed.__scopes[2]
	if seed.__namespace and scp[seed.__namespace] then scp = scp[seed.__namespace] end
	for s in glue.gsplit(seed.munge(seed._clean(name)), ".", 1, true) do
		local munged = seed.munge(s)
		local n, nm = seed._lookup(munged) 
		if type(n) == 'table' and not (n["\6f"] or n["\6op"]) then
			scp = n
		else
			scp[munged] = val
			return nil
		end
	end
	scp = val
	return nil
end
local function define(args)
	local name = table.remove(args, 1)
	local val = seed._minirun(seed._map(seed._unquote, args))[1]
	_rawdefine(name, val)
	return nil
--	seed.__scopes[#seed.__scopes][seed.munge(seed._clean(name))] = val
end
seed.__def(define, -1, "def", nil, true)

local function _fn(args)
	local varargs
	local argseq, arg_idx = {}, 2
	if arg_idx > #args then return nil end
	local a = args[arg_idx]
	while a ~= '\6,]' do
		if varargs == nil then
			argseq[arg_idx - 1] = a
		end
		arg_idx = arg_idx + 1
		if arg_idx > #args then return nil end
		if a == "\6,&&&" then
			varargs = -1
		end
		a = args[arg_idx]
	end
	local my_order = {}
	for i,v in ipairs(argseq) do
		my_order[i] = seed._clean(v)
	end
	return seed._functor(
	function(...)
		local ar = {...}
		seed.__scopes[#seed.__scopes + 1] = {}
		seed.__scopes[#seed.__scopes]["&&&"] = {}
		local i2 = 1
		for i,a in ipairs(ar) do
			if my_order[i2] == "&&&" then
				table.insert(seed.__scopes[#seed.__scopes]["&&&"], a)
			else
				seed.__scopes[#seed.__scopes][my_order[i2]] = a
				i2 = i + 1
			end
		end
		local ret = seed._minirun(seed._map(seed._unquote, args, arg_idx))
		table.remove(seed.__scopes)
		return unpack(ret)
	end,
	varargs or #my_order,
	"anonymous")
end


local function _defn(args)
	local name = table.remove(args, 1)
	local val = _fn(args)
	rawset(val, "\6name", seed.munge(seed._clean(name)))
	_rawdefine(name, val)
	return nil
end

local function _defgroup(args)
	local opener = table.remove(args, 1)
	local closer = table.remove(args, 1)
  local cleanopener, cleancloser = seed.munge(seed._clean(opener)), seed.munge(seed._clean(closer))
  _rawdefine(closer, '\5%stop') -- seed._functor(seed._close, 0, cleancloser)
  _rawdefine(opener, seed._functor(function(...)
    local ar = {...}
		seed.__scopes[#seed.__scopes + 1] = {}
		seed.__scopes[#seed.__scopes]["&&&"] = {}
		for i,a in ipairs(ar) do
			table.insert(seed.__scopes[#seed.__scopes]["&&&"], a)
    end
		local ret = seed._minirun(seed._map(seed._unquote, args))
		table.remove(seed.__scopes)
		return unpack(ret)
	end,
	-1,
	cleanopener,
  cleancloser,
  nil))
	return nil
end


local function _defmacro(args)
	local name = table.remove(args, 1)
	local varargs
	local argseq, arg_idx = {}, 2
	if arg_idx > #args then return nil end
	local a = args[arg_idx]
	while a ~= '\6,]' do
		if varargs == nil then
			argseq[arg_idx - 1] = a
		end
		arg_idx = arg_idx + 1
		if arg_idx > #args then return nil end
		if a == "\6,&&&" then
			varargs = -1
		end
		a = args[arg_idx]
	end
	local my_order = {}
	for i,v in ipairs(argseq) do
		my_order[i] = seed._clean(v)
	end
	_rawdefine(name, seed._functor(
	(function(ar)
--		local ar = {...}
		seed.__scopes[#seed.__scopes + 1] = {}
		seed.__scopes[#seed.__scopes]["&&&"] = {}
		local i2 = 1
    if #ar > 0 then
      local i, ma = 0, nil
      while i < #ar do
        i = i + 1
        ma = ar[i]
        -- scope at element 'arg1' has value '\6,val1' for passed identifiers
        -- so when anything tries to lookup arg1, it gets a quoted identifier.
        if ma == "\6,~" then
          local part, plen = _read_portion(ar, i + 1, #ar, "\6,~")
          i = i + plen
          local inner = seed._minirun(seed._map(seed._unquote, part))
          for ii = 1, #inner do
            local mm = inner[ii]
            if my_order[i2] then
              seed.__scopes[#seed.__scopes][my_order[i2]] = mm
              i2 = i2 + 1
            else
              table.insert(seed.__scopes[#seed.__scopes]["&&&"], mm)
            end
          end
        elseif my_order[i2] == nil or my_order[i2] == "&&&" then
          table.insert(seed.__scopes[#seed.__scopes]["&&&"], ma)
        else
          seed.__scopes[#seed.__scopes][my_order[i2]] = ma
          i2 = i2 + 1
        end
      end
    end
    
		local ret = seed._minirun(seed._map(seed._unquote, args, arg_idx))
		table.remove(seed.__scopes)
		return unpack(ret)
	end),
	-1, -- macros must always be varargs because they won't understand groups otherwise.
	seed.munge(seed._clean(name)),
	false,
	true
	))
	return nil
end

local function _ns(name)
	local s = seed.munge(seed._clean(name))
	local scp = seed.__scopes[2]
	if scp[s] == nil then scp[s] = {} end
	seed.__namespace = s
	return nil
--	if seed.__namespace and scp[seed.__namespace] then scp = scp[seed.__namespace] end
	
	--[[
	local s2
	for s in glue.gsplit(seed.munge(seed._clean(name)), ".", 1, true) do
		s2 = seed.munge(s)
		local n = scp[s2]
		if type(n) == 'table' then
			if n["\6f"] or n["\6op"] then
				scp[s2] = {}
			end
			seed.__namespace = scp[s2]
		else
			scp[s2] = {}
			seed.__namespace = scp[s2]
		end
	end--]]
end

seed.__def(_fn, -1, "fn", nil, true)
seed.__def(_defn, -1, "defn", nil, true)
seed.__def(_defgroup, -1, "defgroup", nil, true)
seed.__def(_defmacro, -1, "defmacro", nil, true)
seed.__def(_ns, -1, "ns", nil, true)

function seed.__sequence(...)
	return {...}
end

local function _basic_get(t, idxs)
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
	return nil
end

function seed.format(val)
	local tp = type(val)
	if val == nil then
		return '_'
	elseif seed._is_identifier(val) then
		return string.sub(val, 3)
	elseif seed._is_quoted(val) then
		return "`" .. string.sub(val, 3)
	elseif seed._is_keyword(val) then
		return string.sub(val, 2)
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

seed[")"] = '\5%stop'
--seed.__def(seed._close, 0, ")")

seed.__def(seed.__sequence, -1, "[", "]")

seed["]"] = '\5%stop'

seed.__def(_basic_get, 2, "=get")
local function _stringify(v)
	if type(v) == 'string' then
		return v
	else
		return pp.format(v)
	end
end

local function _str(...)
	return table.concat(seed._map(_stringify, {...}))
end

seed.__def(_str, -1, "str")
seed.__def(_stringify, 1, "stringify")
seed.__def(pp.print, -1, "pprint")
--]==]
seed.math = math
seed.io = io
seed.file = file
seed.os = os
seed.string = string
seed.print = print
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