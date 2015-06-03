
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
local fun = require'fun'
seed.__scopes = {{}, {}}
seed.__namespace = nil

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

seed.make_fn = make_fn

local function defunction(fun, name)
  if type(fun) ~= 'function' then return fun end
  return make_fn({[-1]=fun}, name or "anonymous")
end

local function get_item(tab, key)
  assert(type(tab) == 'table', "Attempt to lookup a key:\n" .. pp.format(key) .. "\n  in non-table value:\n" .. pp.format(tab))
  return rawget(tab, key)
end

seed.get = get_item

local function kw_get(kw, tab)
  local k = string.sub(kw, 2)
  assert(type(tab) == 'table', "Attempt to lookup keyword:\n" .. pp.format(k) .. "\n  in non-table value:\n" .. pp.format(tab))
  return rawget(tab, k)
end

seed["kw-get"] = kw_get

local function lookup(name)
	for revi=#seed.__scopes, 1, -1 do
		if seed.__scopes[revi][name] ~= nil then
			return seed.__scopes[revi][name], name
		end
	end
  
	if seed.__namespace then
		if seed.__namespace[name] ~= nil then
      return seed.__namespace[name]
    end
  end 
  
  if seed[name] ~= nil then
    return seed[name]
  end
	return nil, name
end

seed.lookup = lookup

local function is_identifier(name)
  if type(name) == 'string' and #name > 1 then
    return string.sub(name, 1, 1) == "\1"
  end
  return false
end

local function is_quoted(name)
  if type(name) == 'string' and #name > 1 then
    return string.sub(name, 1, 1) == "\6"
  end
  return false
end

local function is_keyword(name)
  if type(name) == 'string' and #name > 1 then
    return string.sub(name, 1, 1) == "\5"
  end
  return false
end

local function is_string(name)
  if type(name) == 'string' and #name > 0 then
    return string.sub(name, 1, 1) == "\2"
  end
  return false
end

seed["identifier?"] = is_identifier
seed["quoted?"] = is_quoted
seed["keyword?"] = is_keyword
seed["string?"] = is_string

local function cleanup(name)
  if type(name) == 'string' and string.match(name, "^[\1\2\5\6]") then
    return string.sub(name, 2)
  end
  return name
end

local function identify(name)
  if is_identifier(name) then
    local nm = string.sub(name, 2)
    return lookup(nm)
  end
  return nil, cleanup(name)
end

local function quote_id(name)
  if is_identifier(name) then
    return "\6" .. string.sub(name, 2)
  end
  return name
end

seed.cleanup = cleanup
seed.identify = identify
seed["quote-id"] = quote_id

local nests = {}
local nestlevel = 0

-- runs a partial function once it has been supplied all needed args.
-- returns the function's result, if it has one, followed by another return
-- "\5", or if it has not been supplied enough args, it returns the function
-- with a larger list of supplied args and no extra return. handles grouping.
function seed.__step(fun, arg, terminal)
	local f, given, partial
	-- in the case that we have been given a functor, not a partial
	if fun["\6op"] then
		f = fun
		given = {}
		partial = seed._partial(fun, given)
	else
		f = fun["\6f"]
		given = fun["\6g"]
		partial = fun
	end
	if arg == nil then
		if #given == f["\6arity"] then
			local t = {f["\6op"](unpack(given))}
			t["\5"]=true
			return t
		elseif f["\6macro"] and terminal then
			local t = {f["\6op"](given)}
			t["\5"]=true
			return t
		elseif -1 == f["\6arity"] and terminal then
			local t = {f["\6op"](unpack(given))}
			t["\5"]=true
			return t
		else
			return partial
		end
	end
  --[[
	if nests[#nests] and (nests[#nests] == arg or (type(arg) == 'table' and ((arg["\6op"]
      and nests[#nests] == arg["\6name"])
			or (arg["\6f"] and arg["\6f"]["\6op"]
			and nests[#nests] == arg["\6f"]["\6name"])))) then
		if #nests == 0 then
			local t = {f["\6op"](given)}
			t["\5"]=true
			return t
		else
			given[#given + 1] = arg
		end
	elseif type(arg) == 'table' and ((arg["\6op"] and arg["\6group"])
			or (arg["\6f"] and arg["\6f"]["\6group"])) then
		nests[#nests + 1] =  (arg["\6group"] or arg["\6f"]["\6group"])
		given[#given + 1] = arg
	else--]]
  if f["\6group"] then
    given = glue.extend(given, arg)
  elseif f["\6arity"] == -1 or #given < f["\6arity"] then
		given[#given + 1] = arg
	end
	if #given == f["\6arity"] then
		local t = {f["\6op"](unpack(given))}
		t["\5"]=true
		return t
	elseif f["\6macro"] and terminal then
		local t = {f["\6op"](given)}
		t["\5"]=true
		return t
	elseif -1 == f["\6arity"] and terminal then
		local t = {f["\6op"](unpack(given))}
		t["\5"]=true
		return t
	end
	return partial
end





-- reads a sequence of generated sexps and data from
-- the specified inlist, which should already have a program,
-- and steps through it until it has read a single form.
local function eval(inlist, quotelevel)
  if type(inlist) ~= 'table' then return inlist end
  quotelevel = quotelevel or 0
  if #inlist == 0 then return {} end
  
  local arglist = {}
  local term
  if quotelevel > 0 then
    for i=1, #inlist do
      local ql = quotelevel
      term = inlist[i]
      if arglist[i-1] == "\6unquote" then ql = ql - 1 end
      if type(term) == 'table' then 
        arglist[i] = eval(term, ql)
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
    local op, nm = identify(inlist[1])
    op = defunction(op, nm)
    
    if type(op) ~= 'table' then
      if is_keyword(op) then
        term = inlist[2]
        if type(term) == 'table' then 
          arglist[1] = eval(term, quotelevel)
        else
          arglist[1], nm = identify(term)
          if arglist[1] == nil then arglist[1] = term end
        end
        return kw_get(op, arglist[1])
      else
        error("Tried to call this invalid value like a fn:\n" .. pp.format(inlist[1]))
      end
    end
    
    if op.macro then      
      for i=2, #inlist do
        term = inlist[i]
        if type(term) == 'table' then 
          arglist[i-1] = eval(term, quotelevel + 1)
        else
          arglist[i-1] = quote_id(term)
        end
      end
      return op(unpack(arglist))
    end
    
    for i=2, #inlist do
      term = inlist[i]
      if type(term) == 'table' then 
        arglist[i-1] = eval(term, quotelevel)
      else
        arglist[i-1], nm = identify(term)
        if arglist[i-1] == nil then arglist[i-1] = term end
      end
    end
    return op(unpack(arglist))
  end
end

seed.eval = eval

function seed.minirun(program)
  return eval({"\1do",program})
end

-- the entry point for a program. Clears any possible lingering state,
-- then returns any number of args (ideally 1, if the program
-- completed with one return value) based on evaluating a list of
-- identifier strings and data.
function seed.run(program)
	seed.__scopes = {{},{}}
	seed.__namespace = nil
  return eval({"\1do",program})
end

-- a re-entry point for a program. Keeps any possible lingering state,
-- then returns any number of args (ideally 1, if the program
-- completed with one return value) based on evaluating a list of
-- identfier strings and data.
function seed.run_in(program)
  return eval({"\1do",program})
end

seed["nil"] = nil

local function def(ops, name, macro)
	seed[name] = make_fn(ops, name, macro)
end

seed.def = def

local function count(t)
	if type(t) == 'table' then
		return #t
	elseif not not t then
		return 1
  else
    return 0
	end
end

seed.count = count

local function _do(...)
  local res = nil
  for i=1, select('#', ...) do
    res = eval(select(i, ...))
  end
  return res
end

seed["do"] = _do


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