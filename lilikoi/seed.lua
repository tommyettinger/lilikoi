
-- lilikoi/seed: the core lib used by lilikoi code after translation.
-- Written by Tommy Ettinger. Public Domain.

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
seed.__inlists = {}
seed.__outlists = {}

seed.__scopes = {{}, {}}
seed.__namespace = nil

local function _directcall(fun, ...)
  if(select('#') > 0) then
    return rawget(fun, "\6op")(...)
  else
    return rawget(fun, "\6op")()
  end
end

local function _givencall(fun, ...)
  local filled, supplied, target = #rawget(fun, "\6g"), select('#', ...), rawget(rawget(fun, "\6f"), "\6arity")
  if filled + supplied < target then
    local g = {}
    local nu = seed._partial(
      rawget(fun, "\6f"),
      g,
      false)
    if filled > 0 then  
      for i=1,#rawget(fun, "\6g") do
        rawset(g, #g+1, rawget(rawget(fun, "\6g"), i))
      end
      rawset(nu, "\6q", true)
    end
    if supplied > 0 then
      for i=1,select('#',...) do
        rawset(g, #g+1, (select(i,...)))
      end
      rawset(nu, "\6q", true)
    end
    return nu
  elseif filled == 0 then return rawget(rawget(fun, "\6f"), "\6op")(...)
  else return rawget(rawget(fun, "\6f"), "\6op")(unpack(rawget(fun, "\6g")), ...)
  end
end
local _directmeta = {__call = _directcall}
local _givenmeta = {__call = _givencall}
function seed._functor(op, arity, name, group, macro)
	return setmetatable({
		["\6op"] = op,
		["\6arity"] = arity,
		["\6name"] = name,
		["\6group"] = group,
		["\6macro"] = macro
	}, _directmeta)
end

function seed._partial(fun, given, quote)
	return setmetatable({
		["\6f"] = fun,
		["\6g"] = given or {},
		["\6q"] = quote
	}, _givenmeta)
end

local function lookup_helper(name, pact, tgt)
	local ret = tgt[pact[2]]
	if #pact > 2 then
		for i,a in ipairs(pact) do
			if i > 2 then
				if ret[a] == nil then return nil, nil end
				if type(ret[a]) == 'function' then
					ret = seed._functor(ret[a], -1, name)
					seed.__scopes[1][name] = ret
					return ret, name
				else
					ret = ret[a]
				end
			end
		end
	end
	return ret, name
end
function seed._lookup(name)
  local pact = {name}
	for s in glue.gsplit(name, ".", 1, true) do
		pact[#pact + 1] = s
	end
	local revi = #seed.__scopes
	while revi > 0 do
		if seed.__scopes[revi][name] ~= nil then
			return seed.__scopes[revi][name], name
		end
		revi = revi - 1
	end
	local lookups
	if seed.__namespace then
		lookups = {seed.__namespace, seed}
	else
		lookups = {seed}
	end
	local res, nm
	for t,ns in ipairs(lookups) do
		res, nm = lookup_helper(name, pact, ns)
		if res then
			return res, nm
		end
	end
	return nil, name
end

function seed._is_identifier(name)
  if type(name) == 'string' and #name > 2 then
    return string.sub(name, 1, 2) == "\5%"
  end
  return false
end

function seed._is_quoted(name)
  if type(name) == 'string' and #name > 2 then
    return string.sub(name, 1, 2) == "\6,"
  end
  return false
end

function seed._is_keyword(name)
  if type(name) == 'string' and #name > 2 then
    return string.sub(name, 1, 2) == "\5:"
  end
  return false
end


function seed._identify(name)
  if seed._is_identifier(name) then
    local nm = string.sub(name, 3)
    return seed._lookup(nm)
  end
  return nil, nil
end

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
-- reads a sequence of generated function tables and data from
-- the specified inlist, which should already have a program,
-- and steps through it until it has read a single form, which
-- is usually a group but can be any result of a functiom,
-- then returns the final outlist produced by that form.
function seed.__eval()
	local outlist = seed.__outlists[#seed.__outlists] or {}
  local inlist = seed.__inlists[#seed.__inlists] or {}
  local halt = nil
--	while #seed.__inlists[#seed.__inlists] > 0 and not halt do
		-- consume any tokens that were used
		local a = table.remove(seed.__inlists[#seed.__inlists])
		local ided, nm = seed._identify(a)
    halt = ided == '\5%stop'
		local terminal = #(seed.__inlists[#seed.__inlists]) == 0 -- or halt
		if type(a) == 'function' then
			a = seed._functor(a, -1, "__NATIVE")
		end
		if type(a) == 'table' and (a["\6group"] or (a["\6f"] and a["\6f"]["\6group"])) then
			ided, nm = a, a["\6name"]
		end
		if type(outlist[#outlist]) == 'table' and (outlist[#outlist]["\6macro"]
         or (outlist[#outlist]["\6f"] and outlist[#outlist]["\6f"]["\6macro"])) then
			-- if we are running through a macro, only use names, not values.
			local r, g, q
			if nm then
				q = '\6,' .. nm
        if type(ided) == 'table' and (ided["\6group"] or (ided["\6f"] and ided["\6f"]["\6group"])) then
          nests[#nests + 1] = ided["\6group"] or ided["\6f"]["\6group"]
        end
        if nm == nests[#nests] then
          -- we have reached the end of a level of nesting
          table.remove(nests)
          if #nests < nestlevel then
            -- we have ended the macro
            
            r = seed.__step(table.remove(outlist), nil, true) -- NOTE using nm, not ided
            for i= #r, 1, -1 do
              table.insert(seed.__inlists[#seed.__inlists], r[i])
            end
            return r
          end
        end
			else
				q = a
			end
			r = seed.__step(outlist[#outlist], q, terminal) -- NOTE using nm, not ided    
      outlist[#outlist] = r
      return r
		else
			-- we are not running through a macro
			if ided and not (type(ided) == 'table' and (ided["\6op"] or ided["\6f"])) then
				-- we have looked up a value and it is not a functor or partial
				a = ided
			elseif type(a) == 'table' and a["\6name"] and not a["\6q"] then
				ided, nm = a, a["\6name"]
			end  
      if type(outlist[#outlist]) == 'table' and ((outlist[#outlist]["\6op"] and
					outlist[#outlist]["\6group"]) or (outlist[#outlist]["\6f"] and
					outlist[#outlist]["\6f"]["\6op"] and outlist[#outlist]["\6f"]["\6group"])) then
				-- if we are continuing a function that is on the outlist, and that
				-- function is a grouper, repeatedly eval until the group has closed.
        local r, top
--        table.insert(seed.__inlists[#seed.__inlists], a)
        repeat
          r = seed.__eval()
          top = r[#r]
          if '\5%' == top then
            table.remove(r)
          end
          if not r["\5"] then
            -- if the eval produced a function call result, then the return of
            -- that call has been pushed to inlist, and should be read in by
            -- the next function call (so "1 + 2 * 4" puts 3 on the inlist then
            -- multiplies it by 4, the puts 12 on the inlist). if the latest
            -- eval has only read data from the inlist, and no function call
            -- results, then we want to put that data into the group.
            outlist[#outlist] = seed.__step(outlist[#outlist], r, terminal)
          end
        until ('\5%' == top or #seed.__inlists[#seed.__inlists] == 0)
        table.remove(nests)
        nestlevel = #nests
        local otemp = table.remove(outlist)
        glue.extend(outlist, seed.__step(otemp, nil, true))
        local o = {["\5"] = true}
        for i=1, #outlist do
          local v = outlist[i]
          if type(v) == 'table' and v["\6op"] then
            local tv = seed._partial(v, {}, true)
            table.insert(o, tv)
--            table.insert(seed.__inlists[#seed.__inlists], tv)
          elseif type(v) == 'table' and v["\6f"] then
            v["\6q"] = true
            table.insert(o, v)
--            table.insert(seed.__inlists[#seed.__inlists], v)
          else
            table.insert(o, v)
--            table.insert(seed.__inlists[#seed.__inlists], v)
          end
				end
        -- we have read one form, return it as a list of all results
				return o
        --[[
        if type(ided) == 'table' and (ided["\6group"] or (ided["\6f"] and ided["\6f"]["\6group"])) then
					r = seed.__step(outlist[#outlist], ided, terminal)
				else
					-- NOTE using a, not ided
					r = seed.__step(outlist[#outlist], a, terminal)
				end
        
        r = glue.reverse(outlist[#outlist])
        for i,v in ipairs(r) do
          if type(v) == 'table' and v["\6op"] then
            table.insert(seed.__inlists[#seed.__inlists], seed._partial(v, {}, true))
          elseif type(v) == 'table' and v["\6f"] then
            v["\6q"] = true
            table.insert(seed.__inlists[#seed.__inlists], v)
          else
            table.insert(seed.__inlists[#seed.__inlists], v)
          end
				end
        table.remove(outlist)
        --]]
      elseif type(ided) == 'table' and (ided["\6op"] or
				ided["\6f"]) and not ided["\6q"] then
				
        if ided["\6group"] or (ided["\6f"] and ided["\6f"]["\6group"]) then
				-- if we have just started executing a grouping function,
				-- ignore the current outlist and start the grouping.
					
          seed.__outlists[#seed.__outlists + 1] = {}
          
          if ided["\6group"] or ided["\6f"]["\6group"] then
            nests[#nests + 1] = ided["\6group"] or ided["\6f"]["\6group"]
          end
		
          nestlevel = #nests
          
          repeat
            r = seed.__eval()
            top = r[#r]
            if '\5%stop' == top then
              table.remove(r)
            end
            if not r["\5"] then
--              ided = seed.__step(ided, r, terminal)
            end
          until ('\5%stop' == top or #seed.__inlists[#seed.__inlists] == 0)
          if '\5%stop' == top then
            table.remove(seed.__outlists[#seed.__outlists])
            table.remove(nests)
            nestlevel = #nests
          end
          local otemp = table.remove(seed.__outlists)
          local res = seed.__step(ided, otemp, true)
          local o = {["\5"] = true}
          for i = #res, 1, -1 do
            local v = res[i]
            if type(v) == 'table' and v["\6op"] then
              local tv = seed._partial(v, {}, true)
              table.insert(o, tv)
              table.insert(inlist, tv) -- 
            elseif type(v) == 'table' and v["\6f"] then
              v["\6q"] = true
              table.insert(o, v)
              table.insert(inlist, v) -- seed.__inlists[#seed.__inlists]
            else
              table.insert(o, v)
              table.insert(inlist, v) -- seed.__inlists[#seed.__inlists]
            end
          end
          -- we have read one form, return it as a list of all results
          return o
        
          
				elseif type(outlist[#outlist]) == 'table' and (outlist[#outlist]["\6op"] or
            outlist[#outlist]["\6f"]) then
				-- if we are continuing a function and encounter another function,
				-- terminate the continuing function, and call the current function
        -- with the result of the previous.
          local prev = seed.__step(table.remove(outlist), nil, true)
          if prev["\6op"] then
            table.insert(outlist, seed._partial(prev, {}, true))
          elseif prev["\6g"] then
            prev["\6q"] = true
            table.insert(outlist, prev)
          else
            outlist = glue.extend(outlist, prev)
          end
					outlist[#outlist] = seed.__step(ided, prev, terminal)
          
					if(outlist[#outlist]["\5"]) then
            if outlist[#outlist][1] == '\5%stop' then
              local o = {}
              for i= #prev, 1, -1 do
                local v = prev[i]
                if type(v) == 'table' and v["\6op"] then
--                  table.insert(o, seed._partial(v, {}, true))
                  table.insert(o, seed._partial(v, {}))
                elseif type(v) == 'table' and v["\6f"] then
--                  v["\6q"] = true
                  table.insert(o, v)
                else
                  table.insert(o, v)
                end
              end
              o[#o + 1] = '\5%stop'
              return o
            end
            local r = glue.extend(prev, table.remove(outlist))
						for i= #r, 1, -1 do
              local v = r[i]
              if type(v) == 'table' and v["\6op"] then
                table.insert(seed.__inlists[#seed.__inlists], seed._partial(v, {}, true))
              elseif type(v) == 'table' and v["\6f"] then
                v["\6q"] = true
                table.insert(seed.__inlists[#seed.__inlists], v)
              else
                table.insert(seed.__inlists[#seed.__inlists], v)
              end
						end
            r["\5"] = true
            return r
          end
        else
				-- if we have just started executing a function,
				-- replace the top of the outlist with the function called
				-- with the content of the top of the outlist.
					local start = #outlist
					if start == 0 then start = 1 end
					local r
          if (ided["\6op"] and ided["\6arity"] == 0) or (ided["\6f"] and ided["\6f"]["\6arity"] == 0) then
            r = seed.__step(ided, nil, terminal)
            
            if(r["\5"]) then
              if r[1] == '\5%stop' then
                outlist[#outlist + 1] = '\5%'
                r["\5"] = nil
                return r
              end
              
              for i= #r, 1, -1 do
                table.insert(seed.__inlists[#seed.__inlists], r[i])
              end
              for i= #outlist, 1, -1 do
                table.insert(seed.__inlists[#seed.__inlists], outlist[i])
              end
              
              -- return r
            else
              outlist[start] = r
            end
            return r
          else
            r = seed.__step(ided, outlist[#outlist], terminal)
            if(r["\5"]) then
              table.remove(outlist)
              --[[
              for i= #r, 1, -1 do
                table.insert(seed.__inlists[#seed.__inlists], r[i])
              end--]]
              -- return r
            else
              outlist[start] = r
            end
            return r
          end
				end
			elseif type(outlist[#outlist]) == 'table' and (outlist[#outlist]["\6op"] or
				outlist[#outlist]["\6f"])
				and not outlist[#outlist]["\6q"] then
				-- if we are continuing a function that is on the outlist,
				-- and we have been given data and not a new function,
				-- replace the top of the outlist with the function part-called
				-- with the latest item received.    
        local ol = table.remove(outlist)
        local r
        if '\5%stop' == a then
          r = seed.__step(ol, nil, true) -- NOTE using a, again
        else
          r = seed.__step(ol, a, terminal) -- NOTE using a, again
        end
				if(r["\5"]) then
					for i=#r, 1, -1 do
						table.insert(outlist, r[i])
					end
        else
					outlist[#outlist + 1] = r
				end
        return r

			else
				-- if we are not continuing or starting a function,
				-- append a piece of data to the outlist.
        -- note that this does not return; either this data
        -- will be an argument to an upcoming function, or
        -- this is an element of a list or something similar,
        -- and will be returned when the list terminates.
				outlist[#outlist + 1] = a
        return {a}
			end
		end
	
  if(#(seed.__inlists[#seed.__inlists]) == 0) then
    table.remove(seed.__inlists)
  end
	return outlist
end

function seed._minirun(program)
  table.insert(program, "\5%)")
  local prog = glue.reverse(program)
  table.insert(prog, "\5%(")
  
  seed.__inlists[#seed.__inlists + 1] = prog
  return seed.__eval()
end

-- the entry point for a program. Clears any possible lingering state,
-- then returns any number of args (ideally 1, if the program
-- completed with one return value) based on evaluating a list of
-- identifier strings and data.
function seed.__run(program)
	nests = {}
  seed.__outlists = {}
	seed.__inlists = {glue.reverse(program)}
  seed.__scopes = {{},{}}
	seed.__namespace = nil
	
	return unpack(seed.__eval())
end

-- a re-entry point for a program. Keeps any possible lingering state,
-- then returns any number of args (ideally 1, if the program
-- completed with one return value) based on evaluating a list of
-- identfier strings and data.
function seed.__run_in(program)
	seed.__inlists[#seed.__inlists + 1] = glue.reverse(program)
	return unpack(seed.__eval())
end

seed["nil"] = nil

seed.__munge_table = {
["%"]="\6mod",
["\\"]="\6back",
}

function seed.munge(name)
	return name:gsub(
		"^%.", "\6dot"):gsub(
			"%.$", "\6dot"):gsub(
				"[%%\\]", seed.__munge_table);
end

function seed.__def(op, arity, name, group, macro)
	seed[seed.munge(name)] = seed._functor(op, arity, name, group, macro)
end

function seed._clean(name)
	if type(name) == 'string' and string.find(name, "^\6,") then
		return string.sub(name, 3)
	else
		return name
	end
end

function seed._unquote(name)
	if type(name) == 'string' then
		if string.find(name, "^\6,`.") then
			return "\6," .. string.sub(name, 4)
		elseif seed._is_quoted(name) then
			return "\5%" .. seed._clean(name)
		end
	end
	return name
end

local function _length(t)
	if type(t) == 'table' then
		return #t
	else
		return 0
	end
end

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

seed.math = math
seed.io = io
seed.file = file
seed.os = os
seed.string = string
seed.print = print

return glue.autoload(seed,
{
   ["+"] = 'lilikoi.operators',
   ["-"] = 'lilikoi.operators',
   ["*"] = 'lilikoi.operators',
   ["/"] = 'lilikoi.operators',
   ["^"] = 'lilikoi.operators',
   ["\6mod"] = 'lilikoi.operators',
   ["="] = 'lilikoi.operators',
   ["!="] = 'lilikoi.operators',
   ["<"] = 'lilikoi.operators',
   ["<="] = 'lilikoi.operators',
   [">"] = 'lilikoi.operators',
   [">="] = 'lilikoi.operators',
   concat = 'lilikoi.operators',
   transpile = 'lilikoi.transpiler',
   execute = 'lilikoi.transpiler',
   munge = 'lilikoi.transpiler'
})