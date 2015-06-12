--local lil = require'lilikoi'
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
local fun = require'fun' ()
local lil = require'lilikoi'
local grammar = require'lilikoi.grammar'


--[=[
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
 

local function transfer(capt, pos)
  -- notes
  -- transfer must repeatedly be called, once on each form in the lexed list.
  -- if it encounters a scalar or non-group form at the top-level, it returns
  -- a codestring for that value. BUT if it encounters a group opener, a prefix,
  -- or any other kind of value that awaits further data, it recurses at the end
  -- of transfer, calling transfer again but with the elements of the group as
  -- its new "top-level."
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
          state[#state + 1] = '{"\1table-map",'
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
--]=]
local function check(llk)
	print(llk)
  pp(grammar.lex(llk))
  pp(lil.translate(llk))
	pp(lil.run(llk))
end
--[==[
check("1")
check('"abc"')
check('#[[abc\ndef]]')
check('"ab\ncd" 2')
check('()')
check('(+ 1 2)')
check("(+ ;;[[commentary\nYou'd think this should work, right?]] 1 2 3)")
check('(+ 1.1 2.2 3.3 4.4)')
check('[1]')
check('(+ 1.1 2.2 3.3 4.4 (* 3 3))')


--check('(reduce + [1 2 3])')
check('(reduce + [1 2 3 math.pi])')
--]==]
--check("(reduce + '(1 2 3))")
check("(reduce + '(1 2 3 ~math.pi))")
check("(reduce + (list 1 2 3 math.pi))")
--[=[
check('(reduce #(str %1 (val %2)) "" {:a 1 :b 2})')
check('(reduce #(str %1 (val %2)) ^[:what ever] {:a 1 :b 2})')
check("(apply math.min '(1 2 3))")
check("(defmacro defvariad [opname op] `(defn ~opname [& $args] (reduce ~op $args)))")
check("(apply '~$math.min.foo.bar '''(1 2 3))")
check("math.foo.bar.pi")
check('(reduce + [1 2 3])')
check('((fn ([op] (reduce op [1 2 3])) ([op start] (reduce op start [1 2 3]))) + 10)')
check('((fn [op] (reduce op [1 2 3])) +)')
--]=]



--[=[
each(pp.print, partition(4, range(12)))
uit.each(pp.print, uit.partition(4, uit.range(12)))



each(pp.print, scan(operator.add, 0, {2, 4, 6, 8}))
each(pp.print, reductions(operator.add, {2, 4, 6, 8}))

uit.each(pp.print, uit.scan(operator.add, 0, uit.iter({2, 4, 6, 8})))
uit.each(pp.print, uit.reductions(operator.add, uit.iter({2, 4, 6, 8})))

local u = uit.iter({2, 4, 6, 8})

uit.each(pp.print, uit.scan(operator.add, 0, u))
uit.each(pp.print, uit.scan(operator.add, 0, u))
uit.each(pp.print, uit.scan(operator.add, 0, u))


uit.each(pp.print, uit.drop_while(function(n) return n < 5 end, u))
uit.each(pp.print, uit.drop_while(function(n) return n < 5 end, u))
uit.each(pp.print, uit.drop_while(function(n) return n < 5 end, u))

pp(uit.totable(u))
pp(uit.totable(uit.reductions(operator.add, u)))
--]=]
--uit.each(pp.print, u)

--check("(str (2 ^ 3))")
--]]
--check("'hello, world!'")
--check("7 / 2")
--[[
check("2 * (72)")
check("(7 / 2)")
check("2.5 * 4 - (7 / 2)")
check("1 + 2 * 3 / (10 * 1.2)")

check("((6 / 5) * 10)")
check("1 + 2 * (7 + 8 - 3) / (10 * (6 / 5))")
check("[ 10 20 30 ]")
check("(1 + math.pi)")

check("(math.max 2 3 math.pi)")
check("([ 10 20 30 ] =get 2)")

check("@(* 2) 3")
check("[ ]")

check("(execute_in '2 * 3 + 4')")
check("(execute_in '2 * (3 + 4)') = 14")

check("(fn [ a b ] (b ^ a)) 3 2")
check("(str (2 ^ 3))")
check("(fn [ a b ] (math.max a b math.pi)) @ 3 2")
check("call (fn [ a b ] (str (a ^ b) ' = ' a '^' b)) 3 2")
check("(def formula (fn [ a b ] (str (a ^ b) ' = ' a '^' b))) (formula 3 3)")
check("(defn formula [ a b ] (str (a ^ b) ' = ' a '^' b)) (formula 4 3)")
check("(+ 1 2 4)")

check("(call (* 2) 3)")
check("map (* 2) [ 1 2 3 ]")
check("(off-map (* 2) [ 'foo' 1 2 3 ] 1)")
check("(vmap (*) [ 1 2 3 4 ] [ 10 20 30 ])")
check("(reduce (+) [ 1 2 3 4 ])")
check("(reduce-with (^) [ 1 2 3 ] 2)")
check("(defn sum [ &&& ] (reduce (+) &&&)) (sum 2 4 6 8)")

check("@(clean) '\6,foobar'")
check("(defmacro component [ &&& ] (map (clean) &&&)) (component a 3 b 2)")
check("(defmacro defwrapper [ name value ] (def name [ value ])) (defwrapper alpha ~ [ 1 2 3 ]) alpha")
check("(def t [ 1 2 3 ]) (defmacro defwrapper [ name value ] (def name [ value ])) (defwrapper alpha t) (map (+ 1) (alpha =get 1))")
check("(def t [ 10 20 30 ]) (defmacro defwrapper [ name value1 value2 ] (def name [ value1 value2 value2 ])) (defwrapper alpha ~ [ 1 2 3 ] ~ t) alpha")

check("(map (*) [ 10 20 30 ])")
check("(def inc (1 +)) (inc 5) + 4")
check("(defn decimate [ num ] num * 0.9) (decimate 100)")
check("(defn decimate [ num ] num * 0.9) (map (decimate) [ 10 20 40 80 160 ])")
check("(defgroup *[ ]* (map (*) &&&)) *[ 2 3 4 ]*")
check("(defgroup *[ ]* (map (*) &&&)) (vmap (supply call) *[ 2 3 4 ]* [ 10 20 30 ])")
--]]
print'OK'
