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
local grammar = require'lilikoi.grammar'
pp(grammar.lex("1"))
pp(grammar.lex('"abc"'))
pp(grammar.lex('"abcd" 2'))
pp(grammar.lex('()'))
pp(grammar.lex('(1)'))
pp(grammar.lex('[1]'))
pp(grammar.lex('(reduce + [1 2 3])'))
pp(grammar.lex('(reduce #(str %1 (val %2)) "" {:a 1 :b 2})'))
--[[
local function check(llk)
	print(llk)
	pp(lil.translate(llk))
	pp(lil.run(llk))
end
check("(str (2 ^ 3))")
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
