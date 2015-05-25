local lil = require'lilikoi'
local pp = require'pp'

local function check(llk)
	print(llk)
	pp(lil.translate(llk))
	pp(lil.run(llk))
end

check("'hello, world!'")
check("7 / 2")
check("2.5 * 4 - (7 / 2)")
check("1 + 2 * 3 / (10 * 1.2)")
check("1 + 2 * (7 + 8 - 3) / (10 * (6 / 5))")
check("[ 10 20 30 ]")
check("(1 + math.pi)")
check("(math.max 2 3 math.pi)")
check("([ 10 20 30 ] =get 2)")
check("@(* 2) 3")
check("[ ]")
check("(execute '2 * 3 + 4')")
check("(execute '2 * (3 + 4)') = 14")
check("@(fn [ a b ] (b ^ a)) 3 2")
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

check("(map (*) [ 10 20 30 ])") --(defn call2 [ f v ] (call f v)) 

check("(defgroup *[ ]* (map (*) &&&)) (vmap (supply call) *[ 2 3 4 ]* [ 10 20 30 ])") --(defn call2 [ f v ] (call f v)) 

print'OK'
