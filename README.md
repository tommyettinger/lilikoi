# lilikoi

## a little stack/functional/array programming language

Lilikoi is a brand-new programming language with bits and pieces drawn from
concatenative (also called stack), functional, and array programming families.
It translates to LuaJIT's dialect of Lua.

## EXAMPLES

All of this is likely to change, but currently you can (starting from the
simplest)...
* Do basic arithmetic:
  `1 + 2` returns 3.
* Group arithmetic into logical sections (there is no order of operations):
  `2 * 3 / (5 - 3)` returns 2.5, since the group `(5 - 3)` goes separately
  and the rest goes left to right.
* Define constants and include functions from Lua's standard lib:
  `(def pi2 (math.pi * 2)) pi2` returns a Lua number that's equal to 2*pi.
* Create partial functions, pass them around, and call them when desired:
  `(def inc (1 +)) (inc 5) + 4` returns 10. The value of inc is a
  partial function, as in, it has only received some part of the argument
  list it needs to return the desired value. When it gets 5, it adds 1 to
  it, then adding 4 gets 10.
* Create new functions to operate on data:
  `(defn decimate [ num ] num * 0.9) (decimate 100)` returns 90. In square
  brackets after defn and a name, you have an argument list (here with
  only one argument, num), and everything after the brackets until the 
  parenthesized group ends will be executed after temporarily giving the
  arguments whatever values were passed to the function (here, num
  becomes 100, so in the body, 100 * 0.9 is 90).
* Use functions as first-class items:
  `(defn decimate [ num ] num * 0.9) (map (decimate) [ 10 20 40 80 160 ])`
  returns a Lua table with 5 elements: 9, 18, 36, 72, 144. It needs
  parentheses around `decimate` so it doesn't try to call decimate on the
  table; in future versions this is likely to change so you can call
  a function that takes a scalar argument and call it implicitly on each
  scalar element in a sequence.
* Create macros, custom grouping functions, and soon actually useful,
  new features like independent data and ways to access that data (the
  concept of domains, basically first-class sets of indices). With the
  right custom groups defined, you could run
  `[:< "I eat" 5 "servings of fruit a day." >:]` and get back a result
  modified by the grouping functions `[:< and >:]` such as
  `"I want" 2 "whole fried chickens and a Coke."`
  
## SETUP

This repo uses multigit (scripts are included) to pull in its dependencies
into the same folder. run `mgit clone-all` on Windows or `./mgit clone-all`
on Mac or Linux to download all dependencies, including LuaJIT, into the
current folder. You can use `mgit --all pull` on Windows or
`./mgit --all pull` on Mac or Linux to update. Thanks to the LuaPower project
for all their hard work!

## API

### `local lil = require'lilikoi'`
------------------------------------------ ----------------------
`lil.translate(code) -> result | nil,err`  translate to lua
`lil.run(code) -> nil | nil,err`           run code directly
------------------------------------------ ----------------------

### `lil.translate(code) -> result | nil,err`

Translates a piece of lilikoi code, given as a string, to a string
of lua code as a result.
Raises an error if it fails.

### `lil.run(code) -> true | nil,err`

Translates a piece of lilikoi code, given as a string, to lua and
immediately evaluates that lua code, returning true if it succeeds.
Raises an error if it fails.

## FAQ

 * What does the name mean?
 * Lilikoi is the Hawaiian word for passionfruit,
   and I had just eaten some excellent passionfruit ice cream when I started
   thinking about ways to solve some core design challenges of this language.
   Also, it's a lil' language.
 * Any credits to mention?
 * Lewis Campbell came up with the original idea of a curried stack language,
   and I decided to use a similar core concept with a different implementation.
   Joshua Day, Risto Saarelma, Derrick Creamer, and the other helpful folk
   of #rgrd helped clarify many rough patches in the language idea.
   Alan Malloy, Gary Fredericks, Justin Smith, and the rest of the wonderful
   #clojure community also have contributed in various ways to me being able
   to make this.

## LICENSE

This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to [the Unlicense site](http://unlicense.org)

