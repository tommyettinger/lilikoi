# lilikoi

## a little lisp-y programming language

Lilikoi is a brand-new programming language that is heavily inspired by
[Clojure](http://clojure.org), but with less emphasis on immutability,
and more drawn from array programming languages. It translates to
LuaJIT's dialect of Lua, and can interoperate with Lua code.

## EXAMPLES

Much of this is likely to change, but currently you can (starting from the
simplest)...
* Do basic arithmetic:
  
  `(+ 1 2 3)` returns 6.

* Define functions:
  
  `((fn half-pow [x y] (/ (pow x y) 2)) 10 3)` creates and immediately calls a fn, returning `500`.
   The name is optional.
  
  `((fn add-half ([x y] (+ x (/ y 2))) ([x & ys] (+ x (/ (reduce + ys) 2)))) 10 1 2 3)` creates and immediately calls a fn with multiple argument lists, uses the second fn body with the variable-length arglist `[x & ys]`, and returns `13`.
  
* Use functional programming techniques:
  
  `(reduce * [2 3 4 5])` returns the result of multiplying all the elements in the vector, `120`
  
  `(reductions * [2 3 4 5])` returns a table as a list, `{2, 6, 24, 120}`

  Clojure-style macros are being developed now.
  
## SETUP

This repo uses multigit (scripts are included) to pull in its dependencies
into the same folder. run `mgit clone-all` on Windows or `./mgit clone-all`
on Mac or Linux to download all dependencies, including LuaJIT, into the
current folder. You can use `mgit --all pull` on Windows or
`./mgit --all pull` on Mac or Linux to update. Thanks to the LuaPower project
for all their hard work!

## Lua API

### `local lil = require'lilikoi'`
------------------------------------------ ----------------------
`lil.translate(code) -> str | nil,err`  translate to lua
`lil.run(code) -> value | nil,err`           run code directly
------------------------------------------ ----------------------

### `lil.translate(code) -> str | nil,err`

Translates a piece of lilikoi code, given as a string, to a string
of lua code as a result.
Raises an error if it fails.

### `lil.run(code) -> value | nil,err`

Translates a piece of lilikoi code, given as a string, to lua and
immediately evaluates that lua code, returning whatever the last form in
the code evaluates to, if it succeeds. Raises an error if it fails.

## FAQ

 * What does the name mean?
 * Lilikoi is the Hawaiian word for passionfruit,
   and I had just eaten some excellent passionfruit ice cream when I started
   thinking about ways to solve some core design challenges of this language.
   Also, it's a lil' language.
 * Any credits to mention?
 * Lewis Campbell came up with the original idea of a curried stack language,
   and that inspired the first design of lilikoi. Even though it later became
   difficult to understand the subtleties of the syntax it used, and I switched
   to using Lisp's prefix notation, implementing the original idea showed me
   that it isn't as hard as I thought to make a programming language.
   Joshua Day, Risto Saarelma, Derrick Creamer, and the other helpful folk
   of #rgrd helped clarify many rough patches in the language idea.
   Alan Malloy, Gary Fredericks, Justin Smith, and the rest of the wonderful
   #clojure community also have contributed in various ways to me being able
   to make this. Peter Keller has been an invaluable aid as well; having
   access to the experience of someone who has already implemented multiple
   Lisps has been humbling, and I am grateful to everyone who's contributed
   in any way.

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

