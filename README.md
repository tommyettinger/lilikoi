### lilikoi

## a little stack/functional/array programming language

Lilikoi is a brand-new programming language with bits and pieces drawn from
concatenative (also called stack), functional, and array programming families.
It translates to LuaJIT's dialect of Lua.

## EXAMPLES

None yet, this whole project just started!

## SETUP

This repo uses multigit (scripts are included) to pull in its dependencies
into the same folder. run `mgit clone-all` on Windows or `./mgit clone-all`
on Mac or Linux to download all dependencies, including LuaJIT, into the
current folder. You can use `mgit --all pull` on Windows or
`./mgit --all pull` on Mac or Linux to update. Thanks to the LuaPower project
for all their hard work!

## API

# `local lil = require'lilikoi'`
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

