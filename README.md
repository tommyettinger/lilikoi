Lilikoi
===

Lilikoi is an experiment that's trying to add various features to the Lua programming language, targeting LuaJIT.
It uses the LuaJIT Language Toolkit to generate LuaJIT-2.1-compatible bytecode.

Goals
---

 * Macros
   * Start with textual substitution, replacing words with snippets of code before executing.
   * Move on to AST manipulation, repeatedly performing a stage of macroexpansion to fully replace any macros in source code with the resulting modified AST in generated code.
 * Use some techniques from functional programming, preferring naming conventions from Clojure but offering mutable variants on functions.
   * We don't want too much ASCII soup, though. One-word, all-English-letter names are preferred; if another word is needed, it is separated with an underscore. Capital letters are used as suffixes much like `!` and `?` are in Clojure.
   * `map`, `reduce`, and `filter` should all be present and usable by default, and aliases (using macros) can be used to ensure they don't overlap with existing variable names if so desired.
     * `mapM` and `filterM` mutate the collection (singular) they are called on. A `M` suffix should be reserved for `M`utable functions that act like the non-suffixed function, but mutate one or more arguments.
     * Similarly, an `I` suffix is for `I`mmutable functions that act like a mutable-by-default non-suffixed function, but return a new and different value instead of mutating their argument.
     * A `Q` suffix is for `Q`uery functions that return a true or false value based on the contents of their argument; essentially, a predicate, which would be suffixed with `?` in Clojure.
     * There is no equivalent to Clojure's `!` because it means "unsafe during transactions" there, and since we aren't doing anything on multiple threads, we don't want the cruft involved with transactions.
     * You should never see any overlap between these suffixes, though suffix conventions may be added in the future that overlap with `M` or `I`; in that case, suffixes are always added in alphabetical order.
 * Use some techniques from array programming, preferring the behavior of the J programming language but staying far away from its naming conventions.
   * J has a concept of "rank" for both functions (which it calls verbs, for good reason) and collections (which are pretty much always arrays as far as I can tell).
     * The rank of a collection essentially boils down to how many dimensions the array has, or in a broader sense, how many indices are required to get a scalar from the collection. Scalars, like the number 42, have rank 0.
   * The seq abstraction from Clojure is solid gold. Being able to treat multiple varieties of sequential data as a uniform data structure is extremely useful in practice.
     * However, Lua has less data structures. There's obviously tables (which are fantastic), but on top of that, strings can be considered sequences of characters (or I suppose length-1 strings here), and LuaJIT adds FFI arrays and potentially more.
     * It would be nice to be able to use the seq abstraction for multi-dimensional rectangular data.
       * One way this could work is if you have a number of keys equal to the data's rank, you get a scalar, but if you have less keys, you get another rectangular sequence (if you have all but one needed key, then a column of a 2D array or a plane of a 3D array).
     * Tables can be used to implement numerically-indexed arrays, but they are almost always jagged arrays (you could insert an element into what was a rectangular array, and unless the metatable forbids it, you now have a jagged array).
     * FFI arrays can be rectangular, but could also use C pointers to make a jagged array anyway.
     * The behavior of APL and J in regards to unassigned indices (or reshaping an existing array to a larger amount of total elements) is to give the (nth modulo number of values) value in the array if the nth unassigned value is requested, doing a "wraparound".
     * This is good in many ways, in particular for the exploratory programming that APL and J are exceptional at, but should have a way to be overridden.
     * N-dimensional arrays probably require some variant on an existing data structure to have efficient lookup of arbitrary dimensions and to have the "wraparound" behavior, possibly using metatables or possibly defined with FFI types.
   * J calls its functions "verbs" because it also has "adverbs" that modify an existing function's behavior. This can be extremely hard to follow if used improperly, but if it isn't overused it could be very good to have.
     * There was something about pads and drops here but it was essentially re-implementing optional parameters with worse syntax and behavior.

FAQ
---

 * What does the name mean?
 * Lilikoi is the Hawaiian word for passionfruit,
   and I had just eaten some excellent passionfruit ice cream when I started
   thinking about ways to solve some core design challenges of this language.
   Also, it's a lil' language.

Current Status
---

Currently Lilikoi is vaporware (or to polish that up a bit, "pre-alpha").

It uses the LuaJIT Language Toolkit, which is itself in beta.
