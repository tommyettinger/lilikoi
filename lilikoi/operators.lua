local seed = require'lilikoi.seed'
local fp = require'fun'
local ops = fp.op

local function opdef(oper, name)
	seed[name] = seed._make_fn({[2]=oper, [-1]=function(x, y, ...) return fp.reduce(oper, {x, y, ...}) end}, name)
end
opdef(ops.lt, '<')
opdef(ops.le, '<=')
opdef(ops.eq, '=')
opdef(ops.ne, '!=')
opdef(ops.ge, '>=')
opdef(ops.gt, '>')

opdef(ops.add, '+')
opdef(ops.div, '/')
opdef(ops.floordiv, '/_')
opdef(ops.intdiv, '/|')
opdef(ops.mod, '%')
opdef(ops.mul, '*')
opdef(ops.pow, 'pow')
opdef(ops.concat, 'concat')

opdef(ops.land, 'raw-and')
opdef(ops.lor, 'raw-or')

seed['-'] = seed._make_fn({[1]=ops.neq, [2]=ops.sub, [-1]=function(x, y, ...) return fp.reduce(ops.sub, {x, y, ...}) end}, '-')
seed.len = seed._make_fn({[1]=ops.len}, 'len')
seed['raw-not'] = seed._make_fn({[1]=ops.lnot}, 'raw-not')
seed['truthy'] = seed._make_fn({[1]=ops.truth}, 'truthy')
