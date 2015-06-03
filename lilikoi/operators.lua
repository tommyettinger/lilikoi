local seed = require'lilikoi.seed'
local fp = require'fun'
local op = fp.op

local function opdef(oper, name)
	seed[name] = seed.make_fn({[2]=oper, [-1]=function(x, y, ...) return fp.reduce(oper, {x, y, ...}) end}, name)
end
opdef(op.lt, '<')
opdef(op.le, '<=')
opdef(op.eq, '=')
opdef(op.ne, '!=')
opdef(op.ge, '>=')
opdef(op.gt, '>')

opdef(op.add, '+')
opdef(op.div, '/')
opdef(op.floordiv, '/_')
opdef(op.intdiv, '/|')
opdef(op.mod, '%')
opdef(op.mul, '*')
opdef(op.pow, 'pow')
opdef(op.concat, 'concat')

seed['-'] = seed.make_fn({[1]=op.neq, [2]=op.sub, [-1]=function(x, y, ...) return fp.reduce(op.sub, {x, y, ...}) end}, name)
seed.len = seed.make_fn({[1]=op.len}, name)