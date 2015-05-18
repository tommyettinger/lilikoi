
-- lilikoi: translate a weird little language to lua.
-- Written by Tommy Ettinger. Public Domain.

local lilikoi = {} --lilikoi namespace
local transpiler = require'lilikoi.transpiler'
function lilikoi.translate(code)
    return transpiler.gen(code)
end

function lilikoi.run(code)
    return assert(loadstring(transpiler.gen(code)))()
end

return lilikoi
