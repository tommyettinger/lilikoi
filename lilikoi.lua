
-- lilikoi: translate a weird little language to lua.
-- Written by Tommy Ettinger. Public Domain.

local lilikoi = {} --lilikoi namespace
local seed = require'lilikoi.seed'
function lilikoi.translate(code, mod)
    return seed.transpile(code, mod)
end

function lilikoi.run(code, mod)
    return seed.execute(code, mod)
end

function lilikoi.run_in(code, mod)
    return seed.execute_in(code, mod)
end

return lilikoi
