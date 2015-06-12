
-- lilikoi: translate a weird little language to lua.
-- Written by Tommy Ettinger. Public Domain.

local lilikoi = {} --lilikoi namespace
local seed = require'lilikoi.seed'
function lilikoi.translate(code)
    return seed.transpile(code)
end

function lilikoi.run(code)
    return seed.execute(code)
end

function lilikoi.run_in(code)
    return seed.execute_in(code)
end

return lilikoi
