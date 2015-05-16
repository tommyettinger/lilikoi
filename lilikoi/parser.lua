
-- lilikoi/parser: an internal module for parsing lilikoi.
-- Written by Tommy Ettinger. Public Domain.

local parser = {}

-- returns an iterator function that splits on any whitespace or commas.
function parser.tokenize(s)
    return string.gmatch(s, "[^%s,]+")
end

return parser
