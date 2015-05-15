
-- lilikoi/tokenizer: an internal module for tokenizing lilikoi.
-- Written by Tommy Ettinger. Public Domain.

local tokenizer = {}

-- returns an iterator function that splits on any whitespace or commas.
function tokenizer.split(s)
    return string.gmatch(s, "[^%s,]+")
end

return tokenizer
