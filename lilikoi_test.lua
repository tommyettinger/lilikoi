local lil = require'lilikoi'
local glue = require'glue'

print(lil.translate"1 + 2")
local o = lil.run"300 + 2"
print(o)
for i,v in pairs(glue.keys(o)) do
	print(v .. " : " .. o[v] .. "\n")
end
print'OK'
