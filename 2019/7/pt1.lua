local amp = require('intcomputer')

local tmp = amp.instr_num_args(2)
print('output: ' .. tmp)
local b = IntComputer:new(1)
print('intcomputer: ' .. b.phase)
local c = tostring(133700 / 100)
print('num: ' .. c:sub(3,4))

