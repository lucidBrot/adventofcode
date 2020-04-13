local amp = require('intcomputer')

local tmp = amp.instr_num_args(2)
print('output: ' .. tmp)
local b = IntComputer:new(1)
print('intcomputer: ' .. b.phase)
local c = tostring(133700 / 100)
print('num: ' .. c:sub(3,4))

local x = {}
x[1] = "hello"
x[2] = "bye"
local y = x
y[2] = "rekt"
print("x2: " .. x[2] ..", y2: " .. y[2])

function parse_comma_str(str)
    local tab = {}
    -- match everything but the comma
    for word in string.gmatch(str, '([^,]+)') do
        tab[#tab+1] = word
    end
    return tab
end

function read_file(file)
    local f = assert(io.open(file, "rb"))
    local content = f:read("*all")
    f:close()
    return content
end

function run_file(file)
    local f = read_file(file)
    local a = IntComputer:new(1, parse_comma_str(f))
    a:run()
end

run_file("input.txt")
