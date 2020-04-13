local amp = require('intcomputer')

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
