-- import my intcomputer code
local IntComputer = require('intcomputer')

-- define helper functions for input parsing
local Q = {}
function Q.parse_comma_str(str)
    local tab = {}
    -- match everything but the comma
    for word in string.gmatch(str, '([^,]+)') do
        tab[#tab+1] = word
    end
    return tab
end

function Q.read_file(file)
    local f = assert(io.open(file, "rb"))
    local content = f:read("*all")
    f:close()
    return content
end

-- run intcode file 
-- phase: int ∈ [0,4]
-- file: "input.txt"
function Q.run_file(file, phase)
    local f = Q.read_file(file)
    local a = IntComputer:new(phase, Q.parse_comma_str(f))
    a:run()
    return a:get_latest_output()
end

-- run intcode file in a new thread
local f = function (phas) Q.run_file("input_7.txt", phas) end
lane_a = f(0)
lane_b = f(1)
lane_c = f(2)
lane_d = f(3)
lane_e = f(4)
-- Reading the results joins the threads, waiting for any results not already there.
print("------------------")
print("- Results:       -")
print("------------------")
print(lane_a, lane_b, lane_c, lane_d, lane_e)
