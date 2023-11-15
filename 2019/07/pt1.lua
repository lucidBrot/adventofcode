-- import my intcomputer code
local IntComputer = require('intcomputer')
-- import rosettacode's permutations
local rosetta = require('rosetta')

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
function Q.run_file(file, phase, live_inputs)
    local f = Q.read_file(file)
    local a = IntComputer:new(phase, Q.parse_comma_str(f), live_inputs)
    a:run()
    return a:get_latest_output()
end

-- run intcode file in a new thread with only the phase as argument.
local f = function (phas) return Q.run_file("input_7.txt", phas) end

-- for any permutation of [0,1,2,3,4] run the sequence and keep the max
local MAX_result = nil
function callback(phases)
    local next_input = 0
    for i = 1, #phases do
        -- I'm assuming the next input is always just one number
        next_input = Q.run_file("input_7.txt", phases[i], {next_input})
    end
    if MAX_result == nil or MAX_result < next_input then
        MAX_result = next_input
    end
end

local possible_phases = {0,1,2,3,4}
rosetta.permutation(possible_phases, #possible_phases, callback)
print("MAX: " .. MAX_result)

