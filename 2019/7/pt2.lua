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

-- for any permutation of [0,1,2,3,4] run the sequence and keep the max
local MAX_result = nil
function callback(phases)
    local next_input = {0}
    -- create all coroutines in a suspended state
    local coroutines = {}
    for i = 1, #phases do
        -- I'm assuming the next input is always just one number
        coroutines[i] = coroutine.create(function () Q.run_file("input_7.txt", phases[i], next_input ) end )
        next_input = {}
    end

    -- run the first one, until it outputs a value. Then hand that value over to the second one, and so on.
    -- When a coroutine stops without outputting anything, we will have a possible result.
    local retcode = nil -- true unless the coroutine has stopped
    local retval = nil  -- the value that is outputted
    local repeat_outer = true
    repeat 
        for i = 1, #coroutines do
            retcode, retval = coroutine.resume(coroutines[i])
            if retcode == false then
                -- break the outer loop
                repeat_outer = false
                break
            else
                -- we have a value in retval
                -- and we want to pass it into the next coroutine
                INTCOMP[i+1]:append_inputs({retval})
            end
        end
    until repeat_outer == false

    if MAX_result == nil or MAX_result < retval then
        MAX_result = retval
    end
end

local possible_phases = {0,1,2,3,4}
rosetta.permutation(possible_phases, #possible_phases, callback)
print("MAX: " .. MAX_result)

