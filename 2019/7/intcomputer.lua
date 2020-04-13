local M = {}

-- a table that gives you the number of input arguments for the opcode
-- e.g.    M.instr_num_input_args[01]
M.instr_num_input_args = {
    [01] = 2,
    [02] = 2,
    [03] = 0,
    [04] = 1,
    [05] = 2,
    [06] = 2,
    [07] = 2,
    [08] = 2,
    [99] = 0,
}

-- a table that gives you the number of output arguments for the opcode
M.instr_num_output_args = {
    [01] = 1,
    [02] = 1,
    [03] = 1,
    [04] = 0,
    [05] = 0,
    [06] = 0,
    [07] = 1,
    [08] = 1,
    [99] = 0,
}

-- total number of arguments for opcode
function M.instr_num_args(opcode)
    return M.instr_num_input_args[opcode] + M.instr_num_output_args[opcode]
end

-- a "class"
IntComputer = {}

    function IntComputer:new(phase_setting, mem)
        newObj = { 
            phase = phase_setting,           -- user-specified ∈[0,4]
            program_ended = false,
            pc = nil,                        -- program counter
            memory = mem,
        }
        self.__index = self
        return setmetatable(newObj, self)
    end

    -- get the value from location   if accessMode = 00
    -- return the same value         if accessMode = 01
    function IntComputer:get_value(location_or_value, accessMode)
        if accessMode == 00 then return self.memory[location_or_value]
        elseif accessMode == 01 then return location_or_value
        else return nil
        end
    end

-- end IntComputer "class"

return M
