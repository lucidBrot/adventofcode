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

-- ternary operator because the common
--   x = a and b or c
-- is spooky when b evaluates to false (and would have to be swapped then)
function T(condition, yes, no)
    if condition then return yes else return no end
end

-- a "class"
IntComputer = {}

    function IntComputer:new(phase_setting, mem)
        local m = T(mem == nil, {}, mem)
        newObj = { 
            phase = phase_setting,           -- user-specified ∈[0,4]
            program_ended = false,
            pc = nil,                        -- program counter
            memory = m,                    -- array that stores memory
        }
        self.__index = self
        return setmetatable(newObj, self)
    end

    -- get the value from location   if accessMode = 00
    -- return the same value         if accessMode = 01
    function IntComputer:get_value(location_or_value, accessMode)
        if accessMode == 00 then 
            -- because lua starts counting from 1 instead of 0, we need to shift by 1
            return self.memory[location_or_value + 1]
        elseif accessMode == 01 then return location_or_value
        else return nil
        end
    end

    function IntComputer:set_value(location, value)
        -- because lua starts counting from 1 instead of 0, we need to shift by 1
        self.memory[location + 1] = value
    end

    function IntComputer:run()
        self.pc = 1
        self.program_ended = false
        repeat
            print(" ")
            -- 2-digit opcode, leading digits are accessModes
            local opcodeWithAccessModesAsNumber = self.memory[self.pc]
            -- turn this into a string and an opcode
            opcode = opcodeWithAccessModesAsNumber % 100
            print("<amp" .. self.phase .. "> opcode as Number: " .. opcode)
            accessModes = tostring(math.floor(opcodeWithAccessModesAsNumber / 100))
            -- the access modes can be fetched using tonumber(accessModes:sub(i,i)) for the ith position
            n = M.instr_num_args(opcode)
            
            -- execute instruction
            local args = { table.unpack(self.memory, self.pc + 1, self.pc + n) } -- TODO: if something is off, the bug might be in this line
            -- print("<amp" .. self.phase .. "> about to execute opcode " .. opcode .. " with access modes " .. accessModes .. " and arguments " .. table.concat(args,", ") .. ".")
            for a=1, #args do
                print("    (arg): " .. T(args[a], args[a], "NIL"))
            end
            self:perform_instruction(opcode, accessModes, args)

            -- increment program counter
            self.pc = self.pc + 1 + n
        until self.program_ended
    end

    function IntComputer:perform_instruction(opcode, accessModes, args)
        -- get input args
        local no = M.instr_num_output_args[opcode]
        local ni = M.instr_num_input_args[opcode]
        local n = ni + no
        assert(n == #args, "Wrong number of arguments.")
        -- TODO: if something is off, the bug might be in the below two lines
        local inputargs = { table.unpack(args, 1, ni) }
        local outputargs = { table.unpack(args, no) }

        -- pad accessModes with leading zeros
        local temp = string.rep('0', n - #accessModes) .. accessModes
        -- then reverse because the rightmost accessor is for the leftmost parameter
        local acc = string.reverse(temp)
        print("  acc padded: " .. acc)

        -- get input argument values
        local vals = {}
        for i = 1, ni do
            local acci = tonumber(acc:sub(i,i))
            vals[i] = self:get_value(inputargs[i], acci)
            print("  loading inputarg " .. inputargs[i] .. " for accessor " .. acci .. " : that is " .. tostring(vals[i]) )
        end

        -- output arguments are always locations

        -- combine arguments
        local valargs = nil
        if #outputargs > 0 then
            valargs = {table.unpack(vals)} -- copy
            for i = 1, #outputargs do
                valargs[#vals+i] = outputargs[i]
            end
        else
            valargs = {table.unpack(vals)} -- copy
        end

        print("<pc:" .. self.pc .. "> [" .. opcode .. "]" .. table.unpack(valargs))

    end

-- end IntComputer "class"

return M
