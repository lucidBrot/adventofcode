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
local IntComputer = {}

    function IntComputer:new(phase_setting, mem, live_inputs)
        -- make sure the memory is sane
        for i = 1, #mem do
            mem[i] = tonumber(mem[i])
        end

        newObj = { 
            phase = phase_setting,                   -- user-specified ∈[0,4]
            program_ended = false,
            pc = nil,                                -- program counter, counts from 1
            memory = mem or {},                      -- array that stores memory
            stdin = live_inputs or {},               -- array that contains all user inputs
            stdin_counter = 0,
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
        else 
            print("YOU FUCKED UP!")
            return nil
        end
    end

    function IntComputer:set_value(location, value)
        -- because lua starts counting from 1 instead of 0, we need to shift by 1
        self.memory[location + 1] = value
        print("    set memory[" .. (location+1) .. "] to " .. value )
    end

    function IntComputer:run()
        self.pc = 1
        self.program_ended = false
        repeat
            print("")
            print("--- pc:" .. self.pc .. " ---")
            -- 2-digit opcode, leading digits are accessModes
            local opcodeWithAccessModesAsNumber = self.memory[self.pc]
            -- turn this into a string and an opcode
            opcode = opcodeWithAccessModesAsNumber % 100
            print("<amp" .. self.phase .. "> opcode as Number: " .. opcode)
            accessModes = tostring(math.floor(opcodeWithAccessModesAsNumber / 100))
            -- the access modes can be fetched using tonumber(accessModes:sub(i,i)) for the ith position
            n = M.instr_num_args(opcode)
            
            -- execute instruction
            local args = { table.unpack(self.memory, self.pc + 1, self.pc + n) }
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

        local inputargs = { table.unpack(args, 1, ni) }
        local outputargs = { table.unpack(args, ni+1) }

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

        print("<pc:" .. self.pc .. "> [" .. opcode .. "]  " .. table.concat(valargs, ", "))

        -- call the relevant execution
        local instructions = {
            [01] = function () self:perform_add(table.unpack(valargs)) end,
            [02] = function () self:perform_multiply(table.unpack(valargs)) end,
            [03] = function () self:perform_store_input(outputargs[1]) end,
            [04] = function () self:perform_output(valargs[1]) end,
            [05] = function () self:perform_jnz(table.unpack(valargs)) end,
            [06] = function () self:perform_jz(table.unpack(valargs)) end,
            [07] = function () self:perform_less_than(table.unpack(valargs)) end,
            [08] = function () self:perform_equals(table.unpack(valargs)) end,
            [99] = function () self:perform_exit() end,
        }
        local instr = instructions[opcode]
        if (instr) then
            instr()
        else
            print("No instruction for opcode " .. opcode)
        end

    end

    function IntComputer:perform_exit()
        self.program_ended = true
    end

    function IntComputer:perform_add(a, b, target)
        print("    " .. a .. " + " .. b .. " => " .. target)
        self:set_value(target, a+b)
    end

    function IntComputer:perform_multiply(a, b, target)
        self:set_value(target, a*b)
    end

    function IntComputer:perform_store_input(target)
        self.stdin_counter = self.stdin_counter + 1 
        local inp = nil
        if self.stdin_counter > #self.stdin then
            inp = io.read("*n") -- read a number from the user
        else
            inp = self.stdin[self.stdin_counter]
        end
        self:set_value(target, inp)
    end

    function IntComputer:perform_output(something)
        print("<amp" .. self.phase .. " out> " .. tostring(something))
        self.latest_output = something
    end

    function IntComputer:perform_jnz(conditional, target_pc)
        -- the pc will be increased before the next instruction, so we set it lower than told to
        -- and since it will be increased by (1+num_args) we need to subtract 3 here
        --
        -- BUT because lua counts from 1, our pc counts also from 1 but the intcode thinks it counts from 0.
        -- So we need to add 1
        if conditional ~= 0 then
            self.pc = target_pc - 3 + 1
            print("    jnz: " .. conditional .. " is not 0.  => " .. self.pc)
        end
    end

    function IntComputer:perform_jz(conditional, target_pc)
        if conditional == 0 then
            self.pc = target_pc -3 +1
        end
    end

    function IntComputer:perform_less_than(sm, lg, target)
        local v = T(sm < lg, 1, 0)
        self:set_value(target, v)
    end

    function IntComputer:perform_equals(a, b, target)
        self:set_value(target, T(a == b, 1, 0))
    end

    function IntComputer:get_latest_output()
        return self.latest_output
    end

-- end IntComputer "class"

return IntComputer
