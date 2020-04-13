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

return M
