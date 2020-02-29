<?php
function instr_num_input_args($opcode) {
    switch ($opcode) {
        case 01:
            return 2;
        case 02:
            return 2;
        case 03:
            return 0;
        case 04:
            return 1;
        case 99:
            return 0;
        default: // shouldn't happen
            return 0;
    }
}

function instr_num_output_args($opcode) {
    switch ($opcode) {
        case 01:
            return 1;
        case 02:
            return 1;
        case 03:
            return 1;
        case 04:
            return 0;
        case 99:
            return 0;
        default: // shouldn't happen
            return 0;
    }
}

function instr_num_args($opcode) {
    return instr_num_input_args($opcode) + instr_num_output_args($opcode);
}

// read input into int array
$inputfile = fopen("./input.txt", "r");
$inputarr = fgetcsv($inputfile);
$intputarr = array_map('intval', $inputarr);

// test with first instruction
$instr = $intputarr[0];
$lenn = instr_num_args($instr);
echo("instruction: ".$instr."  numArgs: ".$lenn);
?>

</br><div>
    Something is right with the XAMPP installation :-(
</div>
