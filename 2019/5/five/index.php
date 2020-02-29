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

class IntComputer {
    protected $memory;

    function __construct($memory){
        $this->memory = $memory;
    }

    function get_value($location_or_value, $accessMode){
        switch ($accessMode) {
        case 00:
            return $this->$memory[$location_or_value];
        case 01:
            return $location_or_value;
        }
    }

    function perform_instruction($opcode, $accessModes, ...$args) {
        // get input args
        $no = instr_num_output_args($opcode);
        $ni = instr_num_input_args($opcode);
        $inputargs = array_slice($args, 0, $ni);
        $outputargs = array_slice($args, $ni, $no);

        // pad accessModes with leading zeros
        $acc = array_pad($accessModes, -1 * $n, 0);

        // get input argument values
        $vals = [];
        for ($i = 0; $i < $ni; $i++){
            $vals += $this->get_value($inputargs[$i], $acc[$i]);
        }

        // combine arguments
        $valargs = $vals + $outputargs;

        // call the relevant execution
        switch ($opcode) {
            // the output arguments are, by the specs, always locations and not immediate values.
        case 01:
            perform_add(...$valargs);
            break;
        case 02:
            perform_multiply(...$valargs);
            break;
        case 03:
            perform_store_input($outputargs[0]);
            break;
        case 04:
            perform_output($inputargs[0])
        case 99:
            perform_exit(); // TODO: how to stop?
            break;

            

        }
    }
}

// read input into int array
$inputfile = fopen("./input.txt", "r");
$inputarr = fgetcsv($inputfile);
$intputarr = array_map('intval', $inputarr);

// test with first instruction
$instr = $intputarr[0];
$lenn = instr_num_args($instr);
echo("instruction: ".$instr."  numArgs: ".$lenn);

// do actual thing
$comp = new IntComputer($intputarr);
?>

</br><div>
    Something is right with the XAMPP installation :-(
</div>
