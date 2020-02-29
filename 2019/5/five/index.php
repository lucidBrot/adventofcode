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
    protected $program_ended = false;

    function __construct($memory){
        $this->memory = $memory;
    }

    function dump_memory(){
        echo("<div>--- Mem Dump ---<br/>");
        var_dump($this->memory);
        echo("<br/>-----------------------</div>");
    }

    function get_value($location_or_value, $accessMode){
        switch ($accessMode) {
        case 00:
            return $this->memory[$location_or_value];
        case 01:
            return $location_or_value;
        }
    }

    function set_value($location, $value){
        $this->memory[$location] = $value;
    }

    function run(){
        $pc = 0;
        $this->program_ended=False;
        while (! $this->program_ended ){
            // 2-digit opcode, leading digits are accessModes
            $opcodeWithAccessModesAsNumber = $this->memory[$pc];
            echo("opcodeWithAccessModesAsNumber: ".$opcodeWithAccessModesAsNumber."<br/>");
            // turn this into an array and an opcode
            $opcode = $opcodeWithAccessModesAsNumber % 100;
            $accessModes = array_map('intval', str_split(intval($opcodeWithAccessModesAsNumber / 100)));

            // get number of args
            $n = instr_num_args($opcode);

            // execute instruction
            $args = array_slice($this->memory, $pc + 1, $n);
            echo("About to execute ".$opcode." with access modes ".implode(" ", $accessModes)." and arguments ".implode(" ", $args)."<br/>");
            $this->perform_instruction($opcode, $accessModes, ...$args);

            // increase program counter
            $pc = $pc + 1 + $n;
        }

    }

    // $opcode: int
    // $accessModes: array
    // $args: array
    function perform_instruction($opcode, $accessModes, ...$args) {
        // get input args
        $no = instr_num_output_args($opcode);
        $ni = instr_num_input_args($opcode);
        $n = $no + $ni;
        $inputargs = array_slice($args, 0, $ni);
        $outputargs = array_slice($args, $ni, $no);

        // pad accessModes with leading zeros
        $acc = array_pad($accessModes, -1 * $n, 0);

        // get input argument values
        $vals = [];
        for ($i = 0; $i < $ni; $i++){
            echo("trying to load inputarg ".$inputargs[$i]." for accessor ".$acc[$i]."<br/>");
            $vals[] = $this->get_value($inputargs[$i], $acc[$i]);
        }

        // combine arguments
        if(count($outputargs) > 0){
            $valargs = $vals; array_push($valargs, ...$outputargs);
        } else {
            $valargs = $vals;
        }

        echo("[".$opcode."] ".implode(" ", $valargs)."<br/>");

        // call the relevant execution
        switch ($opcode) {
            // the output arguments are, by the specs, always locations and not immediate values.
        case 01:
            $this->perform_add(...$valargs);
            break;
        case 02:
            $this->perform_multiply(...$valargs);
            break;
        case 03:
            $this->perform_store_input($outputargs[0]);
            break;
        case 04:
            $this->perform_output($valargs[0]);
            break;
        case 99:
        default:
            $this->perform_exit();
        break;

        }
    }

    function perform_exit(){
        $this->program_ended = true;
    }

    function perform_add($a, $b, $target){
        $this->set_value($target, $a+$b);
    }

    function perform_multiply($a, $b, $target) {
        $this->set_value($target, $a * $b);
    }
 
    function perform_store_input($target){
        // TODO: actually get input instead of hardcoded 1?
        $this->set_value($target, 1);
    }

    function perform_output($something){
        echo("output:: ".strval($something)."<br/>");
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

// testing
$comp = new IntComputer(array(01, 1, 2, 0));
$comp->dump_memory();
$comp->perform_instruction(01, [0,0], 1,2,0);
$comp->dump_memory();

// do actual thing
echo("<br/><br/>Actual execution:<br/>");
$comp = new IntComputer($intputarr);
$comp->run();
echo("<br/>DONE!<br/>");
$comp->dump_memory();
?>

</br><div>
    Something is right with the XAMPP installation :-(
</div>
