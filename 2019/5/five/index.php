<?php
class IntCodeProgram {
    public $commands;
}

class Intstruction {
    public $raw; // all digits, split on commas

    function get_size(){
        return count($this->raw, COUNT_NORMAL);
    }

    function __construct($raw) {
        $this->raw = $raw;
    }
}


$inst_1 = new Intstruction(array(1,2,3,0));
var_dump($inst_1);

$inputfile = fopen("./input.txt", "r");
$inputarr = fgetcsv($inputfile);
$intputarr = array_map('intval', $inputarr);
var_dump($intputarr);
?>

</br><div>
    Something is right with the XAMPP installation :-(
</div>
