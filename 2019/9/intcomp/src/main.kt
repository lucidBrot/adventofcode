import java.io.File

fun main(args: Array<String>) {
    println("hello")
    val name = readLine()
    println("Hello $name")
    var ic = IntComputer.from_file("p../input.txt")
}

class IntComputer() {

    companion object {
        // a table that gives you the number of input arguments for the opcode
        val instr_num_input_args: Map<Int, Int> = mapOf(
            1 to 2,
            2 to 2,
            3 to 0,
            4 to 1,
            5 to 2,
            6 to 2,
            7 to 2,
            8 to 2,
            9 to 1,
            99 to 0,
        )

        // a table that gives you the number of output arguments for the opcode
        val instr_num_output_args: Map<Int, Int> = mapOf(
            1 to 1,
            2 to 1,
            3 to 1,
            4 to 0,
            5 to 0,
            6 to 0,
            7 to 1,
            8 to 1,
            9 to 0,
            99 to 0,
        )

        /**
         * number of ALL arguments for opcode
         */
        fun instr_num_args(opcode: Int): Int {
            return instr_num_input_args[opcode]!! + instr_num_output_args[opcode]!!
        }

        fun from_file(filename: String): IntComputer {
            val initial_memory = File(filename).readText().toCharArray()
            return IntComputer(initial_memory)
        }
    }

    // mapping addresses to memory entries
    val mem: MutableMap<Int, Int> = mutableMapOf<Int, Int>()
    constructor(initial_memory: CharArray) : this() {
        // transform Char Array into Int map
        for ((i, char) in initial_memory.withIndex()){
            this.mem[i] = char.toInt()
        }

    }
}