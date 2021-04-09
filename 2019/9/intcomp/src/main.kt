import java.io.File

fun main(args: Array<String>) {
    println("hello")
    var ic = IntComputer.from_file("../input.txt", listOf(1))
}

class FuckupException(msg:String): Exception(msg)

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

        fun from_file(filename: String, user_input: List<Int>): IntComputer {
            val initial_memory = File(filename).readText().split(',').map { s -> s.trim() }
            return IntComputer(initial_memory, user_input)
        }
    }

    var pc: Long = 0
    var relative_base: Long = 0
    var program_finished: Boolean = false

    // mapping addresses to memory entries
    val mem: MutableMap<Long, Long> = mutableMapOf<Long, Long>()
    var user_input: List<Int> = listOf()
    constructor(initial_memory: List<String>, user_input: List<Int>) : this() {
        // transform Char Array into Int map
        for ((i, chars) in initial_memory.withIndex()){
            this.mem[i.toLong()] = chars.toLong()
        }

        this.user_input = user_input
    }

    fun run(){
        this.pc = 0
        this.relative_base = 0

        while (!this.program_finished) {
            // 2-digit opcode, leading digits are accessmodes

        }
    }

    /**
     * get the value from location    if accessmode = 00
     * return the same value          if accessmode = 01
     */
    fun get_value(location_or_value: Long, accessMode :Int): Long {
        return when (accessMode){
            0 -> this.mem.getOrDefault(location_or_value, 0L)
            1 -> location_or_value
            else -> throw FuckupException("get_value called with accessMode $accessMode")
        }
    }

    fun set_value(location_or_value: Long, value: Long, accessMode: Int){
        when (accessMode){
            0 -> this.mem[location_or_value] = value
            1 -> throw FuckupException("can't set to immediate!")
            else -> throw FuckupException("what?")
        }
    }


}