import java.io.File
// This file is basically a port from lua to kotlin, form day 7

fun main(args: Array<String>) {
    println("hello")
    var ic = IntComputer.from_file("../input.txt", listOf(1))
    ic.run()
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
    // the opcode and accesscode are one number, the args are in future elements.
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
            var opcodeWithAccessmodesAsNumber = this.mem.getOrDefault(pc, 0)
            // turn this into the opcode
            var opcode = opcodeWithAccessmodesAsNumber % 100
            // compute the access modes for each argument
            var accessModes = (opcodeWithAccessmodesAsNumber / 100L).toString().toCharArray()
                .also { it.reverse() }
                .map { c -> c.toLong() }
                .toMutableList()
            // access modes can be fetched using indices. It is now in correct order.

            var n = instr_num_args(opcode = opcode.toInt())

            // pad access modes because leading zeros have been lost
            while (accessModes.size < n){
                accessModes.add(0L)
            }

            // get all arguments
            var args = MutableList<Long>(n) { i ->
                this.mem.getOrDefault(this.pc + 1 + i, 0L)
            }

            // execute instruction
            this.perform_instruction(opcode.toInt(), accessModes, args)

            // increment program counter
            this.pc += 1 + n
        }
    }

    fun perform_instruction(opcode: Int, accessModes: List<Long>, args: List<Long>){
        var n = instr_num_args(opcode)
        assert(n == args.size) { "Wrong number of Arguments"}
        var ni = instr_num_input_args.getOrDefault(opcode, -1)
        var inputargs = args.subList(0,ni)
        var outputargs = args.subList(ni+1, args.size)

        assert(n==accessModes.size){ "Wrong length of AccessModes."}
        // get input argument values
        var inputvals = inputargs.mapIndexed { index, elem ->
            get_value(elem, accessModes[index].toInt())
        }

        // get output values. They should always be locations
        var outputvals = outputargs.mapIndexed {index, elem ->
            assert(accessModes[index] != 1L){"Output values can never be immediates"}
            get_value(elem, accessModes[index].toInt())
        }

        // combine the arguments into one list
        var args: List<Long> = mutableListOf<Long>().apply{addAll(inputvals)}.apply{addAll(outputvals)}
        // call the relevant execution
        when (opcode){
            1 -> this.perform_add(args[0], args[1], args[2])
            2 -> this.perform_multiply(args[0], args[1], args[2])
            3 -> this.perform_store_input(args[0])
            4 -> this.perform_output(args[0])
            5 -> this.perform_jnz(args[0], args[1])
            6 -> this.perform_jz(args[0], args[1])
            7 -> this.perform_less_than(args[0],args[1], args[2])
            8 -> this.perform_equals(args[0], args[1], args[2])
            9 -> this.perform_add_to_relative_base(args[0])
            99-> this.perform_exit()
            else -> throw FuckupException("WHAT THE FUCK DID YOU JUST TELL ME TO DO?!")
        }
    }

    /**
     * get the value from location    if accessmode = 00
     * return the same value          if accessmode = 01
     * get value from location+relbase if acessmode = 02
     */
    fun get_value(location_or_value: Long, accessMode :Int): Long {
        return when (accessMode){
            0 -> this.mem.getOrDefault(location_or_value, 0L)
            1 -> location_or_value
            2 -> this.mem.getOrDefault(location_or_value+this.relative_base, 0)
            else -> throw FuckupException("get_value called with accessMode $accessMode")
        }
    }

    fun set_value(location_or_value: Long, value: Long){
        this.mem[location_or_value] = value
    }

    fun perform_exit(){
        this.program_finished = true
    }

    fun perform_add(a:Long, b:Long, target:Long){
        this.set_value(target, a+b)
    }

    fun perform_multiply(a:Long, b:Long, target:Long){
        this.set_value(target, a*b)
    }

    fun perform_store_input(target:Long){
        print("input: ")
        var input = readLine()!!.trim().toLong()
        this.set_value(target, input)
    }

    fun perform_output(something:Long){
        print("output: $something")
    }

    fun perform_jz(cond:Long, target_pc:Long){
        // the pc will be increased before the next instruction, so we set it lower than told to
        // and since it will be increased by (1+num_args) we need to subtract 3 here
        if (cond == 0L) {
            this.pc = target_pc -3
        }
    }

    fun perform_jnz(cond:Long, target_pc:Long){
        if (cond != 0L){
            this.pc = target_pc -3
        }
    }

    fun perform_less_than(sm:Long, lg:Long, target:Long){
        var l : Long = (if (sm < lg) 1L else 0L)
        this.set_value(target, l)
    }

    fun perform_equals(a:Long, b:Long, target:Long){
        this.set_value(target, if (a==b) 1L else 0L)
    }

    fun perform_add_to_relative_base(v:Long){
        this.relative_base += v
    }

}