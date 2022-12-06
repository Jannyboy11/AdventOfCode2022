package day06

def warmup(): Unit =
    for i <- 0 until 1000 do
        o_main
        bb_main

@main def naive_benchmark: Unit = {
    warmup()

    val iterations = 100
    var i = 0

    var Jan: Long = 0
    var Bishabosha: Long = 0

    while i < iterations do
        val oldTime = System.nanoTime()
        o_main
        val midTime = System.nanoTime()
        bb_main
        val newTime = System.nanoTime()

        Jan += midTime - oldTime
        Bishabosha += newTime - midTime

        i += 1

    val mySolutionNanos = Jan / iterations
    val bbSolutionNanos = Bishabosha / iterations

    println(s"Jan's optimised solution took $mySolutionNanos nanoseconds on average")
    println(s"Bishabosha's optimised solution took $bbSolutionNanos nanoseconds on average")
    println(s"${if mySolutionNanos < bbSolutionNanos then "Jan" else "Bishabosha"} wins!")
    
}