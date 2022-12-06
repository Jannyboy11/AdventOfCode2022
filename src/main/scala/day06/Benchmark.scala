package day06

def warmup(): Unit =
    for i <- 0 until 1000 do
        o_main
        bb_main

@main def naive_benchmark: Unit = {
    warmup()

    val oldTime = System.nanoTime()
    o_main
    val midTime = System.nanoTime()
    bb_main
    val newTime = System.nanoTime()

    val mySolutionNanos = midTime - oldTime
    val bbSolutionNanos = newTime - midTime

    println(s"Jan's optimised solution took $mySolutionNanos nanoseconds")
    println(s"Bishabosha's optimised solution took $bbSolutionNanos nanoseconds")
    println(s"${if mySolutionNanos < bbSolutionNanos then "Jan" else "Bishabosha"} wins!")
}