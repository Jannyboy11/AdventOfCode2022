package day06

def warmup(): Unit =
    for i <- 0 until 1000 do
        o_main
        bb_main
        Day06Loops.main(null)

@main def naive_benchmark: Unit = {
    warmup()

    val iterations = 100
    var i = 0

    var Jan: Long = 0
    var Bishabosha: Long = 0
    var JavadocMD: Long = 0

    while i < iterations do
        val beginJan = System.nanoTime()
        o_main
        val beginBishabosha = System.nanoTime()
        bb_main
        val beginJavadocMD = System.nanoTime()
        Day06Loops.main(null)
        val endTime = System.nanoTime()

        Jan += beginBishabosha - beginJan
        Bishabosha += beginJavadocMD - beginBishabosha
        JavadocMD += endTime - beginJavadocMD

        i += 1

    val mySolutionNanos = Jan / iterations
    val bbSolutionNanos = Bishabosha / iterations
    val jdmdSolutionNanos = JavadocMD / iterations

    println(s"Jan's optimised solution took $mySolutionNanos nanoseconds on average.")
    println(s"Bishabosha's optimised solution took $bbSolutionNanos nanoseconds on average.")
    println(s"JavadocMD's optimised solution took $jdmdSolutionNanos nanoseconds on average.")
    println(s"${if mySolutionNanos < bbSolutionNanos then "Jan" else "Bishabosha"} wins!")

}