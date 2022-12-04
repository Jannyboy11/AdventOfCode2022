package day01

import scala.io.Source;

val source = Source.fromResource("day01.in")
val calories: IndexedSeq[Iterable[Int]] = {
    val input = source.getLines().toIndexedSeq
    val resultBuilder = IndexedSeq.newBuilder[Iterable[Int]]

    var i = 0
    var listBuilder = List.newBuilder[Int]
    while i < input.size do
        val string = input(i)
        if string.isEmpty then
            resultBuilder.addOne(listBuilder.result())
            listBuilder = List.newBuilder[Int]
        else
            listBuilder.addOne(string.toInt)

        i += 1

    resultBuilder.result()
}


@main def main: Unit = {

    val result1 = calories.map(_.sum).max
    println(result1)

    val result2 = calories.map(_.sum).foldLeft(TopThree())(_.tryInsert(_)).sum
    println(result2)

}

class TopThree(one: Int, two: Int, three: Int):
    def this() = this(0, 0, 0)

    def tryInsert(x: Int): TopThree = {
        var min = one
        var slot : 1 | 2 | 3 = 1

        if two < min then
            min = two
            slot = 2

        if three < min then
            min = three
            slot = 3

        if x > min then
            slot match
                case 1 => TopThree(x, two, three)
                case 2 => TopThree(one, x, three)
                case 3 => TopThree(one, two, x)
        else
            this
    }

    def sum: Int = one + two + three

