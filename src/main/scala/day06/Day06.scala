package day06

import scala.io.Source;

val source = Source.fromResource("day06.in")
val input = source.mkString

def findMarker(input: String, consecutiveDistinct: Int): Int =
    input.sliding(consecutiveDistinct).indexWhere(_.toSet.size == consecutiveDistinct) + consecutiveDistinct

@main def main: Unit = {

    val result1 = findMarker(input, 4)
    println(result1)

    val result2 = findMarker(input, 14)
    println(result2)

}