package day04

import scala.io.Source;

val source = Source.fromResource("day04.in")
val input: List[(Range, Range)] = source.getLines().map { case s"$a-$b,$c-$d" => (a.toInt to b.toInt, c.toInt to d.toInt) }.toList

def fullyContained(a: Range, b: Range): Boolean =
    (a.start <= b.start && b.end <= a.end) ||
    (b.start <= a.start && a.end <= b.end)

def overlap(a: Range, b: Range): Boolean =
    a.contains(b.start) || a.contains(b.end) || b.contains(a.start) || b.contains(a.end)

@main def main: Unit = {

    val result1 = input.count(fullyContained.tupled)
    println(result1)

    val result2 = input.count(overlap.tupled)
    println(result2)

}