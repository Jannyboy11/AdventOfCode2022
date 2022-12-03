package day03

import scala.io.Source;

val source = Source.fromResource("day03.in")
val input: List[Rucksack] = source.getLines().toList

type Rucksack = String
type Item = Char

object Priority:
    def unapply(item: Item): Priority = new Priority(item)

class Priority(item: Item):
    private val start: Int =
        if ('a' <= item && item <= 'z') then 1 - 'a'.toInt
        else if ('A' <= item && item <= 'Z') then 27 - 'A'.toInt
        else Int.MinValue
    def isEmpty: Boolean = start == Int.MinValue
    def get: Int = start + item.toInt

def priority(item: Item): Int = item match
    case Priority(priority) => priority

def overlappingItem(rucksack: Rucksack): Item = rucksack.splitAt(rucksack.length / 2) match
    case (one, two) => one.toSet.intersect(two.toSet).head

def overlappingItem(one: Rucksack, two: Rucksack, three: Rucksack): Item =
    one.toSet.intersect(two.toSet).intersect(three.toSet).head

@main def main: Unit = {

    val result1 = input.map(overlappingItem).map(priority).sum
    println(result1)

    val result2 = input.grouped(3).map { case List(one, two, three) => overlappingItem(one, two, three) }.map(priority).sum
    println(result2)

}