package day03

import scala.io.Source;

val source = Source.fromResource("day03.in")
val input: List[Rucksack] = source.getLines().toList

type Rucksack = String
type Item = Char

object ItemRange:
    def unapply(item: Item): ItemRange =
        if 'a' <= item && item <= 'z' then ItemRange.Lower
        else if 'A' <= item && item <= 'Z' then ItemRange.Upper
        else ItemRange.OutOfBounds

enum ItemRange(base: Int):
    case Lower extends ItemRange(1 - 'a'.toInt)
    case Upper extends ItemRange(27 - 'A'.toInt)
    case OutOfBounds extends ItemRange(Int.MinValue)
    def isEmpty: Boolean = this == ItemRange.OutOfBounds
    def get: Int = base

def priority(item: Item): Int = item match
    case ItemRange(base) => base + item.toInt

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