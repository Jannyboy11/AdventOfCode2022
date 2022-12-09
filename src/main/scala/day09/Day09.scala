package day09

import scala.io.Source
import Direction.*

import scala.annotation.tailrec

val source = Source.fromResource("day09.in")
val input: List[Command] = source.getLines().map {
    case s"L $count" => Command(Left, count.toInt)
    case s"R $count" => Command(Right, count.toInt)
    case s"U $count" => Command(Up, count.toInt)
    case s"D $count" => Command(Down, count.toInt)
}.toList

enum Direction:
    case Left, Right, Up, Down

case class Command(direction: Direction, distance: Int)

case class Point(x: Int, y: Int)
case class Rope(head: Point, tail: Point)

def processTail(head: Point, tail: Point): Point =
    val updatedX = tail.x + Integer.compare(head.x, tail.x)
    val updatedY = tail.y + Integer.compare(head.y, tail.y)

    val xDiff = Math.abs(head.x - tail.x)
    val yDiff = Math.abs(head.y - tail.y)

    var newX = if xDiff > 1 then updatedX else tail.x
    var newY = if yDiff > 1 then updatedY else tail.y

    if xDiff + yDiff > 2 then
        newX = updatedX
        newY = updatedY

    Point(newX, newY)

def processHead(head: Point, direction: Direction): Point = direction match
    case Left => head.copy(x = head.x - 1)
    case Right => head.copy(x = head.x + 1)
    case Up => head.copy(y = head.y - 1)
    case Down => head.copy(y = head.y + 1)

@tailrec
def simulate1(acc: List[Rope], command: Command): List[Rope] =
    command match
        case Command(_, 0) => acc
        case Command(direction, amount) =>
            val Rope(head, tail) = acc.head
            val newHead: Point = processHead(head, direction)
            val newTail = processTail(newHead, tail)
            simulate1(Rope(newHead, newTail) :: acc, Command(direction, amount-1))

type Rope2 = List[Point]
def processTail2(rope: Rope2): Rope2 =
    rope.zip(rope.tail).map(processTail)

@tailrec
def simulate2(acc: List[Rope2], command: Command): List[Rope2] =
    command match
        case Command(_, 0) => acc
        case Command(direction, amount) =>
            val front = acc.head
            val newHead: Point = processHead(front.head, direction)
            val newTail = processTail2(front)
            simulate2((newHead :: newTail) :: acc, Command(direction, amount-1))

@main def main: Unit = {

    val zero = Point(0, 0)

    val result1 = {
        val initRope = Rope(zero, zero)
        val ropes = input.foldLeft[List[Rope]](List(initRope))(simulate1)
        ropes.map(_.tail).toSet.size
    }
    println(result1)

    val result2 = {
        val initRope: Rope2 = List.fill(10)(zero)
        val ropes = input.foldLeft[List[Rope2]](List(initRope))(simulate2)
        ropes.map(_.last).toSet.size
    }
    println(result2)

}