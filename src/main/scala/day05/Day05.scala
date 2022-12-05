package day05

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

val source = Source.fromResource("day05.in"); val NumStacks = 9
val input = source.getLines().toList

type Crate = Char
type Stack = List[Crate] //head = top of the stack
type Dock = IndexedSeq[Stack]

val (dock: Dock, moves: List[Move]) = {
    val d: IndexedSeq[ListBuffer[Crate]] = IndexedSeq.fill(NumStacks)(new ListBuffer[Crate]())
    val moves = List.newBuilder[Move]

    input.foreach { line => line match
        case s if s.nonEmpty && (s.charAt(0) == '[' || s.charAt(0) == ' ') =>
            for i <- 0 until NumStacks do
                val idx = i * 4 + 1
                if idx <= s.length then
                    val c = s.charAt(idx)
                    if c.isLetter then
                        d(i).addOne(c)

        case s"move $count from $from to $to" =>
            moves.addOne(Move(count.toInt, from.toInt - 1, to.toInt - 1))

        case _ =>
    }

    (d.map(_.result()), moves.result())
}

case class Move(count: Int, from: Int, to: Int)

inline def moveN(stackFrom: Stack, stackTo: Stack, amount: Int, inline reverse: Boolean): (Stack, Stack) =
    val (crates, newFrom) = stackFrom.splitAt(amount)
    val newTo = inline if reverse then stackTo.reverse_:::(crates) else crates ++ stackTo
    (newFrom, newTo)

def interpret1(dock: Dock, move: Move): Dock =
    val (stackFrom, stackTo) = moveN(dock(move.from), dock(move.to), move.count, true)
    dock.updated(move.from, stackFrom).updated(move.to, stackTo)

def interpret2(dock: Dock, move: Move): Dock =
    val (stackFrom, stackTo) = moveN(dock(move.from), dock(move.to), move.count, false)
    dock.updated(move.from, stackFrom).updated(move.to, stackTo)

@main def main: Unit = {

    val result1 = moves.foldLeft(dock)(interpret1).map(_.head).mkString
    println(result1)

    val result2 = moves.foldLeft(dock)(interpret2).map(_.head).mkString
    println(result2)

}