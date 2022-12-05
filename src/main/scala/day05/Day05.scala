package day05

import scala.collection.mutable.ListBuffer
import scala.io.Source

val source = Source.fromResource("day05.in"); val NumStacks = 9
val input = source.getLines().toList

type Crate = Char
type Stack = List[Crate] //head = top of the stack
type Dock = IndexedSeq[Stack]
case class Move(count: Int, from: Int, to: Int)

val (dock: Dock, moves: List[Move]) = {
    val d: IndexedSeq[ListBuffer[Crate]] = IndexedSeq.fill(NumStacks)(new ListBuffer[Crate]())
    val moves = List.newBuilder[Move]

    input.foreach { line => line match
        case s if s.nonEmpty && (s.charAt(0) == '[' || s.charAt(0) == ' ') =>
            for i <- 0 until NumStacks do
                val idx = i * 4 + 1
                if idx < s.length then
                    val c = s.charAt(idx)
                    if c.isLetter then
                        d(i).addOne(c)

        case s"move $count from $from to $to" =>
            moves.addOne(Move(count.toInt, from.toInt - 1, to.toInt - 1))

        case _ =>
    }

    (d.map(_.result()), moves.result())
}

inline def moveN(stackFrom: Stack, stackTo: Stack, amount: Int, inline reverse: Boolean): (Stack, Stack) =
    val (crates, newFrom) = stackFrom.splitAt(amount)
    val newTo = inline if reverse then crates reverse_::: stackTo else crates ::: stackTo
    (newFrom, newTo)

inline def interpret(dock: Dock, move: Move, inline reversePoppedCrates: Boolean): Dock =
    val (stackFrom, stackTo) = moveN(dock(move.from), dock(move.to), move.count, reversePoppedCrates)
    dock.updated(move.from, stackFrom).updated(move.to, stackTo)

@main def main: Unit = {

    val result1 = moves.foldLeft(dock)(interpret(_, _, true)).map(_.head).mkString
    println(result1)

    val result2 = moves.foldLeft(dock)(interpret(_, _, false)).map(_.head).mkString
    println(result2)

}