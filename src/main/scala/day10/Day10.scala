package day10

import scala.io.Source
import scala.collection.immutable.{SortedMap, SortedSet}
import Instruction.*

import scala.annotation.tailrec

val source = Source.fromResource("day10.in")
val input: List[Instruction] = source.getLines().map {
    case "noop" => Noop
    case s"addx $amount" => Addx(amount.toInt)
}.toList

type XPosition = Int
type Signal = Int
type Cycle = Int
type Output[A] = (CPU, Cycle) => A

enum Instruction:
    case Noop
    case Addx(amount: Signal)

case class CPU(X: XPosition = 1)

def step(instruction: Instruction, cpu: CPU, cycle: Cycle): (Cycle, CPU) = (cpu, instruction) match
    case (_, Noop) => (cycle + 1, cpu)
    case (CPU(x), Addx(amount)) => (cycle + 2, CPU(x + amount))

def signalStrength(cpu: CPU, cycle: Cycle): Signal = cpu.X * cycle

@tailrec
def go[A](program: List[Instruction], cycle: Cycle, currentCpu: CPU, cpuQueue: SortedMap[Cycle, CPU], checkpoints: Iterable[Cycle], output: Output[A], acc: Seq[A]): Seq[A] =
    if program.isEmpty then acc else
        val (nextCheckPoints, nextSignals) = if checkpoints.nonEmpty && checkpoints.head == cycle then
            (checkpoints.tail, acc :+ output(currentCpu, cycle)) else (checkpoints, acc)

        val (queueHead, nextCpu) = cpuQueue.head
        if cycle == queueHead then
            go(program.tail, cycle + 1, nextCpu, cpuQueue.tail + step(program.head, nextCpu, cycle), nextCheckPoints, output, nextSignals)
        else
            go(program, cycle + 1, currentCpu, cpuQueue, nextCheckPoints, output, nextSignals)

type On = '#'
type Off = '.'
type Pixel = On | Off

def draw(cpu: CPU, cycle: Cycle, width: Int): Pixel = if Math.abs((cycle-1) % width - cpu.X) <= 1 then '#' else '.'

@main def main: Unit = {

    val initCycle: Cycle = 0
    val initCpu: CPU = CPU()
    val initQueue = SortedMap((initCycle, initCpu))

    val result1 = go[Signal](input, initCycle, initCpu, initQueue,Seq(20, 60, 100, 140, 180, 220), signalStrength, Seq()).sum
    println(result1)

    val width = 40
    go[Pixel](input, initCycle, initCpu, initQueue, Range.inclusive(0, 240), draw(_, _, width), Seq())
        .grouped(width)
        .foreach(row => println(row.mkString))
    //result2 = RKAZAJBR

}