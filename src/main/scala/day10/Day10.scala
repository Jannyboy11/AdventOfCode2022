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
def go[A](program: List[Instruction], cycle: Cycle, currentCpu: CPU, cpuQueue: SortedMap[Cycle, CPU], checkpoints: Iterable[Cycle], outputFunction: Output[A], outputValuesAcc: Seq[A]): Seq[A] =
    if program.nonEmpty || cpuQueue.nonEmpty then

        val (nextCheckPoints, nextOutputs) = if checkpoints.nonEmpty && checkpoints.head == cycle then
            (checkpoints.tail, outputValuesAcc :+ outputFunction(currentCpu, cycle)) else (checkpoints, outputValuesAcc)

        val (nextUpdateIndex, theNextCpu) = cpuQueue.head
        val (nextProgram, nextQueue, nextCpu) =
            if cycle == nextUpdateIndex then
                if program.nonEmpty then (program.tail, cpuQueue.tail + step(program.head, theNextCpu, cycle), theNextCpu) else (program, cpuQueue.tail, theNextCpu)
            else
                (program, cpuQueue, currentCpu)

        go(nextProgram, cycle + 1, nextCpu, nextQueue, nextCheckPoints, outputFunction, nextOutputs)

    else outputValuesAcc

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
    go[Pixel](input, initCycle, initCpu, initQueue, Range.inclusive(1, 240), draw(_, _, width), Seq())
        .grouped(width)
        .foreach(row => println(row.mkString))
    //result2 = RKAZAJBR

}