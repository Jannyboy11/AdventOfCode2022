package day10

import scala.io.Source
import scala.collection.immutable.{SortedMap, SortedSet}
import Instruction.*
import Signals.*
import Pixels.*

import scala.annotation.tailrec

val source = Source.fromResource("day10.in")
val input: List[Instruction] = source.getLines().map {
    case "noop" => Noop
    case s"addx $amount" => Addx(amount.toInt)
}.toList

enum Instruction:
    case Noop
    case Addx(amount: Int)

case class CPU(X: Int = 1)

type Cycle = Int

def step(instruction: Instruction, cpu: CPU, cycle: Cycle): (Cycle, CPU) = (cpu, instruction) match
    case (_, Noop) => (cycle + 1, cpu)
    case (CPU(x), Addx(amount)) => (cycle + 2, CPU(x + amount))

object Signals:
    opaque type Signal = Int
    object Signal:
        inline def apply(x: Int): Signal = x
    extension (x: Signal)
        inline def toInt: Int = x

trait Aggregator[A, B]:
    def aggregate(aggregated: B, newValue: A): B
    def produce(cpu: CPU, cycle: Cycle, aggregated: B): A
    final def next(cpu: CPU, cycle: Cycle, aggregated: B): B = aggregate(aggregated, produce(cpu, cycle, aggregated))

given sumSignal: Aggregator[Signal, Int] = new Aggregator[Signal, Int]:
    override def aggregate(sum: Int, signal: Signal): Int = sum + signal.toInt
    override def produce(cpu: CPU, cycle: Cycle, sum: Int): Signal = Signal(cpu.X * cycle)

@tailrec
def go[A, Agg](program: List[Instruction], cycle: Cycle, currentCpu: CPU, cpuQueue: SortedMap[Cycle, CPU], checkpoints: Iterable[Cycle], acc: Agg)(using aggregator: Aggregator[A, Agg]): Agg =
    if program.nonEmpty || cpuQueue.nonEmpty then
        val (nextCheckPoints, nextOutputs) = if checkpoints.nonEmpty && checkpoints.head == cycle then
            (checkpoints.tail, aggregator.next(currentCpu, cycle, acc)) else (checkpoints, acc)

        val (nextUpdateIndex, theNextCpu) = cpuQueue.head
        val (nextProgram, nextQueue, nextCpu) =
            if cycle == nextUpdateIndex then
                if program.nonEmpty then (program.tail, cpuQueue.tail + step(program.head, theNextCpu, cycle), theNextCpu) else (program, cpuQueue.tail, theNextCpu)
            else
                (program, cpuQueue, currentCpu)

        go(nextProgram, cycle + 1, nextCpu, nextQueue, nextCheckPoints, nextOutputs)
    else acc

object Pixels:
    type On = '#'
    type Off = '.'
    opaque type Pixel = On | Off
    object Pixel:
        inline def apply(c: On | Off): Pixel = c

class CRT(val width: Int, rows: Seq[Seq[Pixel]] = Seq(Seq())):
    def beam(pixel: Pixel): CRT =
        if rows.isEmpty || rows.head.isEmpty then
            CRT(width, Seq(Seq(pixel)))
        else
            val lastRowIdx = rows.size - 1
            val pixels = rows(lastRowIdx)
            CRT(width, if pixels.size < width then rows.updated(lastRowIdx, pixels :+ pixel) else rows :+ Seq(pixel))
    def print(): Unit = rows.foreach(row => println(row.mkString))

given pixelAggregator: Aggregator[Pixel, CRT] = new Aggregator[Pixel, CRT]:
    override def aggregate(crt: CRT, pixel: Pixel): CRT = crt.beam(pixel)
    override def produce(cpu: CPU, cycle: Cycle, crt: CRT): Pixel = Pixel(if Math.abs((cycle-1) % crt.width - cpu.X) <= 1 then '#' else '.')

@main def main: Unit = {

    val initCycle: Cycle = 0
    val initCpu: CPU = CPU()
    val initQueue = SortedMap((initCycle, initCpu))

    val result1 = go[Signal, Int](input, initCycle, initCpu, initQueue,Seq(20, 60, 100, 140, 180, 220), 0)
    println(result1)

    go[Pixel, CRT](input, initCycle, initCpu, initQueue, Range.inclusive(1, 240), CRT(40)).print()
    //result2 = RKAZAJBR

}