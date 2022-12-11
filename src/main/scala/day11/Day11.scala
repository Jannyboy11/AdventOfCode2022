package day11

import scala.io.Source
import Operation.*
import Operand.*

import scala.collection.immutable.Queue

val source = Source.fromResource("day11.in")
val input: IndexedSeq[Monkey] = {

    val monkeys = IndexedSeq.newBuilder[Monkey]

    var currentMonkeyId = -1
    var startingItems = Seq[Worry]()
    var operation: Operation = null
    var divisibleBy: Int = 0
    var ifTrue: Id = -1
    var ifFalse: Id = -1

    for (line <- source.getLines()) {
        line match
            case s"Monkey $id:" => currentMonkeyId = id.toInt
            case s"  Starting items: $items" => startingItems = items.split(", ").map(BigInt(_)).toIndexedSeq
            case s"  Operation: new = $op" =>
                val Array(left, opCode, right) = op.split(" ")
                val leftOperand = left match { case "old" => Old; case x => Literal(x.toInt) }
                val rightOperand = right match { case "old" => Old; case x => Literal(x.toInt) }
                operation = opCode match { case "*" => Multiply(leftOperand, rightOperand); case "+" => Add(leftOperand, rightOperand) }
            case s"  Test: divisible by $d" => divisibleBy = d.toInt
            case s"    If true: throw to monkey $x" => ifTrue = x.toInt
            case s"    If false: throw to monkey $y" => ifFalse = y.toInt
            case "" => monkeys.addOne(Monkey(currentMonkeyId, startingItems, operation, Test(divisibleBy, ifTrue, ifFalse)))
    }
    monkeys.addOne(Monkey(currentMonkeyId, startingItems, operation, Test(divisibleBy, ifTrue, ifFalse)))

    monkeys.result()
}

type Id = Int
type Worry = BigInt
type Inspections = Long

case class Monkey(id: Id, items: Seq[Worry], operation: Operation, test: Test)

enum Operation:
    case Multiply(o1: Operand, o2: Operand)
    case Add(o1: Operand, o2: Operand)

enum Operand:
    case Old
    case Literal(by: Int)

case class Test(divisibleBy: Int, ifTrue: Id, ifFalse: Id)

def step(monkey: Monkey, relieve: Worry => Worry): (Map[Id, Seq[Worry]], Monkey) =
    val itemQueue = monkey.items
    if itemQueue.isEmpty then
        (Map.empty, monkey)
    else
        var map = Map.empty[Id, Seq[Worry]]
        for i <- itemQueue.indices do
            val oldWorry = itemQueue(i)
            //operation
            val operated = monkey.operation match
                case Multiply(Old, Old) => oldWorry * oldWorry
                case Multiply(Old, Literal(by)) => oldWorry * by
                case Add(Old, Old) => oldWorry + oldWorry
                case Add(Old, Literal(by)) => oldWorry + by
            //relieve
            val newWorry = relieve(operated)
            //inspect and throw
            if newWorry % monkey.test.divisibleBy == 0 then
                map = map.updatedWith(monkey.test.ifTrue) { case Some(seq) => Some(seq :+ newWorry); case None => Some(Seq(newWorry)) }
            else
                map = map.updatedWith(monkey.test.ifFalse) { case Some(seq) => Some(seq :+ newWorry); case None => Some(Seq(newWorry)) }

        (map, monkey.copy(items = Seq()))

def receive(monkey: Monkey, items: Seq[Worry]): Monkey = monkey.copy(items = monkey.items.appendedAll(items))

def round(monkeys: IndexedSeq[Monkey], acc: Map[Id, Inspections], step: Monkey => (Map[Id, Seq[Worry]], Monkey)): (IndexedSeq[Monkey], Map[Id, Inspections]) =
    var inspections = acc
    var updated = monkeys
    for i <- monkeys.indices do
        val monkey = updated(i)
        inspections = inspections.updatedWith(i) { case Some(count) => Some(count + monkey.items.size); case None => Some(monkey.items.size) }
        val (throws, newMonkey) = step(monkey)
        updated = updated.updated(i, newMonkey)
        for ((to, items) <- throws) do
            val receiver = receive(updated(to), items)
            updated = updated.updated(to, receiver)
    (updated, inspections)

def go(monkeys: IndexedSeq[Monkey], inspections: Map[Id, Inspections], rounds: Int, round: (IndexedSeq[Monkey], Map[Id, Inspections]) => (IndexedSeq[Monkey], Map[Id, Inspections])): (IndexedSeq[Monkey], Map[Id, Inspections]) =
    var i = 0
    var currentMonkeys = monkeys
    var currentInspections = inspections
    while i < rounds do
        val (newMonkeys, newInspections) = round(currentMonkeys, currentInspections)
        currentMonkeys = newMonkeys
        currentInspections = newInspections
        i += 1
    (currentMonkeys, currentInspections)

def monkeyBusiness(inspections: Map[Id, Inspections]): Inspections =
    val List(one: Inspections, two: Inspections) = inspections.values.toList.sorted(using Ordering.Long.reverse).take(2)
    one * two

@main def main: Unit = {

    val result1 = monkeyBusiness(go(input, Map.empty, 20, round(_, _, step(_, _ / 3)))._2)
    println(result1)

    val lcm = input.foldLeft(BigInt(1)) { case (product, monkey) => util.lcm(product, BigInt(monkey.test.divisibleBy)) }
    val result2 = monkeyBusiness(go(input, Map.empty, 10_000, round(_, _, step(_, _ % lcm)))._2)
    println(result2)

}