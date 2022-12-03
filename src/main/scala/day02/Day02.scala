package day02

import scala.io.Source;

val source = Source.fromResource("day02.in")

enum RPS:
    case Rock, Paper, Scissors
enum Outcome:
    case Win, Lose, Draw
import RPS.*
import Outcome.*

enum Them(rps: RPS):
    case A extends Them(Rock)
    case B extends Them(Paper)
    case C extends Them(Scissors)
    def token: RPS = rps

enum Us(rps: RPS, oc: Outcome):
    case X extends Us(Rock, Lose)
    case Y extends Us(Paper, Draw)
    case Z extends Us(Scissors, Win)
    def token: RPS = rps
    def outcome: Outcome = oc

type Instruction = (Them, Us)

val input: List[Instruction] = source.getLines().map { case s"${them} ${us}" => (Them.valueOf(them), Us.valueOf(us))}.toList
val checkArray: Array[RPS] = RPS.values ++ RPS.values

def outcomeScore(them: RPS, us: RPS): Int = (us.ordinal - them.ordinal + 3) % 3 match
    case 2 => 0
    case 1 => 6
    case 0 => 3

def usScore(us: RPS): Int = us.ordinal + 1

def score(them: RPS, us: RPS): Int =  outcomeScore(them, us) + usScore(us)

def calculateToken(them: RPS, outcome: Outcome): RPS =
    val theirIdx = them.ordinal
    outcome match
        case Win => checkArray(theirIdx + 1)
        case Lose => checkArray(theirIdx + 2)
        case Draw => checkArray(theirIdx)

@main def main(): Unit = {

    val result1 = input.map { case (them, us) => score(them.token, us.token) }.sum
    println(result1)

    val result2 = input.map { case (them, us) => (them.token, calculateToken(them.token, us.outcome)) }.map(score.tupled).sum
    println(result2)

}