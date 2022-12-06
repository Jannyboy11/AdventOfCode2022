package day06

//optimised solution by Bishabosha: https://gist.github.com/bishabosha/24ce043dddb2fed2d26658b94962144c

def part1(input: String): Int =
  findIndexOptimal(input, n = 4)

def part2(input: String): Int =
  findIndexOptimal(input, n = 14)

class MultiSet:
  private val counts = new Array[Int](128)
  private var total = 0
  def size = total

  def add(c: Char) =
    val count = counts(c.toInt)
    if count == 0 then
      total += 1
    counts(c.toInt) += 1

  def remove(c: Char) =
    val count = counts(c.toInt)
    if count > 0 then
      if count == 1 then
        total -= 1
      counts(c.toInt) -= 1
end MultiSet

def findIndexOptimal(input: String, n: Int): Int =
  val counts = MultiSet()
  def loop(previous: Char, i: Int, j: Int): Int =
    if i < input.length then
      val next = input(i)
      val last = input(j)
      if previous != last then
        counts.remove(previous)
        counts.add(last)
      if counts.size == n then
        i + n // found the index
      else
        loop(next, i + 1, j + 1)
    else
      -1
  input.view.take(n).foreach(counts.add)
  loop(input(0), i = 1, j = n)

//end of solution

@main def bb_main: Unit =
  val result1 = part1(input)
  println(result1)
  val result2 = part2(input)
  println(result2)