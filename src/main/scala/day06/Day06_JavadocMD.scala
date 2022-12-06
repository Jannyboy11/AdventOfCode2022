package day06

//optimised solution by JavadocMD: https://gist.github.com/JavadocMD/100e49509c15283390ee124b2638c1c1

import scala.annotation.tailrec

object Day06Loops:
    final def main(args: Array[String]): Unit =
        //val input = Source.fromResource("Day06.input.txt").mkString
        val part1 = findMarker(input, length = 4)
        //println(part1)
        val part2 = findMarker(input, length = 14, position = part1 + 10)
        //println(part2)

    @tailrec
    def findMarker(signal: String, length: Int, position: Int = 0): Int =
        @tailrec
        def findDuplicate(check: Int, cursor: Int, stop: Int): Int =
        // when check advances to stop, conclude no duplicate found
            if check == stop then -1
            // when cursor advances to stop, advance check and reset cursor
            else if cursor == stop then findDuplicate(check + 1, check + 2, stop)
            // if the characters at check and cursor are equal, duplicate found
            // return the index at which to continue the outer search
            else if signal(check) == signal(cursor) then check + 1
            // otherwise, advance cursor
            else findDuplicate(check, cursor + 1, stop)

        if position + length > signal.length then -1 // stop if there aren't enough characters left to make the marker
        else
        // search for duplicates within `length` of our current position
            findDuplicate(position, position + 1, position + length) match
                case -1   => position + length // no duplicate means we've found the marker! success
                case next => findMarker(signal, length, position = next) // otherwise keep looking
    end findMarker

//end of solution