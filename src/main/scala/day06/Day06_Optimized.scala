package day06

/*
    'a' = 0000_0000_0000_0000_0000_0000_0000_0001
    'b' = 0000_0000_0000_0000_0000_0000_0000_0010
    'c' = 0000_0000_0000_0000_0000_0000_0000_0100
    ...etc
*/
inline def o_bitCode(char: Char): Int =
    1 << (char - 'a')

def o_makeSet(input: String, from: Int, to: Int): Int =
    var i = from;
    var res = 0
    while i < to do
        val newRes = res | o_bitCode(input.charAt(i))
        if newRes == res then
            return newRes
        res = newRes
        i += 1
    res

def o_findMarker(input: String, consecutiveDistinct: Int): Int =
    var set = o_makeSet(input, 0, consecutiveDistinct)

    var i = consecutiveDistinct
    while java.lang.Integer.bitCount(set) != consecutiveDistinct do
        set = o_makeSet(input, i - consecutiveDistinct, i)
        i += 1

    i-1

@main def o_main: Unit = {

    val result1 = o_findMarker(input, 4)
    //println(result1)

    val result2 = o_findMarker(input, 14)
    //println(result2)

}