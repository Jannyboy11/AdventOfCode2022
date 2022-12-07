package day07

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

import TerminalOutput.*
import Command.*

val source = Source.fromResource("day07.in")
val input: List[TerminalOutput] = source.getLines().map {
    case s"$$ cd $directory" => Cmd(ChangeDirectory(directory))
    case s"$$ ls" => Cmd(ListFiles)
    case s"dir $directory" => Directory(directory)
    case s"$size $file" => File(size.toInt, file)
}.toList

enum Command:
    case ChangeDirectory(directory: String)
    case ListFiles

enum TerminalOutput:
    case Cmd(cmd: Command)
    case Directory(name: String)
    case File(size: Size, name: String)

type Size = Int

class DirectoryStructure(val dirName: String,
                         val childDirectories: mutable.Map[String, DirectoryStructure],
                         val files: mutable.Map[String, Size],
                         val parent: DirectoryStructure | Null):
    override def toString: String = s"DirectoryStructure($dirName, $childDirectories, $files)"

@tailrec
def buildState(input: List[TerminalOutput], currentDir: DirectoryStructure | Null, rootDir: DirectoryStructure): Unit = input match
    case Cmd(ChangeDirectory("/")) :: t => buildState(t, rootDir, rootDir)
    case Cmd(ChangeDirectory("..")) :: t => buildState(t, currentDir.parent, rootDir)
    case Cmd(ChangeDirectory(dirName)) :: t => buildState(t, currentDir.childDirectories(dirName), rootDir)
    case Cmd(ListFiles) :: t => buildState(t, currentDir, rootDir)
    case File(size, name) :: t =>
        currentDir.files.put(name, size)
        buildState(t, currentDir, rootDir)
    case Directory(name) :: t =>
        currentDir.childDirectories.put(name, DirectoryStructure(name, mutable.Map.empty, mutable.Map.empty, currentDir))
        buildState(t, currentDir, rootDir)
    case Nil => ()

def directorySize(dir: DirectoryStructure): Size =
    dir.files.values.sum + dir.childDirectories.values.map(directorySize).sum

def collectDirectoriesBelowSize(dir: DirectoryStructure, size: Size): List[DirectoryStructure] =
    val mySize = directorySize(dir)
    val children = dir.childDirectories.values.toList.flatMap(collectDirectoriesBelowSize(_, size))
    if mySize < size then dir :: children else children

def collectDirectoriesAboveSize(dir: DirectoryStructure, size: Size): List[DirectoryStructure] =
    val mySize = directorySize(dir)
    val children = dir.childDirectories.values.toList.flatMap(collectDirectoriesAboveSize(_, size))
    if mySize >= size then dir :: children else children

@main def main: Unit = {

    val rootDir = new DirectoryStructure("/", mutable.Map.empty, mutable.Map.empty, null)
    buildState(input, null, rootDir)
    //println(rootDir)

    // Possible optimization: cache sizes using memoization (Map[String, Size])

    val result1 = collectDirectoriesBelowSize(rootDir, 100000).map(directorySize).sum
    println(result1)

    val result2 = {
        val totalUsed: Size = directorySize(rootDir)
        val totalUnused: Size = 70_000_000 - totalUsed
        val required: Size = 30_000_000 - totalUnused
        collectDirectoriesAboveSize(rootDir, required).map(directorySize).min
    }
    println(result2)

}