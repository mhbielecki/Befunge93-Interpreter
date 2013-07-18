import scala.io.Source
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.util.Random

/**
  Befunge (93) interpreter in scala
  @author martinhb
*/

object BefungeInterpreter {
  def main(args: Array[String]) = {

  if (args.length == 0) {
    println("Error: no filename given")
    System exit 1
  }

  val LINE_LENGTH = 80
    try {
      val befungeProgram = new ArrayBuffer[Array[Char]]()
      for (line <- Source.fromFile(args(0)).getLines()) {
        val extendedLine = line.padTo(LINE_LENGTH, " ").mkString
        befungeProgram += extendedLine.toCharArray
      }
      new Interpreter(befungeProgram).interpret()
    }
    catch {
      case e : java.io.FileNotFoundException => println("Error: file not found")
    }
  }
}

class Interpreter(val befungeProgram: ArrayBuffer[Array[Char]]) {

  val LINE_LENGTH = 80
  var xPos = 0
  var yPos = 0
  var xDir = 1
  var yDir = 0
  var stack = new Stack[Long]

  private def pop() = if (stack.size == 0) 0 else stack pop

  private def top() = if (stack.size == 0) 0 else stack top

  private def moveProgramCounter() {

    if (xDir == -1 && xPos == 0)
      xPos = LINE_LENGTH-1
    else if (xDir == 1 && xPos == LINE_LENGTH-1)
      xPos = 0
    else
      xPos += xDir

    if (yDir == -1 && yPos == 0)
      yPos=befungeProgram.length-1
    else if (yDir == 1 && yPos == befungeProgram.length-1)
      yPos = 0
    else
      yPos += yDir
  }

  private def plusExp() {
      stack.push(pop + pop)
  }

  private def minusExp() {
    val a = pop
    val b = pop
    stack.push(b-a)
  }

  def multExp() {
    stack.push(pop * pop)
  }

  private def divExp() {
    try {
      val a = pop
      val b = pop
      stack.push(b / a)
    }
    catch {
      case e : java.lang.ArithmeticException => println("Error: Divided by zero")
      System.exit(1)
    }
  }

  private def modExp() {
    try {
      val a = pop
      val b = pop
      stack.push(b % a)
    }
    catch {
      case e : java.lang.ArithmeticException => println("Error: Divided by zero")
      System.exit(1)
    }
  }

  private def logicalNotExp() {
    if (pop == 0) stack push 1 else stack push 0
  }

  private def greaterThanExp() {
    val a = pop
    val b = pop

    if (b > a) stack push 1 else stack push 0
  }

  private def setRandomDirection() {

    new Random nextInt 4 match {
      case 0 => xDir = 1; yDir = 0  // right
      case 1 => xDir = -1; yDir = 0 // left
      case 2 => xDir = 0; yDir = -1 // up
      case 3 => xDir = 0; yDir = 1  // down
    }
  }

  private def horizontalIf() {
    val a = pop

    if (a == 0) {
      xDir = 1; yDir = 0
    } else {
      xDir = -1; yDir = 0
    }
  }

  private def verticalIf() {
    val a = pop

    if (a == 0) {
      xDir = 0; yDir = 1
    } else {
      xDir = 0; yDir = -1
    }
  }

  private def swapStackValues() {
    val a = pop
    val b = pop

    stack push a
    stack push b
  }

  private def putCall() {
    val y = pop
    val x = pop
    val v = pop

    befungeProgram(x.asInstanceOf[Int])(y.asInstanceOf[Int]) = v toChar
  }

  private def getCall() {
    val y = pop
    val x = pop

    stack push befungeProgram(x.asInstanceOf[Int])(y.asInstanceOf[Int])
  }

  private def inputInt() {
    println("Input integer")
    stack push readInt
  }

  private def inputChar() {
    println("Input character")
    stack push readChar
  }

  def interpret() {
    var currentInstruction = ' '
    var stringMode = false
    var skipNextInstr = false
    var steps = 0
    val vals = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

    while ({currentInstruction = befungeProgram(yPos)(xPos); currentInstruction != '@'}) {

      steps += 1

      if (skipNextInstr) {
        skipNextInstr = !skipNextInstr
      }
      else if (!stringMode) {
        // Not in string mode, so treat a char as an instruction
        currentInstruction match {
          case '+' => plusExp
          case '-' => minusExp
          case '*' => multExp
          case '/' => divExp
          case '%' => modExp
          case '!' => logicalNotExp
          case '`' => greaterThanExp
          case '>' => xDir = 1; yDir = 0
          case '<' => xDir = -1; yDir = 0
          case '^' => xDir = 0; yDir = -1
          case 'v' => xDir = 0; yDir = 1
          case '?' => setRandomDirection
          case '_' => horizontalIf
          case '|' => verticalIf
          case '"' => stringMode = !stringMode
          case ':' => stack push(top) //duplicate top stack value
          case '\\' => swapStackValues
          case '$' => stack pop // pop and discard value
          case '.' => print(pop)
          case ',' => print(pop().toChar)
          case '#' => skipNextInstr = !skipNextInstr
          case 'g' => getCall
          case 'p' => putCall
          case '&' => inputInt
          case '~' => inputChar
          case num if vals contains num => stack push num-48
          case _ => ;
        }
      }
      else {
        // in stringmode,push each characters ascii value on stack until we reach "-char
        // or program-end
        if (currentInstruction == '"')
          stringMode = !stringMode
        else
          stack push currentInstruction
      }
      moveProgramCounter
    }
  }
}
