package Chip8

import scala.collection.mutable
import scala.collection.mutable.Stack

class Chip8 {

  // TODO: Rework this
  private val chip8Fontset: Array[Byte] = new Array(80)

  private var currentOpcode: Short = 0

  private val memory: Array[Byte] = new Array(4069)

  /**
   * Chip-8 has 16 8-bit data registers named V0 to VF.
   */
  private val registerV: Array[Byte] = new Array(16)

  /**
   * The address register I is 12 bits (here 16).
   */
  private var registerI: Short = 0

  private var programCounter: Int = 0

  private var delayTimer: Byte = 0
  private var soundTimer: Byte = 0

  private val stack: mutable.Stack[Int] = new mutable.Stack()
  private var stackPointer: Short = 0

  /**
   * The Chip-8 has a black and white screen with a screen size of 64*32.
   */
  var graphics: Array[Array[Byte]] = Array.ofDim(64, 32)
  var pressedKey: Array[Byte] = new Array(16)

  var drawFlag: Boolean = false


  private def loadFontset(): Unit = {
    for (i <- 0 until 80){
      memory(i) = chip8Fontset(i)
    }
  }

  private def initialize(): Unit = {
    // initialize registers and memory once
    programCounter = 0x200 // Program counter starts at 0x200
    currentOpcode = 0
    registerI = 0
    stackPointer = 0

    // clear display
    // clear stack
    // clear registers V0 - VF
    // clear memory

    loadFontset()

    println("Hello!")

  }

  private def incrementProgramCounter(): Unit = {
    programCounter += 2
  }

  private def emulateCycle(): Unit = {
    // Fetch Opcode
    fetchNewOpcode()

    // Decode Opcode


    // Execute Opcode
    val X = (currentOpcode & 0x0F00) >> 8
    val Y = (currentOpcode & 0x00F0) >> 4
    val NN = (currentOpcode & 0x00FF).toShort
    val NNN = (currentOpcode & 0x0FFF).toShort
    currentOpcode match {
      case 0x00E0 => // 00E0 - Clear the display by setting all pixels to off
        // TODO
      case 0x00EE => // 00EE - Return from a subroutine
        programCounter = stack.pop()
        incrementProgramCounter()
      case _ =>
        currentOpcode & 0xF000 match {
          case 0x1000 => // 1NNN - Jump to address NNN
            programCounter = NNN
          case 0x2000 => // 2NNN - Call the subroutine at address NNN
            stack.push(programCounter)
            programCounter = NNN
          case 0x3000 => // 3XNN - Skip the next instruction if VX == NN
            if (registerV(X) == (currentOpcode & 0x00FF))
              incrementProgramCounter()
            incrementProgramCounter()
          case 0x4000 => // 4XNN - Skip the next instruction if VX != NN
            if (registerV(X) != (currentOpcode & 0x00FF))
              incrementProgramCounter()
            incrementProgramCounter()
          case 0x5000 => // 5XY0 - Skip next instruction if VX == VY
            if (registerV(X) == registerV(Y))
              incrementProgramCounter()
            incrementProgramCounter()
          case 0x6000 => // 6XNN - Load value NN into register VX
            registerV(X) = (currentOpcode & 0x00FF).toByte
            incrementProgramCounter()
          case 0x7000 => // 7XNN - Add value NN to the value of register VX
            registerV(X) = (registerV(X) + NN).toByte
            incrementProgramCounter()
          case 0x8000 =>
            currentOpcode & 0xF00F match {
              case 0x8000 => // 8XY0 - Put value of register VY into VX
                registerV(X) = registerV(Y)
                incrementProgramCounter()
              case 0x8001 => // 8XY1 - Store bitwise OR between values VX and VY in VX
                registerV(X) = (registerV(X) | registerV(Y)).toByte
                incrementProgramCounter()
              case 0x8002 => // 8XY2 - Store bitwise AND between values VX and VY in VX
                registerV(X) = (registerV(X) & registerV(Y)).toByte
                incrementProgramCounter()
              case 0x8003 => // 8XY3 - Perform bitwise XOE between values VX and VY in VX
                registerV(X) = (registerV(X) ^ registerV(Y)).toByte
                incrementProgramCounter()
              case 0x8004 => // 8XY4 - Store sum of VY and VX in VX. Put the carry bit in VF
                registerV(0xF) = if (registerV(X) + registerV(Y) > 0xFF) 1 else 0
                registerV(X) = (registerV(X) + registerV(Y)).toByte
                incrementProgramCounter()
              case 0x8005 => // 8XY5 - Store subtraction of VX and VY in VX. Put the borrow in VF
                registerV(0xF) = if (registerV(X) > registerV(Y)) 1 else 0
                registerV(X) = (registerV(X) - registerV(Y)).toByte
                incrementProgramCounter()
              case 0x8006 => // 8XY6 - Store lest significant bit of VX in VF and shift VX right
                registerV(0xF) = (registerV(X) & 0x01).toByte
                registerV(X) = (registerV(X) / 2).toByte
                incrementProgramCounter()
              case 0x8007 => // 8XY7 - Store result of subtraction of VY and VX in VX. Set VF to 1 if there is no borrow, to 0 otherwise.
                registerV(0xF) = if (registerV(Y) > registerV(X)) 1 else 0
                registerV(X) = (registerV(Y) - registerV(X)).toByte
                incrementProgramCounter()
              case 0x800E => // 8XYE - Store most significant bit of VX in VF and shift VX left
                registerV(0xF) = (registerV(X) & 0x80).toByte
                registerV(X) = (registerV(X) * 2).toByte
                incrementProgramCounter()
            }
          case 0x9000 => // 9XY0 - Skip the next instruction if values of VX and VY are not equal
            if (registerV(X) != registerV(Y))
              incrementProgramCounter()
            incrementProgramCounter()
          case _ =>
        }
    }

  }

  private def fetchNewOpcode(): Unit = {
    currentOpcode = (memory(programCounter) << 8 | memory(programCounter + 1)).toShort
  }


  initialize()

}