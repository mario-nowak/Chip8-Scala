package Chip8

import scala.collection.mutable
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

class Chip8 {

  // TODO: Rework this
  private val chip8Fontset: Array[Byte] = Array()

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
  private val randomNumberGenerator = new Random()

  private val displayWidth: Int = 64
  private val displayHeight: Int = 32
  /**
   * The Chip-8 has a black and white screen with a screen size of 64*32.
   */
  private val keys: Array[Byte] = new Array(16)
  private var waitingForKeypress: Boolean = false
  val display: Array[Array[Byte]] = Array.ofDim(displayWidth, displayHeight)
  
  var waitingForKeyPress: Boolean = false
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
  }

  private def incrementProgramCounter(): Unit = {
    programCounter += 2
  }

  private def clearDisplay(): Unit = {
    for (x <- 0 until displayWidth)
      for (y <- 0 until displayHeight)
        display(x)(y) = 0
  }

  def emulateCycle(): Unit = {
    fetchNewOpcode()

    val X = (currentOpcode & 0x0F00) >> 8
    val Y = (currentOpcode & 0x00F0) >> 4
    val N = (currentOpcode & 0x000F).toShort
    val NN = (currentOpcode & 0x00FF).toShort
    val NNN = (currentOpcode & 0x0FFF).toShort
    currentOpcode match {
      case 0x00E0 => // 00E0 - Clear the display by setting all pixels to off
        clearDisplay()
        incrementProgramCounter()
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

              case _ =>
                println(s"WARNING - encountered unsupported opcode $currentOpcode !!!")
            }
          case 0x9000 => // 9XY0 - Skip the next instruction if values of VX and VY are not equal
            if (registerV(X) != registerV(Y))
              incrementProgramCounter()
            incrementProgramCounter()

          case 0xA000 => // ANNN - Set the value of I to the address NNN
            registerI = NNN
            incrementProgramCounter()

          case 0xB000 => // BNNN - Jump to address NNN + V0
            programCounter = NNN + registerV(0x0)

          case 0xC000 => // CXNN - Store the result of a bitwise AND of VX and a random byte
            val randomByte = randomNumberGenerator.nextInt(256)
            registerV(X) = (randomByte & NN).toByte
            incrementProgramCounter()

          case 0xD000 => // DXYN - The draw instruction
            // save the starting position of the sprite we want to draw
            val startXCoordinate = registerV(X) % displayWidth
            val startYCoordinate = registerV(Y) % displayHeight

            registerV(0xF) = 0
            // iterate over all N rows of the sprite
            breakable {
              for (rowIndex <- 0 until N) {
                // fetch the current row
                val spriteRow = memory(registerI + rowIndex)
                // calculate the y coordinate of the current pixel
                val currentYCoordinate = (startYCoordinate + rowIndex) % displayHeight

                // iterate over all 8 columns of the sprite
                breakable {
                  for (columnIndex <- 0 until 8) {
                    // calculate the x coordinate of the current pixel
                    val currentXCoordinate = (startXCoordinate + columnIndex) % displayWidth
                    val currentPixel = display(currentXCoordinate)(currentYCoordinate)

                    val pixelInSprite = spriteRow & (0x1 << (7 - columnIndex))
                    // perform XOR between current color in the display and the bit in the sprite
                    if (pixelInSprite == 1) {
                      if (currentPixel == 1) {
                        display(currentXCoordinate)(currentYCoordinate) = 0
                        registerV(0xF) = 1
                      } else {
                        display(currentXCoordinate)(currentYCoordinate) = 1
                      }
                    }
                    // stop the iteration if we leave the vertical bounds of the screen
                    if (currentXCoordinate == displayWidth -1) break

                  }
                }
                // stop the iteration if we leave the horizontal bounds of the screen
                if (currentYCoordinate == displayHeight -1) break

              }
            }
            incrementProgramCounter()

          case 0xE000 =>
            currentOpcode & 0xF0FF match {
              case 0xE09E => // EX9E - Skip the next instruction if the key with index VX is currently pressed
                if (keys(registerV(X)) == 1)
                  incrementProgramCounter()
                incrementProgramCounter()

              case 0xE0A1 => // EXA1 - Skip the next instruction if the key with index VX is currently NOT pressed
                if (keys(registerV(X)) == 0)
                  incrementProgramCounter()
                incrementProgramCounter()

              case _ =>
                println(s"WARNING - encountered unsupported opcode $currentOpcode !!!")
            }

          case 0xF000 =>
            currentOpcode & 0xF0FF match {
              case 0xF007 => // FX07 - Store the delay timer value into VX
                registerV(X) = delayTimer
                incrementProgramCounter()

              case 0xF00A => // FX0A - Wait for a key press and then store the value of the key to VX
                // TODO: wait for keypress
                // registerV(X) = key
                incrementProgramCounter()

              case 0xF015 => // FX15 - Load the value of VX into the delay timer
                delayTimer = registerV(X)
                incrementProgramCounter()

              case 0xF018 => // FX18 - Load the value of VX into the sound timer
                soundTimer = registerV(X)
                incrementProgramCounter()

              case 0xF01E => // FX1E - Store the sum of I and VX in I
                registerI = (registerI + registerV(X)).toShort
                incrementProgramCounter()

              case 0xF029 => // FX29 - Set the location of the sprite for the digit VX to I
                /* The font sprites start at address 0x000 and contain the hexadecimal digits from 1 to F.
                 * Each character has a height of 0x05 bytes. To obtain the starting address of a character, we need
                 * to multiply its value times the offset of 5 rows for the previous characters.
                 */
                registerI = (registerV(X) * 0x05).toShort
              case 0xF033 => // FX33 - Store the binary decoded decimal VX into three consecutive memory slots I...I+2
                val hundreds: Byte = (registerV(X) / 100).toByte
                val tens: Byte = ((registerV(X) - hundreds * 100) / 10).toByte
                val ones: Byte = (registerV(X) - hundreds * 100 - tens * 10).toByte
                memory(registerI) = hundreds
                memory(registerI+1) = tens
                memory(registerI+2) = ones
                incrementProgramCounter()

              case 0xF055 => // FX55 - Store registers from V0 to VX in the main memory, starting at location I
                for (registerIndex <- 0 until X)
                  memory(registerI + registerIndex) = registerV(registerIndex)
                incrementProgramCounter()

              case 0xF065 => // FX65 - Load the memory data starting at address I into the registers V0 to VX
                for (registerIndex <- 0 until X)
                  registerV(registerIndex) = memory(registerI + registerIndex)
                incrementProgramCounter()

              case _ =>
                println(s"WARNING - encountered unsupported opcode $currentOpcode !!!")
            }
          case _ =>
            println(s"WARNING - encountered unsupported opcode $currentOpcode !!!")
        }
    }
  }

  private def fetchNewOpcode(): Unit = {
    currentOpcode = (memory(programCounter) << 8 | memory(programCounter + 1)).toShort
  }

  def waitForKeyPress(): Unit = {
    waitingForKeypress = true
    while (waitingForKeyPress) { /* TODO: Remove this ugly busy wait */ }
  }
  
  def registerKeyPress(keyIndex: Int): Unit = {
    keys(keyIndex) = 1
  }

  def registerKeyRelease(keyIndex: Int): Unit = {
    keys(keyIndex) = 0
  }

  initialize()

}